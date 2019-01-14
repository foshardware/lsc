{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSC.Arboresence where

import Control.Monad
import Control.Monad.Trans
import Data.Foldable
import Data.Map (assocs)
import Data.Vector ((!))
import Data.SBV
import Data.SBV.Control
import Data.Text (unpack)
import System.IO

import LSC.Types


pnr :: NetGraph -> LSC NetGraph
pnr netlist@(NetGraph ident (AbstractGate _ _ contacts) _ gates nets) = do

  debug
    [ "start pnr @ module", unpack ident
    , "-", show $ length gates, "gates"
    , "-", show $ length nets, "nets"
    ]

  liftSMT $ do
    setOption $ ProduceUnsatCores True


  nodes <- sequence $ freeGatePolygon <$> gates

  area <- placement nodes

  pins <- sequence $ pinPolygon area <$> contacts

  ring <- powerRing nodes

  edges <- sequence $ arboresence nodes pins <$> nets

  alignPins ring pins

  disjointGates nodes
  disjointNets edges

  result <- checkResult area pins ring nodes edges

  debug ["stop  pnr @ module", unpack ident]

  pure netlist
    { modelGate  = maybe (modelGate  netlist) snd result
    , gateVector = maybe (gateVector netlist) (fst . fst) result
    , netMapping = maybe (netMapping netlist) (snd . fst) result
    }


placement nodes = do

  area <- freeRectangle

  liftSMT $ constrain
    $   left   area .== literal 0
    .&& right  area .<= literal 4 * sum (width  . snd <$> nodes)
    .&& bottom area .== literal 0
    .&& top    area .<= literal 4 * sum (height . snd <$> nodes)

  pure area


alignPins ring pins = do

  sequence_ $ [ rect `alignLeft` outer ring | (p, rect) <- pins, pinDir p ==  In ]
  sequence_ $ [ outer ring `alignLeft` rect | (p, rect) <- pins, pinDir p == Out ]

  sequence_
    [ disjoint a b
    | ((_, a), (_, b)) <- distinctPairs $ toList pins
    ]


disjointGates nodes = do
  sequence_
    [ disjoint a b
    | ((_, a), (_, b)) <- distinctPairs $ toList nodes
    ]


disjointNets edges = do
  sequence_
    [ disjoint a b
    | ((_, as), (_, bs)) <- distinctPairs $ toList edges
    , a <- as
    , b <- bs
    ]


distinctPairs :: [a] -> [(a, a)]
distinctPairs [] = []
distinctPairs (x : xs) = fmap (x, ) xs ++ distinctPairs xs


freeGatePolygon gate = do

  path <- freeRectangle

  dimensions <- lookupDimensions gate <$> ask
  for_ dimensions $ \ (w, h) -> liftSMT $ constrain
    $   right path - left path .== literal w
    .&& top path - bottom path .== literal h

  pure (gate, path)


pinPolygon area pin = do

  path <- freeRectangle

  (w, h) <- standardPin <$> ask

  liftSMT $ constrain
    $    width path .== literal w
    .&& height path .== literal h

  if pinDir pin == In
    then liftSMT $ constrain $  left path .==  left area
    else liftSMT $ constrain $ right path .== right area

  liftSMT $ constrain
    $   bottom path .>= bottom area
    .&&    top path .<=    top area

  pure (pin, path)


alignLeft l r = do
  d <- lambda <$> ask
  liftSMT $ constrain $ left r - right l .> literal d

alignRight = flip alignLeft


inside i o = do
  d <- lambda <$> ask
  liftSMT $ constrain
    $     left o -   left i .> literal d
    .&& bottom o - bottom i .> literal d
    .&&  right o -  right i .> literal d
    .&&    top o -    top i .> literal d


outside = disjoint

disjoint a b = do
  d <- lambda <$> ask
  liftSMT $ constrain
    $   left b - right a .> literal d
    .|| left a - right b .> literal d
    .|| bottom a - top b .> literal d
    .|| bottom b - top a .> literal d


pinConnect a b = do
  liftSMT $ constrain
    $   left a   .== left b   .&& bottom a .== bottom b .&& right a .== right b
    .|| left a   .== left b   .&& bottom a .== bottom b .&& top a   .== top b
    .|| left a   .== left b   .&& right a  .== right b  .&& top a   .== top b
    .|| bottom a .== bottom b .&& right a  .== right b  .&& top a   .== top b


pathCombine a b = do
  liftSMT $ constrain
    $   right a .== right b .&& top a .== top b
    .|| left  a .== left  b .&& top a .== top b
    .|| right a .== right b .&& bottom a .== bottom b
    .|| left  a .== left  b .&& bottom a .== bottom b


arboresence nodes pins net = do

  let sources = vertices Out
      sinks   = vertices In

  hyperedge <- sequence
    [ do

      start  <- freeRectangle
      target <- freeRectangle

      pinConnect start source
      pinConnect target sink

      pathCombine start target

      pure [start, target]

    | source <- sources
    , sink   <- sinks
    ]

  pure (net, join hyperedge)

  where

    vertices dir =
      [ Rect
          (left src + literal l, bottom src + literal b)
          (left src + literal r, bottom src + literal t)
      | (j, assignments) <- assocs $ netPins net
      , source <- assignments
      , pinDir source == dir
      , let (gate, src) = nodes ! gateIndex j
      , Rect (l, b) (r, t) <- take 1 $ portRects $ pinPort source
      ] ++
      [ rect
      | (pin, rect) <- pins
      , pinDir pin /= dir
      , pinIdent pin == netIdent net
      ]


powerRing nodes = do
  
  ring <- freeRing

  (w, h) <- standardPin <$> ask

  sequence_ $ (`inside` inner ring) . snd <$> nodes

  liftSMT $ constrain
    $   width (left ring) .== width (bottom ring)
    .&& width (bottom ring) .== width (right ring)
    .&& width (right ring) .== width (top ring)
    .&& width (top ring) .== literal w

    .&& height (left ring) .== height (bottom ring)
    .&& height (bottom ring) .== height (right ring)
    .&& height (right ring) .== height (top ring)
    .&& height (top ring) .== literal h

  pure ring


freeRing = do

  l <- freeRectangle
  b <- freeRectangle
  r <- freeRectangle
  t <- freeRectangle

  liftSMT $ constrain
    $    left l .==  left b .&& bottom b .== bottom l
    .&&  left l .==  left t .&&    top t .==    top l
    .&& right r .== right b .&& bottom b .== bottom r
    .&& right r .== right t .&&    top t .==    top r

  pure $ Rect (l, b) (r, t)


freeRectangle = do

  area <- Rect <$> freePoint <*> freePoint

  liftSMT $ constrain
    $   width  area .>= 0
    .&& height area .>= 0
    .&&   left area .>= 0
    .&& bottom area .>= 0

  pure area


freePolygon n = sequence $ replicate n freePoint


freePoint = liftSMT $ (,) <$> free_ <*> free_


checkResult area pins ring nodes edges = do

  liftSMT $ query $ do
    result <- checkSat
    case result of

      Sat -> do

        pad <- rectangle area
        power <- sequence $ rectangle <$> toList ring
        inpts <- sequence (pinAssign  <$> pins)

        gates <- sequence (gateAssign <$> nodes)
        nets  <- sequence (netAssign  <$> edges)

        pure $ Just ((gates, nets), AbstractGate [pad] power inpts)

      Unsat -> do

        unsat <- getUnsatCore
        liftIO $ sequence_ $ hPutStrLn stderr <$> unsat

        pure Nothing

      _ -> do

        reason <- getUnknownReason
        liftIO $ hPutStrLn stderr $ show reason

        pure Nothing

  where

    gateAssign (gate, xs) = rectangle xs
      >>= \ path -> pure gate { gatePath = pure path }

    pinAssign (pin, xs) = rectangle xs
      >>= \ path -> pure pin { pinPort = Port mempty [path] }

    netAssign (net, edge) = sequence (rectangle <$> edge)
      >>= \ paths -> pure net { netPaths = pure paths }

    rectangle r = Rect
      <$> ((, ) <$> getValue (left r)  <*> getValue (bottom r))
      <*> ((, ) <$> getValue (right r) <*> getValue (top r))
