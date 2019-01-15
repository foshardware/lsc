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

  pins <- sequence $ freePinPolygon <$> contacts

  ring <- powerRing nodes

  area <- placement nodes ring pins

  edges <- sequence $ arboresence 4 nodes pins <$> nets

  disjointGates nodes
  disjointNets edges

  result <- checkResult area pins ring nodes edges

  debug ["stop  pnr @ module", unpack ident]

  pure netlist
    { modelGate  = maybe (modelGate  netlist) snd result
    , gateVector = maybe (gateVector netlist) (fst . fst) result
    , netMapping = maybe (netMapping netlist) (snd . fst) result
    }


placement nodes ring pins = do

  area <- freeRectangle

  liftSMT $ constrain
    $     left area .== literal 0
    .&& bottom area .== literal 0

  outer ring `inside` area

  d <- literal . lambda <$> ask
  sequence_
    [ liftSMT $ constrain
        $   left path .== left area
        .&& left (outer ring) - left path .> d
    | (p, path) <- pins
    , pinDir p == In
    ]
  sequence_
    [ liftSMT $ constrain
        $   right path .== right area
        .&& right path - right (outer ring) .> d
    | (p, path) <- pins
    , pinDir p == Out
    ]
  sequence_
    [ liftSMT $ constrain
        $   bottom path .>= bottom (inner ring)
        .&&    top path .<=    top (inner ring)
    | (p, path) <- pins
    ]

  sequence_
    [ disjoint a b
    | ((_, a), (_, b)) <- distinctPairs $ toList pins
    ]

  pure area


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


freeWirePolygon net = do

  path <- freeRectangle

  pure (net, path)


freePinPolygon pin = do

  path <- freeRectangle

  (w, h) <- standardPin <$> ask

  liftSMT $ constrain
    $    width path .== literal w
    .&& height path .== literal h

  pure (pin, path)


inside i o = do
  d <- lambda <$> ask
  liftSMT $ constrain
    $     left i -   left o .> literal d
    .&& bottom i - bottom o .> literal d
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


connect a b = do

  liftSMT $ constrain
    $   right a .== right b .&&    top a .==    top b
    .||  left a .==  left b .&&    top a .==    top b
    .|| right a .== right b .&& bottom a .== bottom b
    .||  left a .==  left b .&& bottom a .== bottom b

  liftSMT $ softConstrain
    $     left a .==   left b .&& bottom a .== bottom b .&& right a .== right b
    .||   left a .==   left b .&& bottom a .== bottom b .&&   top a .==   top b
    .||   left a .==   left b .&&  right a .==  right b .&&   top a .==   top b
    .|| bottom a .== bottom b .&&  right a .==  right b .&&   top a .==   top b


arboresence n nodes pins net = do

  let sources = vertices Out
      sinks   = vertices In

  hyperedge <- sequence
    [ do

      src <- snd <$> freeWirePolygon net
      snk <- snd <$> freeWirePolygon net

      jogs <- sequence $ replicate n $ snd <$> freeWirePolygon net

      let wire = [src] ++ jogs ++ [snk]

      connect src source
      connect snk sink

      (w, h) <- standardPin <$> ask
      sequence_
        [ liftSMT $ constrain
            $    width path .== literal w
            .|| height path .== literal h
        | path <- jogs
        ]

      sequence_ [ connect i j | i <- wire | j <- drop 1 wire ]

      pure wire

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
    $   width (left ring) .==  width (right ring)
    .&& width (right ring) .== literal w

    .&& height (bottom ring) .== height (top ring)
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
      >>= \ path -> pure pin { pinPort = Port Metal1 [path] }

    netAssign (net, edge) = sequence (rectangle <$> edge)
      >>= \ paths -> pure net { netPaths = pure paths }

    rectangle r = Rect
      <$> ((, ) <$> getValue (left r)  <*> getValue (bottom r))
      <*> ((, ) <$> getValue (right r) <*> getValue (top r))
