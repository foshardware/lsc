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
pnr netlist@(NetGraph ident abstract _ gates nets) = do

  debug
    [ "start pnr @ module", unpack ident
    , "-", show $ length gates, "gates"
    , "-", show $ length nets, "nets"
    ]

  liftSMT $ do
    setOption $ ProduceUnsatCores True

  nodes <- sequence $ freeGatePolygon <$> gates

  (area, pins) <- placement nodes abstract

  edges <- sequence $ arboresence nodes pins <$> nets

  collision nodes

  result <- checkResult nodes edges

  debug ["stop  pnr @ module", unpack ident]

  pure netlist
    { gateVector = maybe (gateVector netlist) fst result
    , netMapping = maybe (netMapping netlist) snd result
    }


placement nodes (AbstractGate _ pins) = do

  area <- freeRectangle

  abstract <- sequence
    [ freePinPolygon area pin
    | pin <- pins
    , pinDir pin == In
    ]

  liftSMT $ constrain
    $   bottom area .== foldr1 smin (bottom . snd <$> nodes)
    .&& right  area .== foldr1 smax (right  . snd <$> nodes)
    .&& top    area .== foldr1 smax (top    . snd <$> nodes)

  liftSMT $ constrain
    $   left area
        .== foldr1 smin (left . snd <$> abstract)

    .&& foldr1 smax (right . snd <$> abstract)
        .<= foldr1 smin (left . snd <$> nodes)

  let w = sum $ width  . snd <$> nodes
      h = sum $ height . snd <$> nodes

  liftSMT $ constrain
    $   left   area .== literal 0
    .&& right  area .<= w
    .&& bottom area .== literal 0
    .&& top    area .<= h

  pure (area, abstract)


collision nodes = do
  sequence_
    [ do

      liftSMT $ do

        constrain
          $   left b .>= right a
          .|| left a .>= right b
          .|| bottom a .>= top b
          .|| bottom b .>= top a

        softConstrain
          $   left b .== right a
          .|| left a .== right b
          .|| bottom a .== top b
          .|| bottom b .== top a

    | ((_, a), (_, b)) <- distinctPairs $ toList nodes
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


freePinPolygon area pin = do

  path <- freeRectangle

  std <- standardPin <$> ask
  liftSMT $ constrain
    $   left path .>= left area
    .&& right path - left path .== literal (width  std)
    .&& top path - bottom path .== literal (height std)

  pure (pin, path)


overlap a b = do
  liftSMT $ constrain
    $   left a   .== left b
    .&& bottom a .== bottom b
    .&& right a  .== right b
    .&& top a    .== top b


pinConnect a b = do
  liftSMT $ constrain
    $   left a   .== left b   .&& bottom a .== bottom b .&& right a .== right b
    .|| left a   .== left b   .&& bottom a .== bottom b .&& top a   .== top b
    .|| left a   .== left b   .&& right a  .== right b  .&& top a   .== top b
    .|| bottom a .== bottom b .&& right a  .== right b  .&& top a   .== top b


pathCombine a b = do
  liftSMT $ constrain
    $   (right a .== left b .|| right b .== left a) .&& (top a .== top b .|| bottom a .== bottom b)
    .|| (bottom a .== top b .|| bottom b .== top a) .&& (left a .== left b .|| right a .== right b)


arboresence nodes pins net = do

  sources <- sequence
    [ do
      pure (gate, source)
    | (gate, assignments) <- assocs $ contacts net
    , source <- assignments
    , pinDir source == Out
    ]

  hyperedge <- sequence
    [ do

      start  <- freeRectangle
      target <- freeRectangle

      pinConnect start $ Rect
        (left src + literal l, bottom src + literal b)
        (left src + literal r, bottom src + literal t)

      pinConnect target $ Rect
        (left snk + literal m, bottom snk + literal c)
        (left snk + literal s, bottom snk + literal u)

      pathCombine start target

      pure [start, target]

    | (j, source) <- sources
    , (i, assignments) <- assocs $ contacts net
    , sink <- assignments
    , let (_, snk) = nodes ! gateIndex i
    , let (_, src) = nodes ! gateIndex j
    , pinDir sink == In
    , Rect (l, b) (r, t) <- take 1 $ portRects $ pinPort source
    , Rect (m, c) (s, u) <- take 1 $ portRects $ pinPort sink
    ]

  pure (net, join hyperedge)


freeRectangle = Rect <$> freePoint <*> freePoint

freePolygon n = sequence $ replicate n freePoint


freePoint = liftSMT $ (,) <$> free_ <*> free_


checkResult nodes edges = do

  liftSMT $ query $ do
    result <- checkSat
    case result of

      Sat -> do

        gates <- sequence (gateAssign <$> nodes)
        nets  <- sequence (netAssign  <$> edges)

        pure $ Just (gates, nets)

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

    netAssign (net, edge) = sequence (rectangle <$> edge)
      >>= \ paths -> pure net { netPaths = pure paths }

    rectangle (Rect (left, bottom) (right, top)) = Rect
      <$> ((, ) <$> getValue left  <*> getValue bottom)
      <*> ((, ) <$> getValue right <*> getValue top)
