{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSC.Arboresence where

import Control.Monad
import Control.Monad.Trans
import Data.Default
import Data.Foldable
import Data.Map (assocs)
import Data.Vector ((!))
import qualified Data.Vector as Vector
import Data.SBV
import Data.SBV.Control
import Data.Text (unpack)
import System.IO

import LSC.Types


pnr :: NetGraph -> LSC NetGraph
pnr netlist@(NetGraph name pins _ gates nets) = do

  debug
    [ "start pnr @ module", unpack name
    , "-", show $ length gates, "gates"
    , "-", show $ length nets, "nets"
    ]

  liftSMT $ do
    setOption $ ProduceUnsatCores True

  nodes <- sequence $ freeGatePolygon <$> gates

  edges <- sequence $ arboresence nodes <$> nets

  placement nodes pins
  collision nodes

  result <- checkResult nodes edges

  debug ["stop  pnr @ module", unpack name]

  pure netlist
    { gateVector = maybe (gateVector netlist) fst result
    , netMapping = maybe (netMapping netlist) snd result
    }


placement nodes _ | length nodes < 1 = pure ()
placement nodes (inputs, outputs, _) = do

  let inner = snd <$> toList nodes

  let left   = foldr1 smin [ x | Rect (x, _) _ <- inner ]
  let bottom = foldr1 smin [ x | Rect (_, x) _ <- inner ]
  let right  = foldr1 smax [ x | Rect _ (x, _) <- inner ]
  let top    = foldr1 smax [ x | Rect _ (_, x) <- inner ]

  let tie = floor $ sqrt $ fromIntegral $ length nodes :: Integer

  let width  = sum [ right - left | Rect (left, _) (right, _) <- inner ]
      height = sum [ top - bottom | Rect (_, bottom) (_, top) <- inner ]

  liftSMT $ constrain
    $   left   .== literal 0
    .&& right  .<= width
    .&& bottom .== literal 0
    .&& top    .<= height

  sequence_
    [ liftSMT $ constrain $ x .== right
    | (g, Rect _ (x, _)) <- toList nodes
    , any (`elem` outputs) $ gateWires g
    ]


collision nodes = do
  sequence_
    [ do

      liftSMT $ do

        constrain
          $   left2 .>= right1
          .|| left1 .>= right2
          .|| bottom1 .>= top2
          .|| bottom2 .>= top1

        softConstrain
          $   left2 .== right1
          .|| left1 .== right2
          .|| bottom1 .== top2
          .|| bottom2 .== top1

    | ((_, path1), (_, path2)) <- distinctPairs $ toList nodes
    , let Rect (left1, bottom1) (right1, top1) = path1
    , let Rect (left2, bottom2) (right2, top2) = path2
    ]


distinctPairs :: [a] -> [(a, a)]
distinctPairs [] = []
distinctPairs (x : xs) = fmap (x, ) xs ++ distinctPairs xs


freeGatePolygon gate = do

  path @ (Rect (left, bottom) (right, top)) <- freeRectangle

  dimensions <- lookupDimensions gate <$> ask
  for_ dimensions $ \ (width, height) -> liftSMT $ constrain
    $   right - left .== literal width
    .&& top - bottom .== literal height

  pure (gate, path)


overlap
  (Rect (left1, bottom1) (right1, top1))
  (Rect (left2, bottom2) (right2, top2))
  = do
    liftSMT $ constrain
      $   left1   .== left2
      .&& bottom1 .== bottom2
      .&& right1  .== right2
      .&& top1    .== top2


pinConnect
  (Rect (left1, bottom1) (right1, top1))
  (Rect (left2, bottom2) (right2, top2))
  = do
    liftSMT $ constrain
      $   left1   .== left2   .&& bottom1 .== bottom2 .&& right1 .== right2
      .|| left1   .== left2   .&& bottom1 .== bottom2 .&& top1   .== top2
      .|| left1   .== left2   .&& right1  .== right2  .&& top1   .== top2
      .|| bottom1 .== bottom2 .&& right1  .== right2  .&& top1   .== top2


pathCombine
  (Rect (left1, bottom1) (right1, top1))
  (Rect (left2, bottom2) (right2, top2))
  = do
    liftSMT $ constrain
      $   (right1 .== left2 .|| right2 .== left1) .&& (top1 .== top2 .|| bottom1 .== bottom2)
      .|| (bottom1 .== top2 .|| bottom2 .== top1) .&& (left1 .== left2 .|| right1 .== right2)


arboresence nodes net = do

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
        (sourceLeft + literal l, sourceBottom + literal b)
        (sourceLeft + literal r, sourceBottom + literal t)

      pinConnect target $ Rect
        (sinkLeft + literal m, sinkBottom + literal c)
        (sinkLeft + literal s, sinkBottom + literal u)

      pathCombine start target

      pure [start, target]

    | (j, source) <- sources
    , (i, assignments) <- assocs $ contacts net
    , sink <- assignments
    , let (gate, Rect (  sinkLeft,   sinkBottom) _) = nodes ! gateIndex i
    , let (from, Rect (sourceLeft, sourceBottom) _) = nodes ! gateIndex j
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
