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

  boundedSpace nodes
  collision nodes

  result <- checkResult nodes edges

  debug ["stop  pnr @ module", unpack name]

  pure netlist
    { gateVector = maybe (gateVector netlist) fst result
    , netMapping = maybe (netMapping netlist) snd result
    }


boundedSpace nodes | length nodes < 1 = pure ()
boundedSpace nodes = do

  let inner = fmap snd $ drop 1 $ toList nodes

  let (left, bottom) : (right, top) : _ = snd $ nodes ! 0

  let lowerBound = fromIntegral (minBound :: Int)
      upperBound = fromIntegral (maxBound :: Int)

  liftSMT $ constrain
    $   left   .<= foldr smin upperBound [ x | (x, _) : _ : _ <- inner ]
    .&& bottom .<= foldr smin upperBound [ x | (_, x) : _ : _ <- inner ]
    .&& right  .>= foldr smax lowerBound [ x | _ : (x, _) : _ <- inner ]
    .&& top    .>= foldr smax lowerBound [ x | _ : (_, x) : _ <- inner ]

  let tie = literal $ floor $ sqrt $ fromIntegral $ length nodes :: SInteger

  let width  = sum [ right - left | (left, _) : (right, _) : _ <- inner ]
      height = sum [ top - bottom | (_, bottom) : (_, top) : _ <- inner ]

  liftSMT $ constrain
    $   left   .== literal 0
    .&& right  .<= width
    .&& bottom .== literal 0
    .&& top    .<= sDiv height tie


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

    | ((_, path1), (_, path2)) <- distinctPairs $ drop 1 $ toList nodes
    , let (left1, bottom1) : (right1, top1) : _ = path1
    , let (left2, bottom2) : (right2, top2) : _ = path2
    ]


distinctPairs :: [a] -> [(a, a)]
distinctPairs [] = []
distinctPairs (x : xs) = fmap (x, ) xs ++ distinctPairs xs


freeGatePolygon gate = do

  path @ ((left, bottom) : (right, top) : _) <- freeRectangle

  dimensions <- lookupDimensions gate <$> ask
  for_ dimensions $ \ (width, height) -> liftSMT $ constrain
    $   right - left .== literal width
    .&& top - bottom .== literal height

  pure (gate, path)


overlap
  ((left1, bottom1) : (right1, top1) : _)
  ((left2, bottom2) : (right2, top2) : _)
  = do
    liftSMT $ constrain
      $   left1   .== left2
      .&& bottom1 .== bottom2
      .&& right1  .== right2
      .&& top1    .== top2


pinConnect
  ((left1, bottom1) : (right1, top1) : _)
  ((left2, bottom2) : (right2, top2) : _)
  = do
    liftSMT $ constrain
      $   left1   .== left2   .&& bottom1 .== bottom2 .&& right1 .== right2
      .|| left1   .== left2   .&& bottom1 .== bottom2 .&& top1   .== top2
      .|| left1   .== left2   .&& right1  .== right2  .&& top1   .== top2
      .|| bottom1 .== bottom2 .&& right1  .== right2  .&& top1   .== top2


arboresence nodes net = do


  sources <- sequence
    [ do
      pure (gate, source)
    | (gate, assignments) <- assocs $ contacts net
    , (_, source) <- assignments
    , pinDir source == Out
    ]

  hyperedge <- sequence
    [ do

      start  <- freeRectangle
      target <- freeRectangle

      case pinPort sink of
        FreePort
          -> pure ()
        port
          | Rect (l, b) (r, t) : _ <- portRects port
          -> overlap target
              [ (sinkLeft + literal l, sinkBottom + literal b)
              , (sinkLeft + literal r, sinkBottom + literal t)
              ]

      case pinPort source of
        FreePort
          -> pure ()
        port
          | Rect (l, b) (r, t) : _ <- portRects port
          -> overlap start
              [ (sourceLeft + literal l, sourceBottom + literal b)
              , (sourceLeft + literal r, sourceBottom + literal t)
              ]

      pure [start, target]

    | (j, source) <- sources
    , (i, assignments) <- assocs $ contacts net
    , (_, sink) <- assignments
    , let (gate, (sinkLeft, sinkBottom)     : _) = nodes ! gateIndex i
    , let (from, (sourceLeft, sourceBottom) : _) = nodes ! gateIndex j
    , pinDir sink == In
    ]

  pure (net, join hyperedge)


freeRectangle = freePolygon 2

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

    rectangle ((left, bottom) : (right, top) : _) = Rect
      <$> ((, ) <$> getValue left  <*> getValue bottom)
      <*> ((, ) <$> getValue right <*> getValue top)
