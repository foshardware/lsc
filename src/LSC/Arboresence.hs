{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSC.Arboresence where

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

  edges <- sequence $ netPolygon nodes <$> nets

  boundedSpace nodes
  collision nodes

  result <- computeStage1 nodes edges

  debug ["stop  pnr @ module", unpack name]

  pure netlist
    { gateVector = maybe (gateVector netlist) fst result
    , netMapping = maybe (netMapping netlist) snd result
    }



boundedSpace nodes = do

  (width, height) <- padDimensions <$> ask

  sequence_
    [ do
      liftSMT $ do
        constrain
          $   left   .> literal 0
          .&& left   .< literal width
          .&& bottom .> literal 0
          .&& bottom .< literal height

    | (_, path) <- toList nodes
    , (left, bottom) <- take 1 path
    ]


collision nodes = do
  sequence_
    [ do

      liftSMT $ do
        constrain
          $   left2 .> right1
          .|| left1 .> right2
          .|| bottom1 .> top2
          .|| bottom2 .> top1

    | ((_, path1), (_, path2)) <- distinctPairs $ toList nodes
    , let (left1, bottom1) : (right1, top1) : _ = path1
    , let (left2, bottom2) : (right2, top2) : _ = path2
    ]


distinctPairs :: [a] -> [(a, a)]
distinctPairs [] = []
distinctPairs (x : xs) = fmap (x, ) xs ++ distinctPairs xs


freeGatePolygon gate = do

  path @ ((left, bottom) : (right, top) : _) <- freeRectangle
  (width, height) <- lookupDimensions gate <$> ask

  liftSMT $ constrain
    $   right - left .== literal width
    .&& top - bottom .== literal height

  pure (gate, path)


netPolygon nodes net = do

  start @ ((leftStart, bottomStart) : (rightStart, topStart) : _) <- freeRectangle

  sequence_
    [ do

      liftSMT $ constrain
        $   leftStart   .== left   + literal a
        .&& bottomStart .== bottom + literal b
        .&& rightStart  .== left   + literal c
        .&& topStart    .== bottom + literal d

    | (i, assignments) <- assocs $ contacts net
    , (_, source) <- assignments
    , pinDir source == Out
    , Rect (a, b) (c, d) <- take 1 $ portRects $ pinPort source
    , let (gate, (left, bottom) : _) = nodes ! gateIndex i
    ]

  hyperedge <- sequence
    [ do

      target @ ((leftTarget, bottomTarget) : (rightTarget, topTarget) : _) <- freeRectangle
    
      liftSMT $ constrain
        $   leftTarget   .== left   + literal a
        .&& bottomTarget .== bottom + literal b
        .&& rightTarget  .== left   + literal c
        .&& topTarget    .== bottom + literal d

      pure target

    | (i, assignments) <- assocs $ contacts net
    , (_, sink) <- assignments
    , pinDir sink == In
    , Rect (a, b) (c, d) <- take 1 $ portRects $ pinPort sink
    , let (gate, (left, bottom) : _) = nodes ! gateIndex i
    ]

  pure (net, start : hyperedge)


freeRectangle = freePolygon 2

freePolygon n = sequence $ replicate n freePoint


freePoint = liftSMT $ (,) <$> free_ <*> free_


computeStage1 nodes edges = do

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
