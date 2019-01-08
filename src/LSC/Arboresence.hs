{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSC.Arboresence where

import Control.Monad.Trans
import Data.Default
import Data.Foldable
import qualified Data.Map as Map
import Data.SBV
import Data.SBV.Control
import Data.Text (unpack)
import System.IO

import LSC.Types


pnr :: NetGraph -> LSC NetGraph
pnr netlist@(NetGraph name pins _ gates wires) = do

  debug
    [ "start pnr @ module", unpack name
    , "-", show $ length gates, "gates"
    , "-", show $ length wires, "nets"
    ]

  liftSMT $ do
    setOption $ ProduceUnsatCores True

  result <- concLSC [ arboresence gates net | net <- take 1 $ toList wires ]

  debug ["stop  pnr @ module", unpack name]

  pure netlist


arboresence nodes net =
  error $ show $ net


arbor pins@(_, outputs, _) nodes net source = do
  sequence
    [ do

      (_, edge) <- freeWirePolygon net

      target <- case Map.lookup gate nodes of
        Just (bottomLeft : _) -> do
          pure (fst bottomLeft + literal px, snd bottomLeft + literal py)
        _ -> do
          target <- freePoint
          liftSMT $ constrain $ bAnd [ fst target .> x | _ : _ : (x, _) : _ <- toList nodes ]
          pure target

      liftSMT $ do
        constrain $ head edge .== source
        constrain $ last edge .== target

      pure ((net, idn), edge)

    | (gate, cs) <- Map.assocs (contacts net)
                 ++ [ (def, [(netIdent net, def)]) | netIdent net `elem` outputs ]
    , (idn, pin) <- take 1 cs
    , pinDir pin == In
    , (px, py, _, _) <- take 1 $ portRects $ pinPort pin
    ]


manhattan :: [(SInteger, SInteger)] -> SInteger
manhattan ((x1, y1) : (x2, y2) : xs)
  = abs (x1 - x2)
  + abs (y1 - y2)
  + manhattan ((x2, y2) : xs)
manhattan _ = 0


boundedSpace nodes = do
  (w, h) <- padDimensions <$> ask
  sequence_
    [ liftSMT $ do
        constrain $ x .> literal 0 &&& y .> literal 0
        constrain $ x .< literal w &&& y .< literal h
    | rect <- Map.elems nodes
    , (x, y) <- take 1 rect
    ]


rectilinear steiner = sequence_ [ uniform edge *> rectangular edge | (_, edge) <- steiner ]

rectangular ((x1, y1) : (x2, y2) : xs) = do
  liftSMT $ constrain $ x1 .== x2 ||| y1 .== y2
  rectangular ((x2, y2) : xs)
rectangular _ = pure ()


segments (x : y : xs) = (x, y) : segments (y : xs)
segments _ = []


uniform ((x1, y1) : (x2, y2) : (x3, y3) : xs) = do
  liftSMT $ do
    constrain $ (x1, y1) .== (x2, y2)
      ||| (x3 - x2 .> 0) .== (x1 - x2 .> 0)
      &&& (y3 - y2 .> 0) .== (y1 - y2 .> 0)
  uniform ((x2, y2) : (x3, y3) : xs)
uniform _ = pure ()


rows n nodes = channels n $ Map.elems nodes

channels n [] = pure ()
channels n nodes = do

  let (xs, rest) = splitAt n nodes

  sequence_
    [ do

      liftSMT $ do
        constrain $ left1 .== left2
        constrain $ bottom2 .> top1

    | (path1, path2) <- xs `zip` drop 1 xs
    , let (left1, _) : (_, top1) : _ = path1
    , let (left2, bottom2)  : _  : _ = path2
    ]

  sequence_
    [ do

      liftSMT $ do
        constrain $ left2 - right1 .== 8000

    | (path1, path2) <- take 1 xs `zip` take 1 rest
    , let _ : (right1, _) : _ = path1
    , let (left2, _)  : _ : _ = path2
    ]

  channels n rest


collision nodes = do
  sequence_
    [ do

      liftSMT $ do
        constrain
          $   left2 .> right1 ||| left1 .> right2
          ||| bottom1 .> top2 ||| bottom2 .> top1

    | (i, path1) <- [ 1 .. ] `zip` Map.elems nodes
    ,     path2  <- i `drop` Map.elems nodes
    , let (left1, bottom1) : (right1, top1) : _ = path1
    , let (left2, bottom2) : (right2, top2) : _ = path2
    ]


freeGatePolygon gate = do

  path @ (left, bottom) : (right, top) : _ <- freePolygon 2 
  
  let Rect (x1, y1) (x2, y2) : _ = gatePath gate

  liftSMT $ constrain
    $   left   .== literal x1
    &&& bottom .== literal y1
    &&& right  .== literal x2
    &&& top    .== literal y2

  pure (gate, path)


freeWirePolygon net = do

  resolution <- wireResolution <$> ask

  path <- freePolygon resolution

  pure (net, path)


freePolygon n = sequence $ replicate n freePoint


freePoint = liftSMT $ (,) <$> free_ <*> free_


computeStage1 nodes = do

  liftSMT $ query $ do
    result <- checkSat
    case result of

      Sat -> Circuit2D

        <$> pure mempty

        <*> pure mempty

      Unsat -> do

        unsat <- getUnsatCore
        liftIO $ sequence_ $ hPutStrLn stderr <$> unsat

        pure $ Circuit2D mempty mempty

      _ -> do

        reason <- getUnknownReason
        liftIO $ hPutStrLn stderr $ show reason

        pure $ Circuit2D mempty mempty


