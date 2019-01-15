{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LSC.Arboresence where

import Control.Lens hiding ((.>), inside)
import Control.Monad
import Control.Monad.Trans
import Data.Foldable
import Data.Map (assocs)
import Data.Vector (Vector, (!))
import Data.SBV
import Data.SBV.Control
import Data.Text (unpack)
import System.IO

import LSC.Types


pnr :: NetGraph -> LSC NetGraph
pnr netlist = do

  debug
    [ "start pnr @ module", unpack $ netlist ^. identifier
    , "-", show $ length $ netlist ^. gates, "gates"
    , "-", show $ length $ netlist ^. nets, "nets"
    ]

  liftSMT $ do
    setOption $ ProduceUnsatCores True


  nodes <- sequence $ freeGatePolygon <$> (netlist ^. gates)

  outerPins <- sequence $ freePinPolygon <$> (netlist ^. supercell . pins)

  ring <- powerUpAndGround nodes

  area <- placement nodes ring outerPins

  edges <- sequence $ arboresence 4 nodes outerPins <$> (netlist ^. nets)

  disjointGates nodes
  disjointNets edges

  result <- checkResult area outerPins ring nodes edges

  debug ["stop  pnr @ module", unpack $ netlist ^. identifier]

  pure $ netlist
    & supercell .~ maybe (netlist ^. supercell) snd result
    & gates .~ maybe (netlist ^. gates) (fst . fst) result
    & nets .~ maybe (netlist ^. nets) (snd . fst) result


placement nodes ring outerPins = do

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
    | (pin, path) <- toList outerPins
    , pin ^. dir == In
    ]
  sequence_
    [ liftSMT $ constrain
        $   right path .== right area
        .&& right path - right (outer ring) .> d
    | (pin, path) <- toList outerPins
    , pin ^. dir == Out
    ]
  sequence_
    [ liftSMT $ constrain
        $   bottom path .>= bottom (inner ring)
        .&&    top path .<=    top (inner ring)
    | (_, path) <- toList outerPins
    ]

  sequence_
    [ disjoint a b
    | ((_, a), (_, b)) <- distinctPairs $ toList outerPins
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

  dims <- lookupDimensions gate <$> ask
  for_ dims $ \ (w, h) -> liftSMT $ constrain
    $   right path - left path .== literal w
    .&& top path - bottom path .== literal h

  pure (gate, path)


freeWirePolygon net = do

  path <- freeRectangle

  (w, h) <- view standardPin <$> ask

  liftSMT $ constrain
      $    width path .== literal w
      .|| height path .== literal h

  pure (net, path)


freePinPolygon pin = do

  path <- freeRectangle

  (w, h) <- view standardPin <$> ask

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


arboresence n nodes outerPins net = do

  hyperedge <- sequence
    [ do

      wire <- sequence $ replicate n $ snd <$> freeWirePolygon net

      src `connect` head wire
      snk `connect` last wire

      sequence_ [ connect i j | i <- wire | j <- drop 1 wire ]

      pure wire

    | src <- vertices Out
    , snk <- vertices In
    ]

  pure (net, join hyperedge)

  where

    vertices d =
      [ Rect
          (left src + literal l, bottom src + literal b)
          (left src + literal r, bottom src + literal t)
      | (j, assignments) <- assocs $ net ^. contacts
      , source <- assignments
      , source ^. dir == d
      , let (gate, src) = nodes ! view integer j
      , Rect (l, b) (r, t) <- take 1 $ source ^. port . geometry
      ] ++
      [ path
      | (pin, path) <- toList outerPins
      , pin ^. dir /= d
      , view identifier pin == view identifier net
      ]


powerUpAndGround nodes = do
  
  ring <- freeRing

  (w, h) <- view standardPin <$> ask

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


checkResult area outerPins ring nodes edges = do

  liftSMT $ query $ do
    result <- checkSat
    case result of

      Sat -> do

        pad <- rectangle area
        a <- sequence $  rectangle <$> toList ring
        b <- sequence $  pinAssign <$> outerPins
        c <- sequence $ gateAssign <$> nodes
        d <- sequence $  netAssign <$> edges

        pure $ Just ((c, d), AbstractGate [pad] a b)

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
      >>= \ path -> pure $ gate & geometry .~ pure path

    pinAssign (pin, xs) = rectangle xs
      >>= \ path -> pure $ pin & port .~ Port Metal1 [path]

    netAssign (net, edge) = sequence (rectangle <$> edge)
      >>= \ path -> pure $ net & geometry .~ path

    rectangle r = Rect
      <$> ((, ) <$> getValue (left r)  <*> getValue (bottom r))
      <*> ((, ) <$> getValue (right r) <*> getValue (top r))
