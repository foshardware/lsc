{-# LANGUAGE FlexibleContexts #-}

module LSC.Force where

import Control.Lens
import Control.Monad
import Control.Monad.ST
import Control.Monad.IO.Class
import Data.Foldable
import Data.Map (keys)
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as V
import Data.Vector (Vector, fromList, fromListN, unsafeThaw, unsafeFreeze)
import Data.Vector.Mutable (write)
import Linear.Affine hiding (Vector)
import Linear.Metric
import Linear.Vector
import Linear.V2
import Prelude hiding (lookup)

import LSC.Animation
import LSC.Types


type V = V2
type R = Double

scale :: R
scale = 100


placeForce :: NetGraph -> LSC NetGraph
placeForce top = do

  tech <- technology

  let k = top ^. gates . to length

  let edges = fromList $ join [ distinctPairs $ keys $ net ^. contacts | net <- toList $ top ^. nets ]
  let nodes = fromList $ distinctPairs [0 .. k-1]

  let kr = 7

  let outer = fromList $ distinctPairs [k .. k+kr] ++ [ (i, o) | i <- [0 .. k-1], o <- [k .. k+kr] ]
  let tight = fromList $ zip [k .. k+kr] [k+1 .. k+kr]

  let particleVector = center tech <$> view gates top

  let ev = fromIntegral

  let xmin = minimum [q ^. l . to ev | g <- toList $ top ^. gates, q <- g ^. geometry]
      ymin = minimum [q ^. b . to ev | g <- toList $ top ^. gates, q <- g ^. geometry]
      xmax = maximum [q ^. r . to ev | g <- toList $ top ^. gates, q <- g ^. geometry]
      ymax = maximum [q ^. t . to ev | g <- toList $ top ^. gates, q <- g ^. geometry]

  let gap = 10000

  let ringParticles = fromList
        [ ringParticle (xmin - gap, ymin - gap)
        , ringParticle (xmax / 2, ymin - gap)
        , ringParticle (xmax + gap, ymin - gap)
        , ringParticle (xmax + gap, ymax / 2)
        , ringParticle (xmax + gap, ymax + gap)
        , ringParticle (xmax / 2, ymax + gap)
        , ringParticle (xmin - gap, ymax + gap)
        , ringParticle (xmin - gap, ymax / 2)
        ]

  it <- view iterations <$> environment
  ui <- view enableVisuals <$> environment

  let forces =
        [ (edges, hooke 1 4)
        , (nodes, coulomb 0.1)
        , (tight, hooke 4 8)
        , (outer, coulomb 20)
        ]

  let v = fromListN it $ simulate 0.001 $ Step forces $ particleVector <> ringParticles

  when ui
    $ liftIO $ do
      runAnimation (0, v ^? ix 0) (maybe mempty renderStep . snd)
        $ \ _ _ (i, _) -> (succ i, v ^? ix (mod i it))

  let ps = maybe mempty id $ v ^? ix (pred it) . particles

  pure $ top
    & gates %~ fmap (\ g -> g & geometry .~ toList (layered ps (g ^. number) <$> lookupDims g tech))



ringParticle :: (R, R) -> Particle V R
ringParticle (x, y) = Particle (P (V2 x y)) zero zero (42 * ceiling scale, 42 * ceiling scale)


renderStep :: Step V R -> Frame
renderStep = foldMap rectangle . view particles

rectangle :: Particle V R -> Frame
rectangle p = poly $ bimap (/ scale) (/ scale) <$>
  [ (x - w / 2, y - h / 2)
  , (x - w / 2, y + h / 2)
  , (x + w / 2, y + h / 2)
  , (x + w / 2, y - h / 2)
  ]
  where
    (w, h) = bimap fromIntegral fromIntegral $ p ^. dims
    V2 x y = unP $ p ^. pos



center :: Technology -> Gate -> Particle V R
center tech g = Particle
  p
  zero
  zero
  (maybe (10, 10) id $ lookupDims g tech)
  where
    p = P $ last $ V2 0 0 :
        [ V2 (q ^. l + fromIntegral w / 2) (q ^. b + fromIntegral h / 2)
        | q <- fmap fromIntegral <$> g ^. geometry
        , (w, h) <- maybe [(10, 10)] pure $ lookupDims g tech
        ]



layered :: Vector (Particle V R) -> Int -> (Integer, Integer) -> Component Layer Integer
layered v i (x, y)
  = Layered
    (ceiling vx - div x 2)
    (ceiling vy - div y 2)
    (ceiling vx + div x 2)
    (ceiling vy + div y 2)
    [Metal2, Metal3]
    where V2 vx vy = maybe zero unP $ v ^? ix i . pos


simulate :: R -> Step V R -> [Step V R]
simulate d = iterate $ step d


step :: R -> Step V R -> Step V R
step d = (over particles . fmap) (particle d) . recalc


particle :: R -> Particle V R -> Particle V R
particle d = stepPos . stepVel
  where
    stepVel p = p & vel .~ (d *^ (p^.vel ^+^ p^.force))
    stepPos p = p & pos %~ (.+^ p^.vel)


recalc :: Step V R -> Step V R
recalc = calcForces . zeroForces

  where

    zeroForces
      = (particles %~) . fmap
      $ force .~ zero

    calcForces (Step fs ps)
      = Step fs
      $ ala Endo foldMap (foldMap (\ (es, f) -> mkForce f <$> es) fs) (runST $ unsafeFreeze =<< V.thaw ps)

    mkForce :: (Point V R -> Point V R -> V R) -> Edge -> Vector (Particle V R) -> Vector (Particle V R)
    mkForce f (i1, i2) v = case (,) <$> v ^? ix i1 <*> v ^? ix i2 of
      Nothing -> v
      Just (p1, p2) -> runST $ do
        m <- unsafeThaw v
        write m i1 $! p1 & force %~ (^+^ f (p1^.pos) (p2^.pos))
        write m i2 $! p2 & force %~ (^+^ f (p1^.pos) (p2^.pos))
        unsafeFreeze m


euclidean :: (R -> R) -> Point V R -> Point V R -> V R
euclidean f p1 p2 = f (distance p1 p2) *^ signorm (p2 .-. p1)

hooke :: R -> R -> Point V R -> Point V R -> V R
hooke k e = euclidean $ \ d -> k * (d - e)

coulomb :: R -> Point V R -> Point V R -> V R
coulomb k = euclidean $ \ d -> -k / (d*d)
