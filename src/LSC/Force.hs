{-# LANGUAGE FlexibleContexts #-}

module LSC.Force where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.ST
import Control.Monad.IO.Class
import Data.Default.Class
import Data.Foldable
import Data.Map (keys)
import Data.Maybe
import Data.Monoid
import qualified Data.Vector as V
import Data.Vector (Vector, fromList, (!), copy, fromListN, unsafeThaw, unsafeFreeze)
import Data.Vector.Mutable (write)
import Linear.Affine hiding (Vector)
import Linear.Metric
import Linear.Vector
import Linear.V2
import Prelude hiding (lookup)

import System.IO.Unsafe

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

  let particleVector = center tech <$> view gates top

  it <- view iterations <$> environment
  ui <- view enableVisuals <$> environment

  let e = Step [(edges, hooke 1 4), (nodes, coulomb 0.1)] particleVector
  let r = fromListN it $ simulate 0.001 e

  when ui
    $ liftIO $ do
      runAnimation (0, r ! 0) (renderStep . snd) $ \ _ _ (i, f) -> (succ i, r ! mod i it)

  let ps = view particles $ r ! pred it

  pure $ top
    & gates %~ fmap (\ g -> g & geometry .~ maybeToList (layered ps (g ^. number) <$> lookupDimensions g tech))


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
  (maybe (10, 10) id $ lookupDimensions g tech)
  where
    p = P $ last $ V2 0 0 :
        [ V2 (r ^. l + fromIntegral w / 2) (r ^. b + fromIntegral h / 2)
        | r <- fmap fromIntegral <$> g ^. geometry
        , (w, h) <- maybe [(10, 10)] pure $ lookupDimensions g tech
        ]



layered :: Vector (Particle V R) -> Int -> (Integer, Integer) -> Component Layer Integer
layered r i (x, y)
  = Layered
    (ceiling a - div x 2)
    (ceiling b - div y 2)
    (ceiling a + div x 2)
    (ceiling b + div y 2)
    [Metal2, Metal3]
    where V2 a b = unP $ view pos $ r ! i


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
    mkForce f (i1, i2) v = case (v ! i1, v ! i2) of
      (p1, p2) -> runST $ do
        m <- unsafeThaw v
        write m i1 $! p1 & force %~ (^+^ f (p1^.pos) (p2^.pos))
        write m i2 $! p2 & force %~ (^+^ f (p1^.pos) (p2^.pos))
        unsafeFreeze m


euclidean :: (R -> R) -> Point V R -> Point V R -> V R
euclidean f p1 p2 = f (distance p1 p2) *^ signorm (p2 .-. p1)

hooke :: R -> R -> Point V R -> Point V R -> V R
hooke k l = euclidean $ \ d -> k * (d - l)

coulomb :: R -> Point V R -> Point V R -> V R
coulomb k = euclidean $ \ d -> -k / (d*d)