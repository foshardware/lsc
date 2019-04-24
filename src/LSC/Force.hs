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
type F = Double

scale :: F
scale = 100


glossForce :: NetGraph -> LSC NetGraph
glossForce top = do

  tech <- technology

  let k = top ^. gates . to length

  let edges = fromList $ join [ distinctPairs $ keys $ net ^. contacts | net <- toList $ top ^. nets ]
  let allPairs = fromList $ distinctPairs [0 .. k-1]

  let particleVector = middle tech <$> view gates top

  it <- view iterations <$> environment

  let e = Step [(edges, hooke 1 4), (allPairs, coulomb 0.1)] particleVector
  let r = fromListN it $ simulate 0.001 e

  liftIO $ runAnimation (0, r ! 0) (renderStep . snd) $ \ _ _ (i, f) -> (succ i, r ! mod i it)

  pure top


renderStep :: Step V F -> Frame
renderStep = foldMap rectangle . view particles

rectangle :: Particle V F -> Frame
rectangle p = poly $ bimap (/ scale) (/ scale) <$>
  [ (x - w / 2, y - h / 2)
  , (x - w / 2, y + h / 2)
  , (x + w / 2, y + h / 2)
  , (x + w / 2, y - h / 2)
  ]
  where
    (w, h) = bimap fromIntegral fromIntegral $ p ^. dims
    V2 x y = unP $ p ^. pos


placeForce :: NetGraph -> LSC NetGraph
placeForce top = do

  tech <- technology

  let k = top ^. gates . to length

  let edges = fromList $ join [ distinctPairs $ keys $ net ^. contacts | net <- toList $ top ^. nets ]
  let allPairs = fromList $ distinctPairs [0 .. k-1]

  let particleVector = middle tech <$> view gates top

  it <- view iterations <$> environment

  let e = Step [(edges, hooke 1 4), (allPairs, coulomb 0.1)] particleVector
  let r = view particles $ last $ take it $ simulate 0.001 e

  pure $ top
    & gates %~ fmap (\ g -> g & geometry .~ maybeToList (layered r (g ^. number) <$> lookupDimensions g tech))


middle :: Technology -> Gate -> Particle V F
middle tech g = Particle
  p
  zero
  zero
  (maybe (10, 10) id $ lookupDimensions g tech)
  where p = P $ last $ V2 0 0 : [ V2 (r ^. l) (r ^. b) | r <- fmap fromIntegral <$> g ^. geometry ]



layered :: Vector (Particle V F) -> Int -> (Integer, Integer) -> Component Layer Integer
layered r i (x, y)
  = Layered
    (ceiling a - div x 2)
    (ceiling b - div y 2)
    (ceiling a + div x 2)
    (ceiling b + div y 2)
    [Metal2, Metal3]
    where V2 a b = unP $ view pos $ r ! i


simulate :: F -> Step V F -> [Step V F]
simulate damping = iterate $ step damping


step :: F -> Step V F -> Step V F
step d = (over particles . fmap) (particle d) . recalc


particle :: F -> Particle V F -> Particle V F
particle d = stepPos . stepVel
  where
    stepVel p = p & vel .~ (d *^ (p^.vel ^+^ p^.force))
    stepPos p = p & pos %~ (.+^ p^.vel)


recalc :: Step V F -> Step V F
recalc = calcForces . zeroForces

  where

    zeroForces
      = (particles %~) . fmap
      $ force .~ zero

    calcForces (Step fs ps)
      = Step fs
      $ ala Endo foldMap (foldMap (\ (es, f) -> mkForce f <$> es) fs) (runST $ V.freeze =<< unsafeThaw ps)

    mkForce :: (Num n, Additive v) => (Point v n -> Point v n -> v n) -> Edge -> Vector (Particle v n) -> Vector (Particle v n)
    mkForce f (i1, i2) v = case (v ! i1, v ! i2) of
      (p1, p2) -> runST $ do
        m <- unsafeThaw v
        write m i1 $! p1 & force %~ (^+^ f (p1^.pos) (p2^.pos))
        write m i2 $! p2 & force %~ (^+^ f (p1^.pos) (p2^.pos))
        unsafeFreeze m


dist :: (F -> F) -> Point V F -> Point V F -> V F
dist f p1 p2 = f (distance p1 p2) *^ signorm (p2 .-. p1)

hooke :: F -> F -> Point V F -> Point V F -> V F
hooke k l = dist $ \ d -> k * (d - l)

coulomb :: F -> Point V F -> Point V F -> V F
coulomb k = dist $ \ d -> -k/ (d*d)