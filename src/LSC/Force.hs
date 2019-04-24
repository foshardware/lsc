{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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


data Particle v n = Particle
  { _pos   :: Point v n
  , _vel   :: v n
  , _force :: v n
  } deriving (Eq, Show)

makeLenses ''Particle

initParticle :: (Additive v, Num n) => Point v n -> Particle v n
initParticle p = Particle p zero zero

type Edge = (Int, Int)


data Step v n = Step
  { _forces    :: [(Vector Edge, Point v n -> Point v n -> v n)]
  , _particles :: Vector (Particle v n)
  }

makeLenses ''Step


glossForce :: NetGraph -> LSC NetGraph
glossForce top = do

  tech <- technology

  let k = top ^. gates . to length

  let edges = fromList $ join [ distinctPairs $ keys $ net ^. contacts | net <- toList $ top ^. nets ]
  let allPairs = fromList $ distinctPairs [0 .. k-1]

  let particleVector = initParticle . P . bottomLeft <$> view gates top

  it <- view iterations <$> environment

  let e = Step [(edges, hooke 1 4), (allPairs, coulomb 0.1)] particleVector
  let r = fromListN it $ simulate 0.001 e

  liftIO $ runAnimation (0, r ! 0) (renderStep . snd) $ \ _ _ (i, f) -> (succ i, r ! mod i it)

  pure top


renderStep :: Step V2 Float -> Frame
renderStep = foldMap (rect . unP . view pos) . view particles

rect :: V2 Float -> Frame
rect (V2 x y) = poly
  [ (x / sc - 5, y / sc - 5)
  , (x / sc - 5, y / sc + 5)
  , (x / sc + 5, y / sc + 5)
  , (x / sc + 5, y / sc - 5)
  ]

sc :: Float
sc = 100


placeForce :: NetGraph -> LSC NetGraph
placeForce top = do

  tech <- technology

  let k = top ^. gates . to length

  let edges = fromList $ join [ distinctPairs $ keys $ net ^. contacts | net <- toList $ top ^. nets ]
  let allPairs = fromList $ distinctPairs [0 .. k-1]

  let particleVector = initParticle . P . bottomLeft <$> view gates top

  it <- view iterations <$> environment

  let e = Step [(edges, hooke 1 4), (allPairs, coulomb 0.1)] particleVector
  let r = view particles $ last $ take it $ simulate 0.001 e

  pure $ top
    & gates %~ fmap (\ g -> g & geometry .~ layered r (g ^. number) (lookupDimensions g tech))


bottomLeft :: Gate -> V2 Float
bottomLeft g = last $ V2 0 0 : [ V2 (r ^. l) (r ^. b) | r <- fmap fromIntegral <$> g ^. geometry ]


layered _ _ Nothing = []
layered r i (Just (x, y))
  = pure
  $ Layered
    (ceiling a - div x 2)
    (ceiling b - div y 2)
    (ceiling a + div x 2)
    (ceiling b + div y 2)
    [Metal2, Metal3]
    where V2 a b = unP $ view pos $ r ! i


simulate :: (Metric v, Num n, Ord n) => n -> Step v n -> [Step v n]
simulate damping = iterate $ step damping


step :: (Additive v, Num n) => n -> Step v n -> Step v n
step d = (over particles . fmap) (particle d) . recalc


particle :: (Additive v, Num n) => n -> Particle v n -> Particle v n
particle d = stepPos . stepVel
  where
    stepVel p = vel .~ (d *^ (p^.vel ^+^ p^.force)) $ p
    stepPos p = pos %~ (.+^ p^.vel) $ p


recalc :: (Additive v, Num n) => Step v n -> Step v n
recalc = calcForces . zeroForces

  where

    zeroForces
      = (particles %~) . fmap
      $ force .~ zero

    calcForces (Step fs ps)
      = Step fs
      $ ala Endo foldMap (foldMap (\ (es, f) -> mkForce f <$> es) fs) (runST $ V.freeze =<< unsafeThaw ps)

    mkForce :: (Num n, Additive v) => (Point v n -> Point v n -> v n) -> (Int, Int) -> (Vector (Particle v n) -> Vector (Particle v n))
    mkForce f (i1, i2) v = case (v ! i1, v ! i2) of
      (p1, p2) -> runST $ do
        m <- unsafeThaw v
        write m i1 $! p1 & force %~ (^+^ f (p1^.pos) (p2^.pos))
        write m i2 $! p2 & force %~ (^+^ f (p1^.pos) (p2^.pos))
        unsafeFreeze m


dist :: (Metric v, Floating n) => (n -> n) -> Point v n -> Point v n -> v n
dist f p1 p2 = f (distance p1 p2) *^ signorm (p2 .-. p1)

hooke :: (Metric v, Floating n) => n -> n -> Point v n -> Point v n -> v n
hooke k l = dist $ \ d -> k * (d - l)

coulomb :: (Metric v, Floating n) => n -> Point v n -> Point v n -> v n
coulomb k = dist $ \ d -> -k/ (d*d)