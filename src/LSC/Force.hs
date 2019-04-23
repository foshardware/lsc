{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module LSC.Force where

import Control.Lens
import Control.Monad
import Data.Default.Class
import Data.Foldable
import Data.Map (keys)
import Data.Maybe
import Data.Monoid
import Data.Vector (Vector, fromList, (!), update)
import Linear.Affine hiding (Vector)
import Linear.Metric
import Linear.Vector
import Linear.V2
import Prelude hiding (lookup)

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
  { _forces    :: Vector (Vector Edge, Point v n -> Point v n -> v n)
  , _particles :: Vector (Particle v n)
  }

makeLenses ''Step


data ForceOptions n = ForceOpts
  { _damping     :: n
  , _energyLimit :: Maybe n
  , _stepLimit   :: Maybe Int
  }

makeLenses ''ForceOptions

instance Fractional n => Default (ForceOptions n) where
  def = ForceOpts
    { _damping     = 0.8
    , _energyLimit = Just 0.001
    , _stepLimit   = Nothing
    }



placeForce :: NetGraph -> LSC NetGraph
placeForce top = do

  tech <- technology

  let k = top ^. gates . to length

  let edges = fromList $ join [ distinctPairs $ keys $ net ^. contacts | net <- toList $ top ^. nets ]
  let allPairs = fromList $ distinctPairs [0 .. k-1]

  let particleMap = initParticle . P . bottomLeft <$> view gates top

  let e = Step (fromList [(edges, hooke 0.5 4), (allPairs, coulomb 20)]) particleMap
  let ev = view particles $ last $ simulate e $ def
             & damping     .~ 0.8
             & energyLimit .~ Just 0.001
             & stepLimit   .~ Just 0

  pure $ top
    & gates %~ fmap (\ g -> g & geometry .~ fromDims ev (g ^. number) (lookupDimensions g tech))


bottomLeft :: Gate -> V2 Double
bottomLeft g = maybe (V2 0 0) (\r -> V2 (fromIntegral $ r ^. l) (fromIntegral $ r ^. b)) (listToMaybe $ g ^. geometry)


fromDims  _ _ Nothing = []
fromDims ev i (Just (x, y)) = [Layered (a - div x 2) (b - div y 2) (a + div x 2) (b + div y 2) [Metal2, Metal3]]
  where
    V2 a' b' = unP $ view pos (ev ! i)
    a = ceiling a'
    b = ceiling b'




simulate :: (Metric v, Num n, Ord n) => Step v n -> ForceOptions n -> [Step v n]
simulate e opts
  = (e:)
  $ takeWhile (maybe (const True) (<) (opts ^. energyLimit) . kinetic)
  $ maybe id take (opts ^. stepLimit)
  $ drop 1
  $ iterate (opts ^. damping . to step)
  $ e



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
      $ ala Endo foldMap (foldMap (\ (es, f) -> mkForce f <$> es) fs) ps

    mkForce f (i1, i2) m = case (m ! i1, m ! i2) of
      (p1, p2) -> update m $ fromList
        [ (i1, m ! i1 & force %~ (^+^ f (p1^.pos) (p2^.pos)))
        , (i2, m ! i2 & force %~ (^-^ f (p1^.pos) (p2^.pos)))
        ]


kinetic :: (Metric v, Num n) => Step v n -> n
kinetic = ala Sum foldMap . fmap (quadrance . view vel) . view particles


dist :: (Metric v, Floating n) => (n -> n) -> Point v n -> Point v n -> v n
dist f p1 p2 = f (distance p1 p2) *^ signorm (p2 .-. p1)

hooke :: (Metric v, Floating n) => n -> n -> Point v n -> Point v n -> v n
hooke k l = dist $ \ d -> k * (d - l)

coulomb :: (Metric v, Floating n) => n -> Point v n -> Point v n -> v n
coulomb k = dist $ \ d -> -k/ (d*d)