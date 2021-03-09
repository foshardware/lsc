-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LSC.Integer where

#ifdef GLPK
import Control.Exception (evaluate)
import Control.DeepSeq
import Data.Foldable
import Data.Map (unionWith)
import Data.Vector (Vector, fromListN)
import qualified Data.LinearProgram as GLP
#else
import Data.Vector (Vector)
#endif


type Q = Rational


linearCombination :: Ord a => [(Q, a)] -> LinFunc a Q


getSolutionVector :: Ord a => LP a Q -> IO (Maybe (Double, Vector Int))

debugLP :: (Show a, Ord a) => LP a Q -> FilePath -> IO ()


minimize, maximize :: Ord a => LPM a Q () -> LP a Q


setObjective :: LinFunc a Q -> LPM a Q ()

varEq, varGeq, varLeq :: Ord a => a -> Q -> LPM a Q ()
bounds :: Ord a => a -> Q -> Q -> LPM a Q ()

geqTo, leqTo :: Ord a => LinFunc a Q -> Q -> LPM a Q ()

leq, geq :: Ord a => LinFunc a Q -> LinFunc a Q -> LPM a Q ()


boolean, integer :: Ord a => a -> LPM a Q ()


#ifdef GLPK

newtype LinFunc a b = LinFunc { unLinFunc :: GLP.LinFunc a b }
  deriving Show

instance (Ord a, Num b) => Semigroup (LinFunc a b) where
    LinFunc f <> LinFunc g = LinFunc (unionWith (+) f g)

instance (Ord a, Num b) => Monoid (LinFunc a b) where
    mempty = LinFunc mempty
    mappend = (<>)


type LPM a b = GLP.LPM a b

type LP = GLP.LP


linearCombination = LinFunc . GLP.linCombination


getSolutionVector lp
  = do
    result <- GLP.glpSolveVars GLP.mipDefaults lp
    case result of
        (GLP.Success, Just (v, m))
          -> evaluate $ force $ Just (v, round <$> fromListN (length m) (toList m))
        _ -> pure Nothing


#ifdef DEBUG

debugLP = flip GLP.writeLP

#else

debugLP _ _ = pure ()

#endif


minimize m = GLP.execLPM $ GLP.setDirection GLP.Min *> m
maximize m = GLP.execLPM $ GLP.setDirection GLP.Max *> m


setObjective = GLP.setObjective . unLinFunc

varEq  = GLP.varEq
varGeq = GLP.varGeq
varLeq = GLP.varLeq

bounds = GLP.varBds


geqTo = GLP.geqTo . unLinFunc
leqTo = GLP.leqTo . unLinFunc

geq (LinFunc x) (LinFunc y) = GLP.geq x y
leq (LinFunc x) (LinFunc y) = GLP.leq x y


boolean = flip GLP.setVarKind GLP.BinVar
integer = flip GLP.setVarKind GLP.IntVar

#else

data LinFunc a b = LinFunc
  deriving Show

instance (Ord a, Num b) => Semigroup (LinFunc a b) where
    _ <> _ = LinFunc

instance (Ord a, Num b) => Monoid (LinFunc a b) where
    mempty = LinFunc
    mappend = (<>)



data LPM a b c = LPM

instance Functor (LPM a b) where
    fmap _ _ = LPM

instance Applicative (LPM a b) where
    pure = const LPM
    _ <*> _ = LPM

instance Monad (LPM a b) where
    return = pure
    _ >>= _ = LPM


data LP a b = LP


linearCombination = const LinFunc


getSolutionVector _ = pure Nothing


debugLP _ _ = pure ()


maximize = const LP
minimize = const LP


setObjective = const LPM

varEq  _ = const LPM
varGeq _ = const LPM
varLeq _ = const LPM

geqTo _ = const LPM
leqTo _ = const LPM
bounds _ _ = const LPM

geq _ _ = LPM
leq _ _ = LPM


boolean = const LPM
integer = const LPM

#endif

