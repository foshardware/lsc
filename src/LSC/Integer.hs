-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}

module LSC.Integer where

#ifdef GLPK
import Control.Exception (evaluate)
import Control.DeepSeq
import Data.Foldable
import Data.Map (unionWith)
import Data.Vector (Vector, fromList)
import qualified Data.LinearProgram as GLP
#else
import Control.Monad.Free
import Data.Vector (Vector)
#endif



type Coefficient = Rational


linearCombination :: Ord a => [(Coefficient, a)] -> Linear a Coefficient


getSolutionVector :: Ord a => LP a Coefficient -> IO (Maybe (Double, Vector Int))

debugLP :: (Show a, Ord a) => LP a Coefficient -> FilePath -> IO ()


minimize, maximize :: Ord a => Constraint a Coefficient () -> LP a Coefficient


setObjective :: Linear a Coefficient -> Constraint a Coefficient ()

varEq, varGeq, varLeq :: Ord a => a -> Coefficient -> Constraint a Coefficient ()
bounds :: Ord a => a -> Coefficient -> Coefficient -> Constraint a Coefficient ()

geqTo, leqTo :: Ord a => Linear a Coefficient -> Coefficient -> Constraint a Coefficient ()

leq, geq :: Ord a => Linear a Coefficient -> Linear a Coefficient -> Constraint a Coefficient ()


boolean, integer :: Ord a => a -> Constraint a Coefficient ()


#ifdef GLPK

newtype Linear a b = Linear { unLinear :: GLP.LinFunc a b }
  deriving Show

instance (Ord a, Num b) => Semigroup (Linear a b) where
    Linear f <> Linear g = Linear (unionWith (+) f g)

instance (Ord a, Num b) => Monoid (Linear a b) where
    mempty = Linear mempty


type Constraint a b = GLP.LPM a b

type LP = GLP.LP


linearCombination = Linear . GLP.linCombination


getSolutionVector lp
  = do
    result <- GLP.glpSolveVars GLP.mipDefaults lp
    case result of
        (GLP.Success, Just (v, m))
          -> evaluate $ force $ Just (v, fromList $ round <$> toList m)
        _ -> pure Nothing


#ifdef DEBUG

debugLP = flip GLP.writeLP

#else

debugLP _ _ = pure ()

#endif


minimize m = GLP.execLPM $ GLP.setDirection GLP.Min *> m
maximize m = GLP.execLPM $ GLP.setDirection GLP.Max *> m


setObjective = GLP.setObjective . unLinear

varEq  = GLP.varEq
varGeq = GLP.varGeq
varLeq = GLP.varLeq

bounds = GLP.varBds


geqTo = GLP.geqTo . unLinear
leqTo = GLP.leqTo . unLinear

Linear x `geq` Linear y = GLP.geq x y
Linear x `leq` Linear y = GLP.leq x y


boolean = flip GLP.setVarKind GLP.BinVar
integer = flip GLP.setVarKind GLP.IntVar

#else

type Linear a b = ()


data ConstraintF a b c deriving Functor

type Constraint a b = Free (ConstraintF a b)


data LP a b = LP


linearCombination _ = mempty


getSolutionVector _ = pure Nothing

debugLP _ _ = pure ()


maximize _ = LP
minimize _ = LP


setObjective _ = pure ()

varEq  _ _ = pure ()
varGeq _ _ = pure ()
varLeq _ _ = pure ()

geqTo _ _ = pure ()
leqTo _ _ = pure ()
bounds _ _ _ = pure ()

geq _ _ = pure ()
leq _ _ = pure ()


boolean _ = pure ()
integer _ = pure ()

#endif

