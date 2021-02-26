-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE CPP #-}

module LSC.Integer where

#ifdef GLPK
import Control.Exception (evaluate)
import Control.DeepSeq
import Data.Vector (Vector, fromListN)
import Data.Foldable
import qualified Data.LinearProgram as GLP
#else
import Data.Vector (Vector)
#endif



linearCombination :: Ord a => [(Int, a)] -> LinFunc a Int


getSolutionVector :: Ord a => LP a Int -> IO (Maybe (Int, Vector Int))


minimize, maximize :: Ord a => LPM a Int () -> LP a Int


setObjective :: LinFunc a Int -> LPM a Int ()

varGeq :: Ord a => a -> Int -> LPM a Int ()

leqTo :: Ord a => LinFunc a Int -> Int -> LPM a Int ()


boolean, integer :: Ord a => a -> LPM a Int ()


#ifdef GLPK

type LinFunc = GLP.LinFunc

type LPM a b = GLP.LPM a b

type LP = GLP.LP


linearCombination = GLP.linCombination


getSolutionVector lp
  = do
    result <- GLP.glpSolveVars GLP.mipDefaults lp
    case result of
        (GLP.Success, Just (v, m))
          -> evaluate $ force $ Just (round v, round <$> fromListN (length m) (toList m))
        _ -> pure Nothing


minimize m = GLP.execLPM $ GLP.setDirection GLP.Min *> m
maximize m = GLP.execLPM $ GLP.setDirection GLP.Max *> m


setObjective = GLP.setObjective

varGeq = GLP.varGeq

leqTo = GLP.leqTo


boolean = flip GLP.setVarKind GLP.BinVar
integer = flip GLP.setVarKind GLP.IntVar

#else

data LinFunc a b = LinFunc


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


maximize = const LP
minimize = const LP


setObjective = const LPM

varGeq _ = const LPM

leqTo _ = const LPM


boolean = const LPM
integer = const LPM

#endif

