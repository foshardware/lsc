
module LSC.CellFlipping where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import Data.Foldable
import Data.Text (unpack)
import Data.Vector (slice, filter, zipWith)

import Prelude hiding (filter, zipWith)

import LSC.Component
import LSC.Integer
import LSC.Types



cellFlipping :: NetGraph -> LSC NetGraph
cellFlipping top = do

    info ["Cell flipping"]

    result <- liftIO . getSolutionVector . obtainProgram $ top

    case result of

        Nothing -> do

            warning ["No result in cell flipping"]
            pure top

        Just (objective, vector) -> do

            let solution = toEnum <$> views gates (slice 0 . length) top vector

            info
              [ "Cell flipping objective: " ++ show objective
              , "Number of cells flipped: " ++ show (length $ filter (/= N) solution)
              ]

            when (any (/= N) solution) $ debug $ "Cells flipped:" : take 800
              [ show i ++ " - " ++ views identifier unpack g ++ ": " ++ show o
              | g <- toList $ top ^. gates
              , let i = g ^. number
              , let o = solution ^. ix i <> g ^. space . orientation
              , solution ^. ix i /= N
              ]

            pure
              $ top &~ do
                gates %= zipWith (over space . (<>~) orientation) solution



data Symbol
  = G Number
  | Lambda Int
  deriving (Eq, Ord)


obtainProgram :: NetGraph -> LP Symbol Int
obtainProgram _ = maximize $ do

    pure ()


