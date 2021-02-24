-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

module LSC.Logger where

import Control.Applicative
import Control.Concurrent
import Data.Char
import Data.Time.Format
import Data.Time.LocalTime
import System.Console.Concurrent
import System.Console.Pretty
import System.IO



data LogLevel
  = Silent
  | Error
  | Warning
  | Info
  | Debug
  deriving (Eq, Ord, Enum, Show)



levelColor :: Bool -> LogLevel -> String -> String
levelColor False _ = id
levelColor _    Info = color Green
levelColor _ Warning = color Yellow
levelColor _   Error = color Magenta
levelColor _ _ = id


timestamp :: IO String
timestamp = formatTime defaultTimeLocale "[%F %X]" <$> getZonedTime


levelString :: LogLevel -> String
levelString = liftA2 (:) (toLower . head) tail . show



logStderr :: LogLevel -> [String] -> IO ()
logStderr k (x : xs) = do

    time <- timestamp
    pretty <- hIsTerminalDevice stderr
    thread <- myThreadId

    let indent = mappend $ unwords [" ", ' ' <$ time, ' ' <$ levelString k]
        tag = replicate (maximum (length <$> x : xs) - length x + 4) ' ' ++ show thread
        lvl = levelColor pretty k $ levelString k

    errorConcurrent
      $ unlines
      $ unwords ([time, lvl ++ ":", x] ++ [tag | k /= Info])
      : map indent xs

logStderr _ _ = pure ()

