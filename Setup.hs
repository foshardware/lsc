-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

import Distribution.Simple

import System.IO
import System.Environment
import System.Process



main :: IO ()
main = do
  checkGitTree
  defaultMain



checkGitTree :: IO ()
checkGitTree = do
  git ["status", "--short"] $ \ h -> do
    files <- hGetContents h
    if null files
    then git ["rev-parse", "HEAD"] $ \ i -> do
         commit <- hGetContents i
         writeFile ".status" . ("commit " ++) . take 40 $ commit
    else writeFile ".status" "dirty"



git :: [String] -> (Handle -> IO ()) -> IO ()
git xs p = withCreateProcess (proc "git" xs) { std_out = CreatePipe } (\ _ (Just i) _ _ -> p i)

