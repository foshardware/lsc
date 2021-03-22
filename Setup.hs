-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE TemplateHaskell #-}

import Control.Exception
import Control.Monad

import Data.FileEmbed

import Distribution.Simple

import System.IO
import System.Environment
import System.Process



status :: FilePath
status = $(strToExp =<< makeRelativeToProject ".status")



main :: IO ()
main = do
  checkGitTree
  defaultMain



checkGitTree :: IO ()
checkGitTree = do
  writeFile status "cabal build"
  git ["status", "--short"] $ \ files -> do
    if null files
    then git ["rev-parse", "HEAD"] $ \ commit -> do
      unless (length (take 256 commit) < 4)
        $ writeFile status $ take 256 commit
    else writeFile status "dirty"



git :: [String] -> (String -> IO ()) -> IO ()
git xs sink = withCreateProcess
  (proc "git" xs) { std_out = CreatePipe }
  (\ _ (Just i) _ _ -> sink =<< hGetContents i)
  `catch` \ (SomeException e) -> hPutStrLn stderr (show e)

