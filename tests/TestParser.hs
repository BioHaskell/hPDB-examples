{-# LANGUAGE OverloadedStrings, FlexibleContexts, NoMonomorphismRestriction #-}
module Main(main) where

import Control.Concurrent(getNumCapabilities)
import Data.Time(diffUTCTime, getCurrentTime)
import System.Process
import Control.Exception(throw)
import System.Environment(getProgName, getArgs)
import System.Exit(exitWith, exitSuccess, ExitCode(ExitSuccess), ExitCode(..))
import System.Directory(doesFileExist)
import qualified Bio.PDB as PDB(parse, numResidues)
import Data.Iterable
import Control.Monad.IfElse(whenM)
import Control.Monad(unless)

cmd cmdLine = do exitCode <- system cmdLine
                 unless (exitCode == ExitSuccess) $ throw exitCode

inputPath="tests/1htq.ent"
gzPath=inputPath ++ ".gz"

assureInputPath = whenM (not `fmap` doesFileExist inputPath) $ do
  cmd $ "wget http://rcsb.org/pdb/files/1htq.pdb.gz -O " ++ inputPath ++ ".gz"
  cmd $ "gzip --decompress " ++ gzPath

timeIt io = do
  cpus <- getNumCapabilities
  putStrLn $ "Running with " ++ show cpus ++ " threads."
  t1 <- getCurrentTime
  r <- io
  t2 <- getCurrentTime
  print $ t2 `diffUTCTime` t1
  return r

main = do
  assureInputPath
  timeIt $ do
    Just structure <- PDB.parse inputPath
    print $ PDB.numResidues structure
