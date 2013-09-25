{-# LANGUAGE OverloadedStrings, FlexibleContexts, NoMonomorphismRestriction #-}
module Main(main) where

import System.Environment(getProgName, getArgs)
import System.Exit(exitWith, exitSuccess, ExitCode(ExitSuccess), ExitCode(..))
import System.Console.GetOpt
import Bio.PDB as PDB
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Bio.PDB.Fasta
import Control.Monad(when)
import Data.Foldable(forM_)

data Options = Options { gapped    :: Bool,
                         allModels :: Bool
                       }

showHelp :: IO ()
showHelp =
        do prg <- getProgName
           Prelude.putStrLn $ usageInfo prg options

showVersion = Prelude.putStrLn "Version 0.1" -- TODO: extract version from CABAL declaration

exitAfter function exitCode _ =
  do function
     exitWith exitCode
     return undefined

defaultOptions = Options { gapped    = False
                         , allModels = False
                         }

options  :: [OptDescr (Options -> IO Options)]
options = 
  [Option "V" ["version"]       (NoArg (exitAfter showVersion ExitSuccess ))
           "Print program version.",
   Option "h" ["help"]          (NoArg (exitAfter showHelp    ExitSuccess ))
           "Prints help.",
   Option "g" ["with-gaps"]     (NoArg (\opts -> return $ opts { gapped    = True }))
           "Shows sequence with gaps.",
   Option "a" ["all-models"]    (NoArg (\opts -> return $ opts { allModels = True }))
           "Shows sequences for all MODELs."
  ]

main = do
  args <- getArgs
  let (actions, filenames, opts) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  when (null filenames) $ exitAfter showHelp (ExitFailure 3) undefined
  mapM_ (processFile opts) filenames
  exitSuccess -- TODO: only if no errors!
-- | This program loads a PDB file and outputs FASTA sequences of all chains within

printFastaRecords :: Options -> FilePath -> Structure -> IO ()
printFastaRecords opts fname s = if allModels opts
                                   then
                                     process s
                                   else
                                     case firstModel s of
                                       Nothing -> Prelude.putStrLn $ "No models found within file '" ++ fname ++ "'."
                                       Just m  -> process m
  where
    process = PDB.itfoldM (\() ch -> Prelude.putStrLn $ mkRecord fname ch) ()
    mkRecord = if gapped opts
                 then fastaGappedRecord
                 else fastaRecord 

--printFastaRecords fname s = PDB.itfoldM (\() ch -> Prelude.putStrLn $ fastaRecord fname ch) () s

processFile opts fname = do maybePDB <- PDB.parse fname
                            forM_ maybePDB $ printFastaRecords opts fname

