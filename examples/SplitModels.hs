{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}

module Main where

import System.Environment
import System.Exit
import System.IO(hPutStrLn, stderr)
--import qualified Bio.PDB.IO as PDBIO
import qualified Data.ByteString.Char8 as BS
--import Bio.PDB.Iterable as It
import Bio.PDB as PDB
import Data.List
import Text.Printf
--import Data.Vector.V3
import qualified Data.Vector as V
import Control.Monad(forM, when)
--import Control.DeepSeq

splitModels aStructure = map mkStructure . V.toList . models $ aStructure
  where mkStructure aModel = aStructure{ models = V.singleton aModel }

usage = do hPutStrLn stderr $ concat [ "Usage: SplitModels <input.pdb> <output_prefix>"
                                     , "Splitting of PDB files with multiple MODEL entries." ]
           exitFailure

main = do lenArgs <- length `fmap` getArgs
          when (lenArgs /= 2) usage
          [inpfname, outfname] <- getArgs
          Just structure <- PDB.parse $ BS.pack inpfname
          let splitted = zip [1..] $ splitModels structure
          forM splitted $ \(num, aStructure) ->
            do let fname = outfname ++ show num ++ ".pdb"
               putStrLn $ "Writing " ++ show fname ++ " with " ++ show (numAtoms aStructure) ++ " atoms."
               PDB.write aStructure $ BS.pack fname
