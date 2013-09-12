{-# LANGUAGE OverloadedStrings  #-}

module Main where

import System.Environment
import Bio.PDB.IO as PDBIO
import Data.ByteString.Char8 as BS

main = do [inpfname, outfname] <- getArgs
          Just structure <- PDBIO.parse inpfname
          PDBIO.write structure outfname
