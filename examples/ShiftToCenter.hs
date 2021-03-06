{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import System.Environment
import Bio.PDB as PDB
import qualified Data.ByteString.Char8 as BS
import Data.List
import Text.Printf
import Linear.V3

center :: PDB.Structure -> V3 Double
center s = avgv
  where
    sumv = itfoldl' addCoord 0 (s :: PDB.Structure)
    n = realToFrac . fromIntegral . PDB.numAtoms $ s
    addCoord v (PDB.Atom { coord   = c }) = v+c
    avgv = (1/n) *| sumv

shift v = PDB.itmap subCoord
  where
    subCoord (at@Atom { coord = c }) = at { coord = c - v }

main = do [inpfname, outfname] <- getArgs
          Just structure <- PDB.parse inpfname
          let c@(V3 x y z) = center structure
          printf "Center %.2f %.2f %.2f\n" x y z
          putStrLn $ show (PDB.numAtoms structure) ++ " atoms."
          let s1 = shift c structure
          PDB.write s1 outfname
