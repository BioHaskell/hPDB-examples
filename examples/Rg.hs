{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, BangPatterns, FlexibleContexts #-}
module Main(main) where

import Bio.PDB as PDB
import System.Environment(getArgs)
import System.IO.Unsafe(unsafePerformIO)
import System.IO(stderr, stdout, hFlush)
import Data.ByteString.Char8 as BS
import Control.DeepSeq
import Text.Printf
import System.Exit
--import Control.Parallel

import Bio.PDB.Structure.Elements(atomicMass, assignElement)

main = do args <- getArgs
          mapM_ readAndComputeRg args
          exitSuccess

readAndComputeRg filename =
  do Just structure <- PDB.parse filename
     BS.putStr "Parsed "
     hFlush stdout
     BS.putStr . BS.pack . show $ numAtoms structure
     hFlush stdout
     rnf structure `seq` BS.putStrLn " atoms!"
     rg <- return $! radiusOfGyration structure
     printf "%s: %.2f\n" filename rg 
     return rg

-- TODO: consider mass of an atom
-- TODO: use Data.Tensor or some other standard notation for 3D vectors
-- TODO: check if there is nice library with quaternions and rotation matrices for these
radiusOfGyration :: Iterable Structure Atom => Structure -> Double
radiusOfGyration structure = avgDistDev
  where
    -- (c -> b -> c) -> c -> a -> c
    avgDistDev = sqrt (itfoldl' addDistDev 0.0 structure/totalMass)
    addDistDev !total at = total + vnorm (coord at - center)**2 * atMass at
    atMass :: Atom -> Double
    atMass at = atomicMass $ case assignElement at of
                               ""        -> "C"
                               otherwise -> otherwise
    center = v |* (1.0/totalMass)
      where v = itfoldl' addCoord nullVector structure
    nullVector              = fromInteger 0
    addCoord v (at :: Atom) = v + coord at |* atMass at
    counter  x (at :: Atom) = x + atMass at 
    totalMass               = itfoldl' counter 0.0 structure


