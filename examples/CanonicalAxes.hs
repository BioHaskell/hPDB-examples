{-# LANGUAGE OverloadedStrings, BangPatterns, FlexibleInstances,UndecidableInstances  #-}

module Main where

import Control.Monad(when)
import System.IO
import System.Environment
import System.Exit(exitFailure, exitSuccess)
import qualified Bio.PDB.IO as PDBIO
import qualified Data.ByteString.Char8 as BS
import Bio.PDB.Iterable as It
import Bio.PDB.Structure as PDBS
import Bio.PDB.Structure.Vector
import Data.List
import Text.Printf

ifoldrPairs fred fpair e s = pairs
  where
    pairs' a cont = It.ifoldr (\at r -> (at `fpair` a) `fred` r) cont (s :: Structure)
    pairs         = It.ifoldr (\at r -> pairs' at r) e                (s :: Structure)

ifoldPairs fred fpair e s = pairs
  where
    pairs' a cont = It.ifoldl' (\r at -> (at `fpair` a) `fred` r) cont (s :: Structure)
    pairs         = It.ifoldl' (\r at -> pairs' at r) e                (s :: Structure)

-- | findAxes finds all three principal axes so that dimensions are ordered.
-- TODO: find a way to loop v1, v2, v3 assignment
findAxes structure = let v1    = findLongestOrthogonalVector [            ] structure
                         axis1 = vnormalise v1
                         dim1  = vnorm v1
                         v2    = findLongestOrthogonalVector [axis1       ] structure
                         axis2 = vnormalise v2
                         dim2  = vnorm v2
                         v3    = findLongestOrthogonalVector [axis1, axis2] structure
                         axis3 = vnormalise v3
                         dim3  = vnorm v3
                     in dim1 `seq` dim2 `seq` dim3 `seq` ([dim1, dim2, dim3], [axis1, axis2, axis3])
  where
    findLongestOrthogonalVector axes = ifoldPairs pickMaxDist (atDistPerpend axes) nullVector 
    nullVector          = fromInteger 0
    atDistPerpend axes !a1 !a2 = vperpends ((coord a1) - (coord a2)) axes
    pickMaxDist !v1 !v2 = if vnorm v1 > vnorm v2 then v1 else v2
    

{-center :: Structure -> (Double, Double, Double)
center s = (realToFrac sx, realToFrac sy, realToFrac sz)
  where
    
    result  = It.ifoldr 
    maxFst (a, b) (c, d) = if a >= c then (a, b) else (c, d)
    coords  = [It.ifoldr (\at r -> coord (at :: PDBS.Atom) : r) [] (s :: Structure)]
-}


main = do args <- getArgs
          when (length args /= 2) $ do hPutStrLn stderr "USAGE: CanonicalAxes <input.pdb> <output.pdb>"
                                       exitFailure
          let [inpfname, outfname] = args
          Just structure <- PDBIO.parse inpfname
          let ([d1, d2, d3], axes@[yaxis, xaxis, zaxis]) = findAxes structure
          printf "Dimensions: %.2f %.2f %.2f\n" d1 d2 d3
          putStr "Axis 1 (Y): "
          print yaxis
          putStr "Axis 2 (X): "
          print xaxis
          putStr "Axis 3 (Z): "
          print zaxis
          {- This requires implementation of geometric transforms
          let xform = axesToTransform axes
          let s2    = applyTransform xform s1
          PDBIO.write s2 $ BS.pack outfname
          -}
          exitSuccess
