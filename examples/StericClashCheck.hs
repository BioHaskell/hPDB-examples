{-# LANGUAGE OverloadedStrings, DisambiguateRecordFields, ScopedTypeVariables  #-}

module Main where

import qualified System.Environment           as Env(getArgs)
import qualified Data.ByteString.Char8        as BS
import qualified Data.Octree                  as Oct
import           Bio.PDB                      as PDB
import qualified Bio.PDB.Structure.Elements   as PDB(vanDerWaalsRadius)
import qualified Bio.PDB.Structure.Neighbours as PDB

-- TODO: way to hand over Atom object along with its Residue, Chain and Model
-- TODO: make it so that Show gives a "full_id" string by default, e.g. Model, chain, residue id, atom name.
clashCheck s1 s2 = filter (/= []) . Prelude.map clashes $ itfoldr (:) [] s2
  where
    clashes (at :: PDB.Atom) = PDB.findInRadius (PDB.makeOctree s1) (radius + maxRadius) (PDB.coord at)
      where
        radius :: Double = realToFrac $ PDB.vanDerWaalsRadius . PDB.element $ at

-- Bio.PDB.Structure.Elements should export max bound for vdw etc.
-- or a list of known element codes.
maxRadius = 1.6

size ot =  length . Oct.toList $ ot

main = do [input1, input2] <- Env.getArgs
          Just structure1 <- PDB.parse input1
          Just structure2 <- PDB.parse input2
          print $ clashCheck structure1 structure2
          print "Depths:"
          print . Oct.depth . PDB.makeOctree $ structure1
          print . Oct.depth . PDB.makeOctree $ structure2
          print "Sizes:"
          print . size      . PDB.makeOctree $ structure1
          print . size      . PDB.makeOctree $ structure2

