{-# LANGUAGE OverloadedStrings, DisambiguateRecordFields, ScopedTypeVariables  #-}

module Main where

import qualified System.Environment as Env
import qualified Data.ByteString.Char8 as BS
import qualified Data.Octree as Oct
import Bio.PDB               as PDB
import qualified Bio.PDB.Structure.Elements as Elt

-- TODO: way to hand over Atom object along with its Residue, Chain and Model
-- TODO: make it so that Show gives a "full_id" string by default, e.g. Model, chain, residue id, atom name.
clashCheck s1 s2 = filter (/= []) . Prelude.map clashes $ itfoldr (:) [] s2
  where
    clashes (at :: PDB.Atom) = Oct.withinRange ot (radius + maxRadius) (v3v $ PDB.coord at)
      where
        radius :: Double = realToFrac $ Elt.vanDerWaalsRadius . PDB.element $ at
    ot :: Oct.Octree (Int, Double)
    ot = makeOctree s1

extract :: PDB.Atom -> (Oct.Vector3, (Int, Double))
extract (PDB.Atom { coord    = cvec,
                    atSerial = ser ,
                    element  = elt }) = (v3v cvec,
                                         (ser, realToFrac vdw))
  where
    vdw    = Elt.vanDerWaalsRadius elt

makeOctree structure = Oct.fromList . Prelude.map extract . itfoldr (:) [] $ structure

-- Bio.PDB.Structure.Elements should export max bound for vdw etc.
-- or a list of known element codes.
maxRadius = 1.6

v3v = id

size ot =  length . Oct.toList $ ot

main = do [input1, input2] <- Env.getArgs
          Just structure1 <- PDB.parse input1
          Just structure2 <- PDB.parse input2
          print $ clashCheck structure1 structure2
          print "Depths:"
          print . Oct.depth . makeOctree $ structure1
          print . Oct.depth . makeOctree $ structure2
          print "Sizes:"
          print . size      . makeOctree $ structure1
          print . size      . makeOctree $ structure2

