{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, FlexibleContexts #-}

module Main where

import System.Environment
import Bio.PDB as PDB
import qualified Data.ByteString.Char8 as BS
import Data.List
import Text.Printf
import Data.Vector.V3
import Control.Monad.State(State, modify, get, runState)

type CounterM a = State Int a

counter :: CounterM Int
counter   = do r <- get
               modify (+1)
               return r

runCounterM :: CounterM a -> a
runCounterM = fst . flip runState 1

renumberResidues :: (Iterable s Chain) => s -> s
renumberResidues = imap (\ch -> runCounterM          .
                                imapM forEachResidue $ (ch :: Chain))
  where
    forEachResidue r = do v <- counter
                          return $ r { resSeq = v }

renumberAtoms :: (Iterable s Model) => s -> s
renumberAtoms = imap (\m -> runCounterM $
                            imapM forEachChain (m :: Model))
  where
    forEachChain :: Chain -> CounterM Chain
    forEachChain ch = do newCh <- imapM forEachAtom ch
                         counter
                         return newCh
    forEachAtom :: Atom -> CounterM Atom
    forEachAtom at  = do v <- counter
                         return $ at { atSerial = v }

main = do [inpfname, outfname] <- getArgs
          Just structure <- PDB.parse inpfname
          putStrLn $ show (PDB.numAtoms structure) ++ " atoms."
          let s1 = renumberResidues $ renumberAtoms structure
          PDB.write s1 outfname

