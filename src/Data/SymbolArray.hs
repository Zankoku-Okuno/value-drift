{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Data.SymbolArray
  ( Symbol
  , Symtab
  , newSymtab
  , intern
  , freeze
  , extern
  ) where

import Prelude hiding (read)

import Control.Monad (void, when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Primitive.Contiguous (Array, MutableArray)
import Data.Ref (Ref, PrimRef)

import qualified Data.Primitive.Contiguous as Arr
import qualified Data.Ref as Ref


-- the for parameter specifies which table is being indexed into
-- the idea is that each (group of related) table(s) gets its own phantom type,
-- which is shared by both the table and its symbols, and they must match for a lookup to compile
newtype Symbol for = Symbol { _unSymbol :: Int }

-- a fully-initialized symtab for use in accessing game data based on numeric ids
-- FIXME I should probly use something a bit more strict/performant than 'Array'
newtype Symtab for a = Symtab { _unTable :: Array a }
-- symtab not yet initialized, or partially-initialized
-- used as part of loading game data into an id-based data structure
data MutSymtab s for a = MutSymtab
  { mutTable :: Ref s (MutableArray s a)
  , nextId :: PrimRef s Int
  }

newSymtab :: forall for a m. (PrimMonad m)
  => Int -- ^ initial size of table ('MutSymtab's will resize themselves as needed)
  -> m (MutSymtab (PrimState m) for a)
newSymtab size0 = do
  table <- Arr.new size0
  mutTable <- Ref.new table
  nextId <- Ref.new 0
  pure MutSymtab { mutTable, nextId }

intern :: PrimMonad m => MutSymtab (PrimState m) for a -> a -> m (Symbol for)
intern symtab v = do
  !newId <- Ref.read symtab.nextId
  Ref.write symtab.nextId (newId + 1)
  !table <- Ref.read symtab.mutTable
  curSize <- Arr.sizeMutable table
  if (curSize <= newId)
  then do
    table' <- Arr.resize table (max 128 $ 2 * curSize)
    Ref.write symtab.mutTable table'
    Arr.write table' newId v
  else
    Arr.write table newId v
  pure $ Symbol newId

freeze :: PrimMonad m => MutSymtab (PrimState m) for a -> m (Symtab for a)
freeze mut = do
  !table <- Ref.read mut.mutTable
  !curSize <- Ref.read mut.nextId
  _unTable <- Arr.freeze table 0 curSize
  pure Symtab{_unTable}

extern :: Symtab for a -> Symbol for -> a
extern tab sym = Arr.index tab._unTable sym._unSymbol


main :: IO ()
main = do
  mt <- newSymtab @() @Int 0
  x <- intern mt 4004
  t <- freeze mt
  print $ extern t x
