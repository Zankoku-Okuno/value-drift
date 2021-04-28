{-# LANGUAGE TypeFamilies #-}

module Data.Ref
  ( Assignable(..)
  , Ref
  , PrimRef
  , Always
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Kind (Type)
import Data.Primitive (Prim)
import Data.Primitive.Contiguous (Always)
import GHC.Exts (Constraint)

import qualified Data.Primitive.Contiguous as Arr


class Assignable ref where
  type Element ref :: Type -> Constraint
  empty :: (PrimMonad m, Element ref a) => m (ref (PrimState m) a)
  new :: (PrimMonad m, Element ref a) => a -> m (ref (PrimState m) a)
  new x = do
    cell <- empty
    write cell x
    pure cell
  read :: (PrimMonad m, Element ref a) => ref (PrimState m) a -> m a
  write :: (PrimMonad m, Element ref a) => ref (PrimState m) a -> a -> m ()


newtype Ref s a = Ref (Arr.SmallMutableArray s a)

instance Assignable Ref where
  type Element Ref = Always
  empty = Ref <$> Arr.new 1
  read (Ref cell) = Arr.read cell 0
  write (Ref cell) x = Arr.write cell 0 x

newtype PrimRef s a = PrimRef (Arr.MutablePrimArray s a)

instance Assignable PrimRef where
  type Element PrimRef = Prim
  empty = PrimRef <$> Arr.new 1
  read (PrimRef cell) = Arr.read cell 0
  write (PrimRef cell) x = Arr.write cell 0 x
