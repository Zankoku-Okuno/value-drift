{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Finite.Contiguous
  ( Contiguous(..)
  , Array
  , MutArray
  , PrimArray
  , MutPrimArray
  -- re-exports
  , Nat, Fin, WithNat(..)
  ) where

import Arithmetic.Types (Nat, WithNat(..), Fin, type (<), type (<=))
import Control.Monad.Identity (Identity(..), runIdentity)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Kind (Type)
import Data.Primitive (Prim)
import Data.Primitive.Contiguous (Always)
import GHC.Exts (Constraint)

import qualified Arithmetic.Fin as Fin
import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Unsafe as Unsafe
import qualified Data.Primitive.Contiguous as Arr
import qualified GHC.TypeNats as GHC

-- | Two numbers, one less than the other, and both drawn from the finite set of
-- elements 'Fin n'.
data Slice (n :: GHC.Nat) = forall l m. Slice
  { lower :: !(Nat l)
  , upper :: !(Nat m)
  , proofs :: !(KnownSlice l m n)
  }

data KnownSlice (l :: GHC.Nat) (m :: GHC.Nat) (n :: GHC.Nat) = KnownSlice
  { proofLower :: !(l <= m)
  , proofUpper :: !(m < n)
  }

withSlice :: Slice n -> (forall d. Nat d -> f d) -> WithNat f
withSlice slice f = runIdentity $ withSliceF slice (Identity . f)

withSliceF :: (Functor m)
  => Slice n
  -> (forall d. Nat d -> m (f d))
  -> m (WithNat f)
withSliceF Slice{lower,upper,proofs} f =
  WithNat theDiff <$> (f theDiff)
  where
  theDiff = Unsafe.Nat $ diff (proofLower proofs) upper lower
  diff :: (b <= a) -> Nat a -> Nat b -> Int
  diff _ a b = Unsafe.getNat a - Unsafe.getNat b


-- type family Size slice where
--   Size (Slice l u _ _) = Nat (u GHC.- l)

demote :: Slice n -> (Int, Int)
demote Slice{lower, upper} = (Nat.demote lower, Nat.demote upper)


class Contiguous (arr :: Type -> GHC.Nat -> Type) where
  -- | The Mutable counterpart to the array.
  type family Mutable arr = (r :: Type -> Type -> GHC.Nat -> Type) | r -> arr
  -- | The constraint needed to store elements in the array.
  type family Element arr :: Type -> Constraint

  -- | Allocate a new mutable array of the given size.
  new :: forall n a m. (PrimMonad m, Element arr a)
    => Nat n
    -> m (Mutable arr (PrimState m) a n)
  -- | @'replicateMut' n x@ is a mutable array of length @n@ with @x@ the value of every element.
  replicateMut :: (PrimMonad m, Element arr a)
    => Nat n
    -> a
    -> m (Mutable arr (PrimState m) a n)

  -- | Index into an array at the given index.
  index :: Element arr a => arr a n -> Fin n -> a
  -- | Read a mutable array at the given index.
  read :: (PrimMonad m, Element arr a) => Mutable arr (PrimState m) a n -> Fin n -> m a
  -- | Write to a mutable array at the given index.
  write :: (PrimMonad m, Element arr a) => Mutable arr (PrimState m) a n -> Fin n -> a -> m ()

  -- | Turn a mutable array into an immutable one without copying.
  --   The mutable array should not be used after this conversion.
  unsafeFreeze :: PrimMonad m
    => Mutable arr (PrimState m) a n
    -> m (arr a n)
  -- | Turn a mutable array into an immutable one with copying, using a slice of the mutable array.
  freeze :: (PrimMonad m, Element arr a)
    => Mutable arr (PrimState m) a n
    -> Slice n
    -> m (arr a n)
  -- | Copy a slice of an immutable array into a new mutable array.
  thaw :: (PrimMonad m, Element arr a)
    => arr a n
    -> Slice n
    -> m (Mutable arr (PrimState m) a n)

  -- | Create a new immutable array by copyin a slice of an existing one.
  clone :: (Element arr a)
    => arr a n
    -> Slice n
    -> WithNat (arr a)
-- | Create a new mutable array by copying a slice of an existing one.
  cloneMut :: (PrimMonad m, Element arr a)
    => Mutable arr (PrimState m) a n
    -> Slice n
    -> m (WithNat (Mutable arr (PrimState m) a))
  -- -- | Copy a slice of an immutable array into a mutable array.
  -- copy :: (PrimMonad m, Element arr b)
  --   => Mutable arr (PrimState m) b -- ^ destination array
  --   -> Int -- ^ offset into destination array
  --   -> arr b -- ^ source array
  --   -> Int -- ^ offset into source array
  --   -> Int -- ^ number of elements to copy
  --   -> m ()
  -- -- | Copy a slice of a mutable array into another mutable array.
  -- --   In the case that the destination and source arrays are the
  -- --   same, the regions may overlap.
  -- copyMutable :: (PrimMonad m, Element arr b)
  --   => Mutable arr (PrimState m) b -- ^ destination array
  --   -> Int -- ^ offset into destination array
  --   -> Mutable arr (PrimState m) b -- ^ source array
  --   -> Int -- ^ offset into source array
  --   -> Int -- ^ number of elements to copy
  --   -> m ()

newtype Array a (n :: GHC.Nat) = Array (Arr.Array a)

newtype MutArray s a (n :: GHC.Nat) = MutArray (Arr.MutableArray s a)

instance Contiguous Array where
  type Mutable Array = MutArray
  type Element Array = Always

  new n = MutArray <$> Arr.new (Nat.demote n)
  replicateMut n x = MutArray <$> Arr.replicateMutable (Nat.demote n) x

  index (Array arr) n = Arr.index arr (Fin.demote n)
  read (MutArray arr) n = Arr.read arr (Fin.demote n)
  write (MutArray arr) n x = Arr.write arr (Fin.demote n) x

  unsafeFreeze (MutArray arr) = Array <$> Arr.unsafeFreeze arr
  freeze (MutArray arr) (demote -> (l, u)) = Array <$> Arr.freeze arr l u
  thaw (Array arr) (demote -> (l, u)) = MutArray <$> Arr.thaw arr l u

  clone (Array arr) slice@(demote -> (l, u)) = withSlice slice $ \_ ->
    Array $ Arr.clone arr l u
  cloneMut (MutArray arr) slice@(demote -> (l, u)) = withSliceF slice $ \_ ->
    MutArray <$> Arr.cloneMutable arr l u


newtype PrimArray a (n :: GHC.Nat) = PrimArray (Arr.PrimArray a)

newtype MutPrimArray s a (n :: GHC.Nat) = MutPrimArray (Arr.MutablePrimArray s a)

instance Contiguous PrimArray where
  type Mutable PrimArray = MutPrimArray
  type Element PrimArray = Prim

  new n = MutPrimArray <$> Arr.new (Nat.demote n)
  replicateMut n x = MutPrimArray <$> Arr.replicateMutable (Nat.demote n) x

  index (PrimArray arr) n = Arr.index arr (Fin.demote n)
  read (MutPrimArray arr) n = Arr.read arr (Fin.demote n)
  write (MutPrimArray arr) n x = Arr.write arr (Fin.demote n) x

  unsafeFreeze (MutPrimArray arr) = PrimArray <$> Arr.unsafeFreeze arr
  freeze (MutPrimArray arr) (demote -> (l, u)) = PrimArray <$> Arr.freeze arr l u
  thaw (PrimArray arr) (demote -> (l, u)) = MutPrimArray <$> Arr.thaw arr l u

  clone (PrimArray arr) slice@(demote -> (l, u)) = withSlice slice $ \_ ->
    PrimArray $ Arr.clone arr l u
  cloneMut (MutPrimArray arr) slice@(demote -> (l, u)) = withSliceF slice $ \_ ->
    MutPrimArray <$> Arr.cloneMutable arr l u
