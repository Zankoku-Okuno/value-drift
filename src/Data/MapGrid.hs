{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Data.MapGrid
  ( MapGrid(..)
  , GridIx
  , xyzToIx
  -- accessors/mutators
  , terrainAt
  -- Symbol Kinds
  , TerrainId
  ) where


import Arithmetic.Types (Fin)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Finite.Contiguous (MutPrimArray)
import Data.Finite.Contiguous (Nat)
import Data.SymbolArray (Symbol)


import qualified Arithmetic.Types as Unsafe
import qualified Arithmetic.Unsafe as Unsafe
import qualified Data.Finite.Contiguous as Arr
import qualified GHC.TypeNats as GHC


data TerrainId

data MapGrid s x y z = MapGrid
  { xSize :: !(Nat x)
  , ySize :: !(Nat y)
  , zSize :: !(Nat z)
  , terrain :: MutPrimArray s (Symbol TerrainId) (x GHC.* y GHC.* z)
  }

newtype GridIx x y z = GridIx (Fin (x GHC.* y GHC.* z))

xyzToIx :: MapGrid s x y z -> Fin x -> Fin y -> Fin z -> GridIx x y z
xyzToIx grid
        (Unsafe.Fin (Unsafe.Nat x) _)
        (Unsafe.Fin (Unsafe.Nat y) _)
        (Unsafe.Fin (Unsafe.Nat z) _) =
  GridIx $ Unsafe.Fin (Unsafe.Nat $ x + y * yMul + z * zMul) Unsafe.Lt
  where
  (Unsafe.Nat yMul) = grid.ySize
  (Unsafe.Nat zMul) = grid.zSize

terrainAt :: (PrimMonad m)
  => MapGrid (PrimState m) x y z
  -> GridIx x y z
  -> m (Symbol TerrainId)
terrainAt grid (GridIx ix) = Arr.read grid.terrain ix




toFin :: (Integral a) => Nat n -> a -> Maybe (Fin n)
toFin (Unsafe.Nat n) (fromIntegral -> a)
  | a < n = Just (Unsafe.Fin (Unsafe.Nat a) Unsafe.Lt)
  | otherwise = Nothing
