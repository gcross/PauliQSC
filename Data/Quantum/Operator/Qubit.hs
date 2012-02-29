{-# OPTIONS_GHC -funbox-strict-fields #-}

  -- Language extensions {{{

{-# LANGUAGE UnicodeSyntax #-}

-- }}} Language extensions

module Data.Quantum.Operator.Qubit where

-- Imports {{{

import Data.Bits (Bits())
import Data.Monoid (mappend)
import Data.Word

import Data.Quantum.Operator

-- }}} Imports

-- Types {{{

data Qubit α = Qubit -- {{{
    {   qubitX :: !(Operator α)
    ,   qubitZ :: !(Operator α)
    } deriving (Eq,Ord,Show)
-- }}}

-- }}} Types

-- Functions {{{

makeOperatorCommuteWithQubit :: Bits α ⇒ Operator α → Qubit α → Operator α -- {{{
makeOperatorCommuteWithQubit o (Qubit x z) =
    multiplyByIfAntiCommuteWith x z
    .
    multiplyByIfAntiCommuteWith z x
    $
    o
{-# SPECIALIZE INLINE makeOperatorCommuteWithQubit :: Operator Word8 → Qubit Word8 → Operator Word8 #-}
{-# SPECIALIZE INLINE makeOperatorCommuteWithQubit :: Operator Word16 → Qubit Word16 → Operator Word16 #-}
{-# SPECIALIZE INLINE makeOperatorCommuteWithQubit :: Operator Word32 → Qubit Word32 → Operator Word32 #-}
{-# SPECIALIZE INLINE makeOperatorCommuteWithQubit :: Operator Word64 → Qubit Word64 → Operator Word64 #-}
{-# SPECIALIZE INLINE makeOperatorCommuteWithQubit :: Operator Integer → Qubit Integer → Operator Integer #-}
{-# INLINE makeOperatorCommuteWithQubit #-}
-- }}}

multiplyQubitByIfAntiCommuteWith :: Bits α ⇒ Operator α → Operator α → Qubit α → Qubit α -- {{{
multiplyQubitByIfAntiCommuteWith multiplier commuter (Qubit x z) =
    Qubit
        (multiplyByIf (antiCommute commuter x) multiplier x)
        (multiplyByIf (antiCommute commuter z) multiplier z)
{-# SPECIALIZE INLINE multiplyQubitByIfAntiCommuteWith :: Operator Word8 → Operator Word8 → Qubit Word8 → Qubit Word8 #-}
{-# SPECIALIZE INLINE multiplyQubitByIfAntiCommuteWith :: Operator Word16 → Operator Word16 → Qubit Word16 → Qubit Word16 #-}
{-# SPECIALIZE INLINE multiplyQubitByIfAntiCommuteWith :: Operator Word32 → Operator Word32 → Qubit Word32 → Qubit Word32 #-}
{-# SPECIALIZE INLINE multiplyQubitByIfAntiCommuteWith :: Operator Word64 → Operator Word64 → Qubit Word64 → Qubit Word64 #-}
{-# SPECIALIZE INLINE multiplyQubitByIfAntiCommuteWith :: Operator Integer → Operator Integer → Qubit Integer → Qubit Integer #-}
{-# INLINE multiplyQubitByIfAntiCommuteWith #-}
-- }}}

qubitY (Qubit x z) = x `mappend` z  -- {{{
{-# SPECIALIZE INLINE qubitY :: Qubit Word8 → Operator Word8 #-}
{-# SPECIALIZE INLINE qubitY :: Qubit Word16 → Operator Word16 #-}
{-# SPECIALIZE INLINE qubitY :: Qubit Word32 → Operator Word32 #-}
{-# SPECIALIZE INLINE qubitY :: Qubit Word64 → Operator Word64 #-}
{-# SPECIALIZE INLINE qubitY :: Qubit Integer → Operator Integer #-}
{-# INLINE qubitY #-}
-- }}}

-- }}} Functions
