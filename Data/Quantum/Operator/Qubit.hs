  -- Language extensions {{{

{-# LANGUAGE UnicodeSyntax #-}

-- }}} Language extensions

module Data.Quantum.Operator.Qubit where

-- Imports {{{

import Data.Bits (Bits())
import Data.Monoid (mappend)

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
-- }}}

multiplyQubitByIfAntiCommuteWith :: Bits α ⇒ Operator α → Operator α → Qubit α → Qubit α -- {{{
multiplyQubitByIfAntiCommuteWith multiplier commuter (Qubit x z) =
    Qubit
        (multiplyByIf (antiCommute commuter x) multiplier x)
        (multiplyByIf (antiCommute commuter z) multiplier z)
-- }}}

qubitY (Qubit x z) = x `mappend` z

-- }}} Functions
