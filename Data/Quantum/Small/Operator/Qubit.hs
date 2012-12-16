-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}} Language extensions

module Data.Quantum.Small.Operator.Qubit where

-- Imports {{{
import Data.Bits (Bits())
import Data.Monoid (mappend)
import Data.Word

import Data.Quantum.Small.Operator
-- }}}

-- Types {{{

data Qubit = Qubit -- {{{
    {   qubitX :: {-# UNPACK #-} !Operator
    ,   qubitZ :: {-# UNPACK #-} !Operator
    } deriving (Eq,Ord,Show)
-- }}}

-- }}}

-- Functions {{{

makeOperatorCommuteWithQubit :: Operator → Qubit → Operator -- {{{
makeOperatorCommuteWithQubit o (Qubit x z) =
    multiplyByIfAntiCommuteWith x z
    .
    multiplyByIfAntiCommuteWith z x
    $
    o
-- }}}

multiplyQubitByIfAntiCommuteWith :: Operator → Operator → Qubit → Qubit -- {{{
multiplyQubitByIfAntiCommuteWith multiplier commuter (Qubit x z) =
    Qubit
        (multiplyByIf (antiCommute commuter x) multiplier x)
        (multiplyByIf (antiCommute commuter z) multiplier z)
-- }}}

qubitY (Qubit x z) = x `mappend` z

-- }}} Functions
