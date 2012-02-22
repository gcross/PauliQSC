-- Language extensions {{{

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

-- }}} Language extensions

module Data.Quantum.Operator where

-- Imports {{{

import Control.Applicative (liftA2)

import Data.Bits (Bits((.&.),clearBit,setBit,shiftL,shiftR,testBit,xor))
import Data.Foldable (foldl')
import Data.Function (on)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.List (replicate)
import Data.Monoid (Monoid(..))
import Data.Word

-- }}} Imports

-- Classes {{{

class Commutable α where
    commute :: α → α → Bool
    antiCommute :: α → α → Bool

    commute x y = not (antiCommute x y)
    antiCommute x y = not (commute x y)

-- }}}

-- Types (with instances) {{{

-- Operator {{{
data Operator α = Operator
    {   operatorX :: !α
    ,   operatorZ :: !α
    } deriving (Eq,Show,Read)

instance Bits α ⇒ Monoid (Operator α) where
    mempty = Operator 0 0
    {-# INLINE mempty #-}
    mappend a b = Operator ((xor `on` operatorX) a b) ((xor `on` operatorZ) a b)
    {-# INLINE mappend #-}
    mconcat = liftA2 Operator
        (foldl' xor 0 . map operatorX)
        (foldl' xor 0 . map operatorZ)
    {-# INLINE mconcat #-}

instance Bits α ⇒ Commutable (Operator α) where
    commute a b =
        ( countBits (operatorX a .&. operatorZ b)
        + countBits (operatorX b .&. operatorZ a)
        ) `mod` 2 == 0
-- }}} Operator

-- Pauli {{{
data Pauli = I | X | Z | Y deriving (Eq,Ord,Read,Enum)

instance Monoid Pauli where
instance Show Pauli where -- {{{
    show = (:[]) . pauliToChar
    showList [] s = s
    showList (p:rest) s = pauliToChar p : showList rest s
-- }}}

    mempty = I
    mappend a b = toEnum (fromEnum a `xor` fromEnum b)

instance Commutable Pauli where
    commute I _ = True
    commute _ I = True
    commute x y = x == y
-- Pauli }}}

-- }}} Types

-- Functions {{{

-- commuteAt/antiCommuteAt {{{
commuteAt :: Bits α ⇒ Int → Operator α → Operator α → Bool
commuteAt i a b = not (antiCommuteAt i a b)

antiCommuteAt :: Bits α ⇒ Int → Operator α → Operator α → Bool
antiCommuteAt i a b =
    (testBit (operatorX a) i && testBit (operatorZ b) i) /=
    (testBit (operatorZ a) i && testBit (operatorX b) i)
-- }}}

countBits :: Bits α ⇒ α → Int -- {{{
countBits = go 0
  where
    go accum 0 = accum
    go accum x = go (if x .&. 1 == 0 then accum else accum + 1) (shiftR x 1)
-- }}}

fromPauliList :: Bits α ⇒ [Pauli] → Operator α -- {{{
fromPauliList = go 0 0 0
  where
    go :: Bits α ⇒ Int → α → α → [Pauli] → Operator α
    go i ox oz [] = Operator ox oz
    go i ox oz ((fromEnum → pn):rest) =
        go  (i+1)
            (if pn .&. 1 /= 0 then setBit ox i else ox)
            (if pn .&. 2 /= 0 then setBit oz i else oz)
            rest
-- }}}

getPauliAt :: Bits α ⇒ Int → Operator α → Pauli -- {{{
getPauliAt column (Operator x z) =
    case (testBit x column, testBit z column) of
        (False,False) → I
        (True ,False) → X
        (False,True ) → Z
        (True ,True ) → Y
-- }}}

hasXBit, hasZBit :: Pauli → Bool -- {{{

hasXBit I = False
hasXBit X = True
hasXBit Z = False
hasXBit Y = True

hasZBit I = False
hasZBit X = False
hasZBit Z = True
hasZBit Y = True
-- }}}

maybeFirstNonTrivialColumnOf :: Bits α ⇒ Operator α → Maybe Int -- {{{
maybeFirstNonTrivialColumnOf (Operator 0 0) = Nothing
maybeFirstNonTrivialColumnOf (Operator x z) = Just (go 0 x z)
  where
    go :: Bits α ⇒ Int → α → α → Int
    go i x z
      | x .&. 1 /= 0 = i
      | z .&. 1 /= 0 = i
      | otherwise    = go (i+1) (shiftR x 1) (shiftR z 1)
-- }}}

multiplyByIf :: Bits α ⇒ Bool → Operator α → Operator α → Operator α -- {{{
multiplyByIf False a b = b
multiplyByIf True a b = b `mappend` a
-- }}}

multiplyByIfAntiCommuteAt :: Bits α ⇒ Int → Operator α → Operator α → Operator α -- {{{
multiplyByIfAntiCommuteAt column a b = multiplyByIf (antiCommuteAt column a b) a b
-- }}}

nonTrivialAt :: Bits α ⇒ Int → Operator α → Bool -- {{{
nonTrivialAt i (Operator x z) = testBit x i || testBit z i
-- }}}

setPauliAt :: Bits α ⇒ Int → Pauli → Operator α → Operator α -- {{{
setPauliAt column I (Operator x z) = Operator (clearBit x column) (clearBit z column)
setPauliAt column X (Operator x z) = Operator (setBit   x column) (clearBit z column)
setPauliAt column Z (Operator x z) = Operator (clearBit x column) (setBit   z column)
setPauliAt column Y (Operator x z) = Operator (setBit   x column) (setBit   z column)
-- }}}

toPauliList :: (Integral α, Bits α) ⇒ Int → Operator α → [Pauli] -- {{{
toPauliList 0 _ = []
toPauliList n (Operator 0 0) = replicate n I
toPauliList n (Operator x z) = toEnum (fromIntegral (x .&. 1 + shiftL (z .&. 1) 1)) : toPauliList (n-1) (Operator (shiftR x 1) (shiftR z 1))
-- }}}

-- }}}
