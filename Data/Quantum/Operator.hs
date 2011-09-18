-- @+leo-ver=5-thin
-- @+node:gcross.20110724213035.1123: * @file Data/Quantum/Operator.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20110724213035.1160: ** << Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- @-<< Language extensions >>

module Data.Quantum.Operator where

-- @+<< Import needed modules >>
-- @+node:gcross.20110724213035.1161: ** << Import needed modules >>
import Control.Applicative (liftA2)

import Data.Bits (Bits((.&.),setBit,shiftL,shiftR,testBit,xor))
import Data.Foldable (foldl') -- ' (the quote is a dumb syntax highlighting bug workaround)
import Data.Function (on)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.List (replicate)
import Data.Monoid (Monoid(..))
import Data.Word
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20110918102335.1199: ** Classes
-- @+node:gcross.20110918102335.1200: *3* Commutable
class Commutable α where
    commute :: α → α → Bool
    antiCommute :: α → α → Bool

    commute x y = not (antiCommute x y)
    antiCommute x y = not (commute x y)
-- @+node:gcross.20110724213035.1162: ** Types (with instances)
-- @+node:gcross.20110724213035.1163: *3* Operator
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
        (foldl' xor 0 . map operatorX) -- '
        (foldl' xor 0 . map operatorZ) -- '
    {-# INLINE mconcat #-}

instance Bits α ⇒ Commutable (Operator α) where
    commute a b =
        ( countBits (operatorX a .&. operatorZ b)
        + countBits (operatorX b .&. operatorZ a)
        ) `mod` 2 == 0
-- @+node:gcross.20110724213035.1197: *3* Pauli
data Pauli = I | X | Z | Y deriving (Eq,Show,Read,Enum)

instance Monoid Pauli where
    mempty = I
    mappend a b = toEnum (fromEnum a `xor` fromEnum b)

instance Commutable Pauli where
    commute I _ = True
    commute _ I = True
    commute x y = x == y
-- @+node:gcross.20110724213035.1164: ** Functions
-- @+node:gcross.20110911234057.1157: *3* commuteAt/antiCommuteAt
commuteAt :: Bits α ⇒ Int → Operator α → Operator α → Bool
commuteAt i a b = not (antiCommuteAt i a b)

antiCommuteAt :: Bits α ⇒ Int → Operator α → Operator α → Bool
antiCommuteAt i a b =
    (testBit (operatorX a) i && testBit (operatorZ b) i) /=
    (testBit (operatorZ a) i && testBit (operatorX b) i)
-- @+node:gcross.20110724213035.1175: *3* countBits
countBits :: Bits α ⇒ α → Int
countBits = go 0
  where
    go accum 0 = accum
    go accum x = go (if x .&. 1 == 0 then accum else accum + 1) (shiftR x 1)
-- @+node:gcross.20110724213035.1196: *3* fromPauliList
fromPauliList :: Bits α ⇒ [Pauli] → Operator α
fromPauliList = go 0 0 0
  where
    go :: Bits α ⇒ Int → α → α → [Pauli] → Operator α
    go i ox oz [] = Operator ox oz
    go i ox oz ((fromEnum → pn):rest) =
        go  (i+1)
            (if pn .&. 1 /= 0 then setBit ox i else ox)
            (if pn .&. 2 /= 0 then setBit oz i else oz)
            rest
-- @+node:gcross.20110911234057.1165: *3* maybeFirstNonTrivialColumnOf
maybeFirstNonTrivialColumnOf :: Bits α ⇒ Operator α → Maybe Int
maybeFirstNonTrivialColumnOf (Operator 0 0) = Nothing
maybeFirstNonTrivialColumnOf (Operator x z) = Just (go 0 x z)
  where
    go :: Bits α ⇒ Int → α → α → Int
    go i x z
      | x .&. 1 /= 0 = i
      | z .&. 1 /= 0 = i
      | otherwise    = go (i+1) (shiftR x 1) (shiftR z 1)
-- @+node:gcross.20110911234057.1160: *3* multiplyByIf
multiplyByIf :: Bits α ⇒ Bool → Operator α → Operator α → Operator α
multiplyByIf False a b = b
multiplyByIf True a b = b `mappend` a
-- @+node:gcross.20110911234057.1162: *3* multiplyByIfAntiCommuteAt
multiplyByIfAntiCommuteAt :: Bits α ⇒ Int → Operator α → Operator α → Operator α
multiplyByIfAntiCommuteAt column a b = multiplyByIf (antiCommuteAt column a b) a b
-- @+node:gcross.20110918102335.1169: *3* nonTrivialAt
nonTrivialAt :: Bits α ⇒ Int → Operator α → Bool
nonTrivialAt i (Operator x z) = testBit x i || testBit z i
-- @+node:gcross.20110724213035.1199: *3* toPauliList
toPauliList :: (Integral α, Bits α) ⇒ Int → Operator α → [Pauli]
toPauliList 0 _ = []
toPauliList n (Operator 0 0) = replicate n I
toPauliList n (Operator x z) = toEnum (fromIntegral (x .&. 1 + shiftL (z .&. 1) 1)) : toPauliList (n-1) (Operator (shiftR x 1) (shiftR z 1))
-- @-others
-- @-leo
