{-# OPTIONS_GHC -funbox-strict-fields #-}

-- Language extensions {{{

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

-- }}} Language extensions

module Data.Quantum.Operator where

-- Imports {{{

import Control.Applicative (liftA2)

import Data.Bits (Bits((.&.),bitSize,clearBit,setBit,shiftL,shiftR,testBit,xor))
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
    } deriving (Eq,Ord)

instance Bits α ⇒ Monoid (Operator α) where -- {{{
    mempty = Operator 0 0
    {-# INLINE mempty #-}
    mappend a b = Operator ((xor `on` operatorX) a b) ((xor `on` operatorZ) a b)
    {-# INLINE mappend #-}
    mconcat = liftA2 Operator
        (foldl' xor 0 . map operatorX)
        (foldl' xor 0 . map operatorZ)
    {-# INLINE mconcat #-}
-- }}}

instance Bits α ⇒ Commutable (Operator α) where -- {{{
    commute a b =
        ( countBits (operatorX a .&. operatorZ b)
        + countBits (operatorX b .&. operatorZ a)
        ) `mod` 2 == 0
    {-# INLINE commute #-}

    antiCommute a b = not (commute a b)
    {-# INLINE antiCommute #-}
-- }}}

instance Bits α ⇒ Read (Operator α) where -- {{{
    readsPrec _ "" = [(Operator 0 0,"")]
    readsPrec _ text = go1 [] text
      where
        go1 accum ('I':rest) = go1 (I:accum) rest
        go1 accum ('X':rest) = go1 (X:accum) rest
        go1 accum ('Z':rest) = go1 (Z:accum) rest
        go1 accum ('Y':rest) = go1 (Y:accum) rest
        go1 accum remainder = go2 accum remainder

        go2 accum@(p:rest) remainder =
            (fromPauliList . reverse $ accum,remainder)
            :
            go2 rest (pauliToChar p:remainder)
        go2 [] remainder = []
-- }}}

instance Bits α ⇒ Show (Operator α) where -- {{{
    show o@(Operator x z) = show (toPauliList maximum_bit o)
      where
        maximum_bit = go (bitSize x)
          where
            go 0 = 0
            go n
             | testBit x (n-1) || testBit z (n-1) = n
             | otherwise = go (n-1)
-- }}}

-- }}} Operator

-- Pauli {{{
data Pauli = I | X | Z | Y deriving (Eq,Ord,Read,Enum)

instance Show Pauli where -- {{{
    show = (:[]) . pauliToChar
    showList [] s = s
    showList (p:rest) s = pauliToChar p : showList rest s
-- }}}

instance Monoid Pauli where -- {{{
    mempty = I
    {-# INLINE mempty #-}
    mappend a b = toEnum (fromEnum a `xor` fromEnum b)
-- }}}

instance Commutable Pauli where -- {{{
    commute I _ = True
    commute _ I = True
    commute x y = x == y
-- }}}

-- Pauli }}}

-- }}} Types

-- Functions {{{

agreeAt :: Bits α ⇒ Int → Operator α → Operator α → Bool -- {{{
agreeAt i a b =
    (testBit (operatorX a) i == testBit (operatorX b) i) &&
    (testBit (operatorZ a) i == testBit (operatorZ b) i)
{-# SPECIALIZE INLINE agreeAt :: Int → Operator Word8 → Operator Word8 → Bool #-}
{-# SPECIALIZE INLINE agreeAt :: Int → Operator Word16 → Operator Word16 → Bool #-}
{-# SPECIALIZE INLINE agreeAt :: Int → Operator Word32 → Operator Word32 → Bool #-}
{-# SPECIALIZE INLINE agreeAt :: Int → Operator Word64 → Operator Word64 → Bool #-}
{-# SPECIALIZE INLINE agreeAt :: Int → Operator Integer → Operator Integer → Bool #-}
{-# INLINE agreeAt #-}
-- }}}

-- commuteAt/antiCommuteAt {{{
commuteAt :: Bits α ⇒ Int → Operator α → Operator α → Bool
commuteAt i a b = not (antiCommuteAt i a b)
{-# SPECIALIZE INLINE commuteAt :: Int → Operator Word8 → Operator Word8 → Bool #-}
{-# SPECIALIZE INLINE commuteAt :: Int → Operator Word16 → Operator Word16 → Bool #-}
{-# SPECIALIZE INLINE commuteAt :: Int → Operator Word32 → Operator Word32 → Bool #-}
{-# SPECIALIZE INLINE commuteAt :: Int → Operator Word64 → Operator Word64 → Bool #-}
{-# SPECIALIZE INLINE commuteAt :: Int → Operator Integer → Operator Integer → Bool #-}
{-# INLINE commuteAt #-}

antiCommuteAt :: Bits α ⇒ Int → Operator α → Operator α → Bool
antiCommuteAt i a b =
    (testBit (operatorX a) i && testBit (operatorZ b) i) /=
    (testBit (operatorZ a) i && testBit (operatorX b) i)
{-# SPECIALIZE INLINE antiCommuteAt :: Int → Operator Word8 → Operator Word8 → Bool #-}
{-# SPECIALIZE INLINE antiCommuteAt :: Int → Operator Word16 → Operator Word16 → Bool #-}
{-# SPECIALIZE INLINE antiCommuteAt :: Int → Operator Word32 → Operator Word32 → Bool #-}
{-# SPECIALIZE INLINE antiCommuteAt :: Int → Operator Word64 → Operator Word64 → Bool #-}
{-# SPECIALIZE INLINE antiCommuteAt :: Int → Operator Integer → Operator Integer → Bool #-}
{-# INLINE antiCommuteAt #-}
-- }}}

countBits :: Bits α ⇒ α → Int -- {{{
countBits = go 0
  where
    go !accum 0 = accum
    go !accum x = go (if testBit x 0 then accum + 1 else accum) (shiftR x 1)
{-# SPECIALIZE INLINE countBits :: Word8 → Int #-}
{-# SPECIALIZE INLINE countBits :: Word16 → Int #-}
{-# SPECIALIZE INLINE countBits :: Word32 → Int #-}
{-# SPECIALIZE INLINE countBits :: Word64 → Int #-}
{-# SPECIALIZE INLINE countBits :: Integer → Int #-}
{-# INLINE countBits #-}
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
{-# SPECIALIZE INLINE getPauliAt :: Int → Operator Word8 → Pauli #-}
{-# SPECIALIZE INLINE getPauliAt :: Int → Operator Word16 → Pauli #-}
{-# SPECIALIZE INLINE getPauliAt :: Int → Operator Word32 → Pauli #-}
{-# SPECIALIZE INLINE getPauliAt :: Int → Operator Word64 → Pauli #-}
{-# SPECIALIZE INLINE getPauliAt :: Int → Operator Integer → Pauli #-}
{-# INLINE getPauliAt #-}
-- }}}

hasXBit, hasZBit :: Pauli → Bool -- {{{

hasXBit I = False
hasXBit X = True
hasXBit Z = False
hasXBit Y = True
{-# INLINE hasXBit #-}

hasZBit I = False
hasZBit X = False
hasZBit Z = True
hasZBit Y = True
{-# INLINE hasZBit #-}
-- }}}

hasXBitAt, hasZBitAt :: Bits α ⇒ Int → Operator α → Bool -- {{{

hasXBitAt column (Operator x _) = testBit x column
{-# SPECIALIZE INLINE hasXBitAt :: Int → Operator Word8 → Bool #-}
{-# SPECIALIZE INLINE hasXBitAt :: Int → Operator Word16 → Bool #-}
{-# SPECIALIZE INLINE hasXBitAt :: Int → Operator Word32 → Bool #-}
{-# SPECIALIZE INLINE hasXBitAt :: Int → Operator Word64 → Bool #-}
{-# SPECIALIZE INLINE hasXBitAt :: Int → Operator Integer → Bool #-}
{-# INLINE hasXBitAt #-}

hasZBitAt column (Operator _ z) = testBit z column
{-# SPECIALIZE INLINE hasZBitAt :: Int → Operator Word8 → Bool #-}
{-# SPECIALIZE INLINE hasZBitAt :: Int → Operator Word16 → Bool #-}
{-# SPECIALIZE INLINE hasZBitAt :: Int → Operator Word32 → Bool #-}
{-# SPECIALIZE INLINE hasZBitAt :: Int → Operator Word64 → Bool #-}
{-# SPECIALIZE INLINE hasZBitAt :: Int → Operator Integer → Bool #-}
{-# INLINE hasZBitAt #-}
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
{-# SPECIALIZE INLINE maybeFirstNonTrivialColumnOf :: Operator Word8 → Maybe Int #-}
{-# SPECIALIZE INLINE maybeFirstNonTrivialColumnOf :: Operator Word16 → Maybe Int #-}
{-# SPECIALIZE INLINE maybeFirstNonTrivialColumnOf :: Operator Word32 → Maybe Int #-}
{-# SPECIALIZE INLINE maybeFirstNonTrivialColumnOf :: Operator Word64 → Maybe Int #-}
{-# SPECIALIZE INLINE maybeFirstNonTrivialColumnOf :: Operator Integer → Maybe Int #-}
{-# INLINE maybeFirstNonTrivialColumnOf #-}
-- }}}

multiplyByIf :: Bits α ⇒ Bool → Operator α → Operator α → Operator α -- {{{
multiplyByIf False a b = b
multiplyByIf True a b = b `mappend` a
{-# SPECIALIZE INLINE multiplyByIf :: Bool → Operator Word8 → Operator Word8 → Operator Word8 #-}
{-# SPECIALIZE INLINE multiplyByIf :: Bool → Operator Word16 → Operator Word16 → Operator Word16 #-}
{-# SPECIALIZE INLINE multiplyByIf :: Bool → Operator Word32 → Operator Word32 → Operator Word32 #-}
{-# SPECIALIZE INLINE multiplyByIf :: Bool → Operator Word64 → Operator Word64 → Operator Word64 #-}
{-# SPECIALIZE INLINE multiplyByIf :: Bool → Operator Integer → Operator Integer → Operator Integer #-}
{-# INLINE multiplyByIf #-}
-- }}}

multiplyByIfAntiCommuteAt :: Bits α ⇒ Int → Operator α → Operator α → Operator α -- {{{
multiplyByIfAntiCommuteAt column a b = multiplyByIf (antiCommuteAt column a b) a b
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteAt :: Int → Operator Word8 → Operator Word8 → Operator Word8 #-}
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteAt :: Int → Operator Word16 → Operator Word16 → Operator Word16 #-}
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteAt :: Int → Operator Word32 → Operator Word32 → Operator Word32 #-}
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteAt :: Int → Operator Word64 → Operator Word64 → Operator Word64 #-}
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteAt :: Int → Operator Integer → Operator Integer → Operator Integer #-}
{-# INLINE multiplyByIfAntiCommuteAt #-}
-- }}}

multiplyByIfAntiCommuteWith :: Bits α ⇒ Operator α → Operator α → Operator α → Operator α -- {{{
multiplyByIfAntiCommuteWith multiplier commuter op = multiplyByIf (antiCommute commuter op) multiplier op
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteWith :: Operator Word8 → Operator Word8 → Operator Word8 → Operator Word8 #-}
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteWith :: Operator Word16 → Operator Word16 → Operator Word16 → Operator Word16 #-}
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteWith :: Operator Word32 → Operator Word32 → Operator Word32 → Operator Word32 #-}
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteWith :: Operator Word64 → Operator Word64 → Operator Word64 → Operator Word64 #-}
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteWith :: Operator Integer → Operator Integer → Operator Integer → Operator Integer #-}
{-# INLINE multiplyByIfAntiCommuteWith #-}
-- }}}

multiplyByIfAgreeAt :: Bits α ⇒ Int → Operator α → Operator α → Operator α -- {{{
multiplyByIfAgreeAt column a b = multiplyByIf (agreeAt column a b) a b
{-# SPECIALIZE INLINE multiplyByIfAgreeAt :: Int → Operator Word8 → Operator Word8 → Operator Word8 #-}
{-# SPECIALIZE INLINE multiplyByIfAgreeAt :: Int → Operator Word16 → Operator Word16 → Operator Word16 #-}
{-# SPECIALIZE INLINE multiplyByIfAgreeAt :: Int → Operator Word32 → Operator Word32 → Operator Word32 #-}
{-# SPECIALIZE INLINE multiplyByIfAgreeAt :: Int → Operator Word64 → Operator Word64 → Operator Word64 #-}
{-# SPECIALIZE INLINE multiplyByIfAgreeAt :: Int → Operator Integer → Operator Integer → Operator Integer #-}
{-# INLINE multiplyByIfAgreeAt #-}
-- }}}

-- multiplyByIfHas(X/Z/XZ)BitAt {{{
multiplyByIfHasXBitAt :: Bits α ⇒ Int → Operator α → Operator α → Operator α
multiplyByIfHasXBitAt column a b = multiplyByIf (hasXBitAt column b) a b
{-# SPECIALIZE INLINE multiplyByIfHasXBitAt :: Int → Operator Word8 → Operator Word8 → Operator Word8 #-}
{-# SPECIALIZE INLINE multiplyByIfHasXBitAt :: Int → Operator Word16 → Operator Word16 → Operator Word16 #-}
{-# SPECIALIZE INLINE multiplyByIfHasXBitAt :: Int → Operator Word32 → Operator Word32 → Operator Word32 #-}
{-# SPECIALIZE INLINE multiplyByIfHasXBitAt :: Int → Operator Word64 → Operator Word64 → Operator Word64 #-}
{-# SPECIALIZE INLINE multiplyByIfHasXBitAt :: Int → Operator Integer → Operator Integer → Operator Integer #-}
{-# INLINE multiplyByIfHasXBitAt #-}

multiplyByIfHasZBitAt :: Bits α ⇒ Int → Operator α → Operator α → Operator α
multiplyByIfHasZBitAt column a b = multiplyByIf (hasZBitAt column b) a b
{-# SPECIALIZE INLINE multiplyByIfHasZBitAt :: Int → Operator Word8 → Operator Word8 → Operator Word8 #-}
{-# SPECIALIZE INLINE multiplyByIfHasZBitAt :: Int → Operator Word16 → Operator Word16 → Operator Word16 #-}
{-# SPECIALIZE INLINE multiplyByIfHasZBitAt :: Int → Operator Word32 → Operator Word32 → Operator Word32 #-}
{-# SPECIALIZE INLINE multiplyByIfHasZBitAt :: Int → Operator Word64 → Operator Word64 → Operator Word64 #-}
{-# SPECIALIZE INLINE multiplyByIfHasZBitAt :: Int → Operator Integer → Operator Integer → Operator Integer #-}
{-# INLINE multiplyByIfHasZBitAt #-}

multiplyByIfHasXZBitAt :: Bits α ⇒ Int → Operator α → Operator α → Operator α → Operator α
multiplyByIfHasXZBitAt column x_op z_op =
    multiplyByIfHasXBitAt column x_op
    .
    multiplyByIfHasZBitAt column z_op
{-# INLINE multiplyByIfHasXZBitAt #-}
{-# SPECIALIZE INLINE multiplyByIfHasXZBitAt :: Int → Operator Word8 → Operator Word8 → Operator Word8 → Operator Word8 #-}
{-# SPECIALIZE INLINE multiplyByIfHasXZBitAt :: Int → Operator Word16 → Operator Word16 → Operator Word16 → Operator Word16 #-}
{-# SPECIALIZE INLINE multiplyByIfHasXZBitAt :: Int → Operator Word32 → Operator Word32 → Operator Word32 → Operator Word32 #-}
{-# SPECIALIZE INLINE multiplyByIfHasXZBitAt :: Int → Operator Word64 → Operator Word64 → Operator Word64 → Operator Word64 #-}
{-# SPECIALIZE INLINE multiplyByIfHasXZBitAt :: Int → Operator Integer → Operator Integer → Operator Integer → Operator Integer #-}
-- }}}

pauliToChar :: Pauli → Char -- {{{
pauliToChar I = 'I'
pauliToChar X = 'X'
pauliToChar Z = 'Z'
pauliToChar Y = 'Y'
-- }}}

isIdentity :: Num α ⇒ Operator α → Bool -- {{{
isIdentity (Operator 0 0) = True
isIdentity _ = False
{-# SPECIALIZE INLINE isIdentity :: Operator Word8 → Bool #-}
{-# SPECIALIZE INLINE isIdentity :: Operator Word16 → Bool #-}
{-# SPECIALIZE INLINE isIdentity :: Operator Word32 → Bool #-}
{-# SPECIALIZE INLINE isIdentity :: Operator Word64 → Bool #-}
{-# SPECIALIZE INLINE isIdentity :: Operator Integer → Bool #-}
{-# INLINE isIdentity #-}
-- }}}

isNotIdentity :: Num α ⇒ Operator α → Bool -- {{{
isNotIdentity = not . isIdentity
{-# SPECIALIZE INLINE isNotIdentity :: Operator Word8 → Bool #-}
{-# SPECIALIZE INLINE isNotIdentity :: Operator Word16 → Bool #-}
{-# SPECIALIZE INLINE isNotIdentity :: Operator Word32 → Bool #-}
{-# SPECIALIZE INLINE isNotIdentity :: Operator Word64 → Bool #-}
{-# SPECIALIZE INLINE isNotIdentity :: Operator Integer → Bool #-}
{-# INLINE isNotIdentity #-}
-- }}}

-- nonTrivialAt / trivialAt -- {{{
nonTrivialAt :: Bits α ⇒ Int → Operator α → Bool
nonTrivialAt i (Operator x z) = testBit x i || testBit z i
{-# SPECIALIZE INLINE nonTrivialAt :: Int → Operator Word8 → Bool #-}
{-# SPECIALIZE INLINE nonTrivialAt :: Int → Operator Word16 → Bool #-}
{-# SPECIALIZE INLINE nonTrivialAt :: Int → Operator Word32 → Bool #-}
{-# SPECIALIZE INLINE nonTrivialAt :: Int → Operator Word64 → Bool #-}
{-# SPECIALIZE INLINE nonTrivialAt :: Int → Operator Integer → Bool #-}
{-# INLINE nonTrivialAt #-}

trivialAt :: Bits α ⇒ Int → Operator α → Bool
trivialAt i = not . nonTrivialAt i
{-# SPECIALIZE INLINE trivialAt :: Int → Operator Word8 → Bool #-}
{-# SPECIALIZE INLINE trivialAt :: Int → Operator Word16 → Bool #-}
{-# SPECIALIZE INLINE trivialAt :: Int → Operator Word32 → Bool #-}
{-# SPECIALIZE INLINE trivialAt :: Int → Operator Word64 → Bool #-}
{-# SPECIALIZE INLINE trivialAt :: Int → Operator Integer → Bool #-}
{-# INLINE trivialAt #-}
-- }}}

setPauliAt :: Bits α ⇒ Int → Pauli → Operator α → Operator α -- {{{
setPauliAt column I (Operator x z) = Operator (clearBit x column) (clearBit z column)
setPauliAt column X (Operator x z) = Operator (setBit   x column) (clearBit z column)
setPauliAt column Z (Operator x z) = Operator (clearBit x column) (setBit   z column)
setPauliAt column Y (Operator x z) = Operator (setBit   x column) (setBit   z column)
{-# SPECIALIZE INLINE setPauliAt :: Int → Pauli → Operator Word8 → Operator Word8 #-}
{-# SPECIALIZE INLINE setPauliAt :: Int → Pauli → Operator Word16 → Operator Word16 #-}
{-# SPECIALIZE INLINE setPauliAt :: Int → Pauli → Operator Word32 → Operator Word32 #-}
{-# SPECIALIZE INLINE setPauliAt :: Int → Pauli → Operator Word64 → Operator Word64 #-}
{-# SPECIALIZE INLINE setPauliAt :: Int → Pauli → Operator Integer → Operator Integer #-}
{-# INLINE setPauliAt #-}
-- }}}

toPauliList :: Bits α ⇒ Int → Operator α → [Pauli] -- {{{
toPauliList 0 _ = []
toPauliList n (Operator 0 0) = replicate n I
toPauliList n op@(Operator x z) = getPauliAt 0 op : toPauliList (n-1) (Operator (shiftR x 1) (shiftR z 1))
-- }}}

-- }}}
