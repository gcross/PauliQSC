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
type Operator8 = Operator Word8
type Operator16 = Operator Word16
type Operator32 = Operator Word32
type Operator64 = Operator Word64
type OperatorArb = Operator Integer

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
{-# SPECIALIZE INLINE agreeAt :: Int → Operator8 → Operator8 → Bool #-}
{-# SPECIALIZE INLINE agreeAt :: Int → Operator16 → Operator16 → Bool #-}
{-# SPECIALIZE INLINE agreeAt :: Int → Operator32 → Operator32 → Bool #-}
{-# SPECIALIZE INLINE agreeAt :: Int → Operator64 → Operator64 → Bool #-}
{-# SPECIALIZE INLINE agreeAt :: Int → OperatorArb → OperatorArb → Bool #-}
{-# INLINE agreeAt #-}
-- }}}

-- commuteAt/antiCommuteAt {{{
commuteAt :: Bits α ⇒ Int → Operator α → Operator α → Bool
commuteAt i a b = not (antiCommuteAt i a b)
{-# SPECIALIZE INLINE commuteAt :: Int → Operator8 → Operator8 → Bool #-}
{-# SPECIALIZE INLINE commuteAt :: Int → Operator16 → Operator16 → Bool #-}
{-# SPECIALIZE INLINE commuteAt :: Int → Operator32 → Operator32 → Bool #-}
{-# SPECIALIZE INLINE commuteAt :: Int → Operator64 → Operator64 → Bool #-}
{-# SPECIALIZE INLINE commuteAt :: Int → OperatorArb → OperatorArb → Bool #-}
{-# INLINE commuteAt #-}

antiCommuteAt :: Bits α ⇒ Int → Operator α → Operator α → Bool
antiCommuteAt i a b =
    (testBit (operatorX a) i && testBit (operatorZ b) i) /=
    (testBit (operatorZ a) i && testBit (operatorX b) i)
{-# SPECIALIZE INLINE antiCommuteAt :: Int → Operator8 → Operator8 → Bool #-}
{-# SPECIALIZE INLINE antiCommuteAt :: Int → Operator16 → Operator16 → Bool #-}
{-# SPECIALIZE INLINE antiCommuteAt :: Int → Operator32 → Operator32 → Bool #-}
{-# SPECIALIZE INLINE antiCommuteAt :: Int → Operator64 → Operator64 → Bool #-}
{-# SPECIALIZE INLINE antiCommuteAt :: Int → OperatorArb → OperatorArb → Bool #-}
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
{-# SPECIALIZE INLINE getPauliAt :: Int → Operator8 → Pauli #-}
{-# SPECIALIZE INLINE getPauliAt :: Int → Operator16 → Pauli #-}
{-# SPECIALIZE INLINE getPauliAt :: Int → Operator32 → Pauli #-}
{-# SPECIALIZE INLINE getPauliAt :: Int → Operator64 → Pauli #-}
{-# SPECIALIZE INLINE getPauliAt :: Int → OperatorArb → Pauli #-}
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
{-# SPECIALIZE INLINE hasXBitAt :: Int → Operator8 → Bool #-}
{-# SPECIALIZE INLINE hasXBitAt :: Int → Operator16 → Bool #-}
{-# SPECIALIZE INLINE hasXBitAt :: Int → Operator32 → Bool #-}
{-# SPECIALIZE INLINE hasXBitAt :: Int → Operator64 → Bool #-}
{-# SPECIALIZE INLINE hasXBitAt :: Int → OperatorArb → Bool #-}
{-# INLINE hasXBitAt #-}

hasZBitAt column (Operator _ z) = testBit z column
{-# SPECIALIZE INLINE hasZBitAt :: Int → Operator8 → Bool #-}
{-# SPECIALIZE INLINE hasZBitAt :: Int → Operator16 → Bool #-}
{-# SPECIALIZE INLINE hasZBitAt :: Int → Operator32 → Bool #-}
{-# SPECIALIZE INLINE hasZBitAt :: Int → Operator64 → Bool #-}
{-# SPECIALIZE INLINE hasZBitAt :: Int → OperatorArb → Bool #-}
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
{-# SPECIALIZE INLINE maybeFirstNonTrivialColumnOf :: Operator8 → Maybe Int #-}
{-# SPECIALIZE INLINE maybeFirstNonTrivialColumnOf :: Operator16 → Maybe Int #-}
{-# SPECIALIZE INLINE maybeFirstNonTrivialColumnOf :: Operator32 → Maybe Int #-}
{-# SPECIALIZE INLINE maybeFirstNonTrivialColumnOf :: Operator64 → Maybe Int #-}
{-# SPECIALIZE INLINE maybeFirstNonTrivialColumnOf :: OperatorArb → Maybe Int #-}
{-# INLINE maybeFirstNonTrivialColumnOf #-}
-- }}}

multiplyByIf :: Bits α ⇒ Bool → Operator α → Operator α → Operator α -- {{{
multiplyByIf False a b = b
multiplyByIf True a b = b `mappend` a
{-# SPECIALIZE INLINE multiplyByIf :: Bool → Operator8 → Operator8 → Operator8 #-}
{-# SPECIALIZE INLINE multiplyByIf :: Bool → Operator16 → Operator16 → Operator16 #-}
{-# SPECIALIZE INLINE multiplyByIf :: Bool → Operator32 → Operator32 → Operator32 #-}
{-# SPECIALIZE INLINE multiplyByIf :: Bool → Operator64 → Operator64 → Operator64 #-}
{-# SPECIALIZE INLINE multiplyByIf :: Bool → OperatorArb → OperatorArb → OperatorArb #-}
{-# INLINE multiplyByIf #-}
-- }}}

multiplyByIfAntiCommuteAt :: Bits α ⇒ Int → Operator α → Operator α → Operator α -- {{{
multiplyByIfAntiCommuteAt column a b = multiplyByIf (antiCommuteAt column a b) a b
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteAt :: Int → Operator8 → Operator8 → Operator8 #-}
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteAt :: Int → Operator16 → Operator16 → Operator16 #-}
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteAt :: Int → Operator32 → Operator32 → Operator32 #-}
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteAt :: Int → Operator64 → Operator64 → Operator64 #-}
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteAt :: Int → OperatorArb → OperatorArb → OperatorArb #-}
{-# INLINE multiplyByIfAntiCommuteAt #-}
-- }}}

multiplyByIfAntiCommuteWith :: Bits α ⇒ Operator α → Operator α → Operator α → Operator α -- {{{
multiplyByIfAntiCommuteWith multiplier commuter op = multiplyByIf (antiCommute commuter op) multiplier op
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteWith :: Operator8 → Operator8 → Operator8 → Operator8 #-}
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteWith :: Operator16 → Operator16 → Operator16 → Operator16 #-}
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteWith :: Operator32 → Operator32 → Operator32 → Operator32 #-}
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteWith :: Operator64 → Operator64 → Operator64 → Operator64 #-}
{-# SPECIALIZE INLINE multiplyByIfAntiCommuteWith :: OperatorArb → OperatorArb → OperatorArb → OperatorArb #-}
{-# INLINE multiplyByIfAntiCommuteWith #-}
-- }}}

multiplyByIfAgreeAt :: Bits α ⇒ Int → Operator α → Operator α → Operator α -- {{{
multiplyByIfAgreeAt column a b = multiplyByIf (agreeAt column a b) a b
{-# SPECIALIZE INLINE multiplyByIfAgreeAt :: Int → Operator8 → Operator8 → Operator8 #-}
{-# SPECIALIZE INLINE multiplyByIfAgreeAt :: Int → Operator16 → Operator16 → Operator16 #-}
{-# SPECIALIZE INLINE multiplyByIfAgreeAt :: Int → Operator32 → Operator32 → Operator32 #-}
{-# SPECIALIZE INLINE multiplyByIfAgreeAt :: Int → Operator64 → Operator64 → Operator64 #-}
{-# SPECIALIZE INLINE multiplyByIfAgreeAt :: Int → OperatorArb → OperatorArb → OperatorArb #-}
{-# INLINE multiplyByIfAgreeAt #-}
-- }}}

-- multiplyByIfHas(X/Z/XZ)BitAt {{{
multiplyByIfHasXBitAt :: Bits α ⇒ Int → Operator α → Operator α → Operator α
multiplyByIfHasXBitAt column a b = multiplyByIf (hasXBitAt column b) a b
{-# SPECIALIZE INLINE multiplyByIfHasXBitAt :: Int → Operator8 → Operator8 → Operator8 #-}
{-# SPECIALIZE INLINE multiplyByIfHasXBitAt :: Int → Operator16 → Operator16 → Operator16 #-}
{-# SPECIALIZE INLINE multiplyByIfHasXBitAt :: Int → Operator32 → Operator32 → Operator32 #-}
{-# SPECIALIZE INLINE multiplyByIfHasXBitAt :: Int → Operator64 → Operator64 → Operator64 #-}
{-# SPECIALIZE INLINE multiplyByIfHasXBitAt :: Int → OperatorArb → OperatorArb → OperatorArb #-}
{-# INLINE multiplyByIfHasXBitAt #-}

multiplyByIfHasZBitAt :: Bits α ⇒ Int → Operator α → Operator α → Operator α
multiplyByIfHasZBitAt column a b = multiplyByIf (hasZBitAt column b) a b
{-# SPECIALIZE INLINE multiplyByIfHasZBitAt :: Int → Operator8 → Operator8 → Operator8 #-}
{-# SPECIALIZE INLINE multiplyByIfHasZBitAt :: Int → Operator16 → Operator16 → Operator16 #-}
{-# SPECIALIZE INLINE multiplyByIfHasZBitAt :: Int → Operator32 → Operator32 → Operator32 #-}
{-# SPECIALIZE INLINE multiplyByIfHasZBitAt :: Int → Operator64 → Operator64 → Operator64 #-}
{-# SPECIALIZE INLINE multiplyByIfHasZBitAt :: Int → OperatorArb → OperatorArb → OperatorArb #-}
{-# INLINE multiplyByIfHasZBitAt #-}

multiplyByIfHasXZBitAt :: Bits α ⇒ Int → Operator α → Operator α → Operator α → Operator α
multiplyByIfHasXZBitAt column x_op z_op =
    multiplyByIfHasXBitAt column x_op
    .
    multiplyByIfHasZBitAt column z_op
{-# INLINE multiplyByIfHasXZBitAt #-}
{-# SPECIALIZE INLINE multiplyByIfHasXZBitAt :: Int → Operator8 → Operator8 → Operator8 → Operator8 #-}
{-# SPECIALIZE INLINE multiplyByIfHasXZBitAt :: Int → Operator16 → Operator16 → Operator16 → Operator16 #-}
{-# SPECIALIZE INLINE multiplyByIfHasXZBitAt :: Int → Operator32 → Operator32 → Operator32 → Operator32 #-}
{-# SPECIALIZE INLINE multiplyByIfHasXZBitAt :: Int → Operator64 → Operator64 → Operator64 → Operator64 #-}
{-# SPECIALIZE INLINE multiplyByIfHasXZBitAt :: Int → OperatorArb → OperatorArb → OperatorArb → OperatorArb #-}
-- }}}

pauliToChar :: Pauli → Char -- {{{
pauliToChar I = 'I'
pauliToChar X = 'X'
pauliToChar Z = 'Z'
pauliToChar Y = 'Y'
-- }}}

isIdentity :: (Eq α, Num α) ⇒ Operator α → Bool -- {{{
isIdentity (Operator 0 0) = True
isIdentity _ = False
{-# SPECIALIZE INLINE isIdentity :: Operator8 → Bool #-}
{-# SPECIALIZE INLINE isIdentity :: Operator16 → Bool #-}
{-# SPECIALIZE INLINE isIdentity :: Operator32 → Bool #-}
{-# SPECIALIZE INLINE isIdentity :: Operator64 → Bool #-}
{-# SPECIALIZE INLINE isIdentity :: OperatorArb → Bool #-}
{-# INLINE isIdentity #-}
-- }}}

isNotIdentity :: (Eq α, Num α) ⇒ Operator α → Bool -- {{{
isNotIdentity = not . isIdentity
{-# SPECIALIZE INLINE isNotIdentity :: Operator8 → Bool #-}
{-# SPECIALIZE INLINE isNotIdentity :: Operator16 → Bool #-}
{-# SPECIALIZE INLINE isNotIdentity :: Operator32 → Bool #-}
{-# SPECIALIZE INLINE isNotIdentity :: Operator64 → Bool #-}
{-# SPECIALIZE INLINE isNotIdentity :: OperatorArb → Bool #-}
{-# INLINE isNotIdentity #-}
-- }}}

-- nonTrivialAt / trivialAt -- {{{
nonTrivialAt :: Bits α ⇒ Int → Operator α → Bool
nonTrivialAt i (Operator x z) = testBit x i || testBit z i
{-# SPECIALIZE INLINE nonTrivialAt :: Int → Operator8 → Bool #-}
{-# SPECIALIZE INLINE nonTrivialAt :: Int → Operator16 → Bool #-}
{-# SPECIALIZE INLINE nonTrivialAt :: Int → Operator32 → Bool #-}
{-# SPECIALIZE INLINE nonTrivialAt :: Int → Operator64 → Bool #-}
{-# SPECIALIZE INLINE nonTrivialAt :: Int → OperatorArb → Bool #-}
{-# INLINE nonTrivialAt #-}

trivialAt :: Bits α ⇒ Int → Operator α → Bool
trivialAt i = not . nonTrivialAt i
{-# SPECIALIZE INLINE trivialAt :: Int → Operator8 → Bool #-}
{-# SPECIALIZE INLINE trivialAt :: Int → Operator16 → Bool #-}
{-# SPECIALIZE INLINE trivialAt :: Int → Operator32 → Bool #-}
{-# SPECIALIZE INLINE trivialAt :: Int → Operator64 → Bool #-}
{-# SPECIALIZE INLINE trivialAt :: Int → OperatorArb → Bool #-}
{-# INLINE trivialAt #-}
-- }}}

setPauliAt :: Bits α ⇒ Int → Pauli → Operator α → Operator α -- {{{
setPauliAt column I (Operator x z) = Operator (clearBit x column) (clearBit z column)
setPauliAt column X (Operator x z) = Operator (setBit   x column) (clearBit z column)
setPauliAt column Z (Operator x z) = Operator (clearBit x column) (setBit   z column)
setPauliAt column Y (Operator x z) = Operator (setBit   x column) (setBit   z column)
{-# SPECIALIZE INLINE setPauliAt :: Int → Pauli → Operator8 → Operator8 #-}
{-# SPECIALIZE INLINE setPauliAt :: Int → Pauli → Operator16 → Operator16 #-}
{-# SPECIALIZE INLINE setPauliAt :: Int → Pauli → Operator32 → Operator32 #-}
{-# SPECIALIZE INLINE setPauliAt :: Int → Pauli → Operator64 → Operator64 #-}
{-# SPECIALIZE INLINE setPauliAt :: Int → Pauli → OperatorArb → OperatorArb #-}
{-# INLINE setPauliAt #-}
-- }}}

toPauliList :: Bits α ⇒ Int → Operator α → [Pauli] -- {{{
toPauliList 0 _ = []
toPauliList n (Operator 0 0) = replicate n I
toPauliList n op@(Operator x z) = getPauliAt 0 op : toPauliList (n-1) (Operator (shiftR x 1) (shiftR z 1))
-- }}}

-- }}}
