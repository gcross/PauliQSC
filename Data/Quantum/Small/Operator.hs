-- Language extensions {{{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Data.Quantum.Small.Operator where

-- Imports {{{

import Control.Applicative (liftA2)

import Data.Bits
import Data.Foldable (foldl')
import Data.Function (on)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.List (replicate)
import Data.Monoid (Monoid(..))
import Data.Word

-- }}}

--- Classes {{{

class Commutable α where -- {{{
    commute :: α → α → Bool
    antiCommute :: α → α → Bool

    commute x y = not (antiCommute x y)
    antiCommute x y = not (commute x y)
-- }}}

-- }}}

-- Types (with instances) {{{

-- Operator {{{
data Operator = Operator
    {   operatorX :: {-# UNPACK #-} !Word
    ,   operatorZ :: {-# UNPACK #-} !Word
    } deriving (Eq,Ord)

instance Commutable Operator where -- {{{
    commute a b =
        ( popCount (operatorX a .&. operatorZ b)
        + popCount (operatorX b .&. operatorZ a)
        ) `mod` 2 == 0
    {-# INLINE commute #-}
-- }}}
instance Monoid Operator where -- {{{
    mempty = Operator 0 0
    {-# INLINE mempty #-}
    mappend a b = Operator ((xor `on` operatorX) a b) ((xor `on` operatorZ) a b)
    {-# INLINE mappend #-}
    mconcat = liftA2 Operator
        (foldl' xor 0 . map operatorX)
        (foldl' xor 0 . map operatorZ)
    {-# INLINE mconcat #-}
-- }}}
instance Read Operator where -- {{{
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
instance Show Operator where -- {{{
    show o@(Operator x z) = show (toPauliList maximum_bit o)
      where
        maximum_bit = go (bitSize x)
          where
            go 0 = 0
            go n
             | testBit x (n-1) || testBit z (n-1) = n
             | otherwise = go (n-1)
-- }}}
-- }}}

-- Pauli {{{
data Pauli = I | X | Z | Y deriving (Eq,Ord,Read,Enum)

instance Commutable Pauli where -- {{{
    commute I _ = True
    commute _ I = True
    commute x y = x == y
    {-# INLINE commute #-}
-- }}}
instance Monoid Pauli where -- {{{
    mempty = I
    {-# INLINE mempty #-}
    mappend a b = toEnum (fromEnum a `xor` fromEnum b)
    {-# INLINE mconcat #-}
-- }}}
instance Show Pauli where -- {{{
    show = (:[]) . pauliToChar
    showList [] s = s
    showList (p:rest) s = pauliToChar p : showList rest s
-- }}}
-- }}}

-- }}}

-- Functions {{{

agreeAt :: Int → Operator → Operator → Bool -- {{{
agreeAt i a b =
    (testBit (operatorX a) i == testBit (operatorX b) i) &&
    (testBit (operatorZ a) i == testBit (operatorZ b) i)
{-# INLINE agreeAt #-}
-- }}}

-- commuteAt/antiCommuteAt {{{
commuteAt :: Int → Operator → Operator → Bool
commuteAt i a b = not (antiCommuteAt i a b)
{-# INLINE commuteAt #-}

antiCommuteAt :: Int → Operator → Operator → Bool
antiCommuteAt i a b =
    (testBit (operatorX a) i && testBit (operatorZ b) i) /=
    (testBit (operatorZ a) i && testBit (operatorX b) i)
{-# INLINE antiCommuteAt #-}
-- }}}

fromPauliList :: [Pauli] → Operator -- {{{
fromPauliList = go 0 0 0
  where
    go i ox oz [] = Operator ox oz
    go i ox oz ((fromEnum → pn):rest) =
        go  (i+1)
            (if pn .&. 1 /= 0 then setBit ox i else ox)
            (if pn .&. 2 /= 0 then setBit oz i else oz)
            rest
-- }}}

getPauliAt :: Int → Operator → Pauli -- {{{
getPauliAt column (Operator x z) =
    case (testBit x column, testBit z column) of
        (False,False) → I
        (True ,False) → X
        (False,True ) → Z
        (True ,True ) → Y
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

hasXBitAt, hasZBitAt :: Int → Operator → Bool -- {{{

hasXBitAt column (Operator x _) = testBit x column
{-# INLINE hasXBitAt #-}

hasZBitAt column (Operator _ z) = testBit z column
{-# INLINE hasZBitAt #-}
-- }}}

maybeFirstNonTrivialColumnOf :: Operator → Maybe Int -- {{{
maybeFirstNonTrivialColumnOf (Operator 0 0) = Nothing
maybeFirstNonTrivialColumnOf (Operator x z) = Just (go 0 x z)
  where
    go i x z
      | x .&. 1 /= 0 = i
      | z .&. 1 /= 0 = i
      | otherwise    = go (i+1) (shiftR x 1) (shiftR z 1)
{-# INLINE maybeFirstNonTrivialColumnOf #-}
-- }}}

multiplyByIf :: Bool → Operator → Operator → Operator -- {{{
multiplyByIf False a b = b
multiplyByIf True a b = b `mappend` a
{-# INLINE multiplyByIf #-}
-- }}}

multiplyByIfAntiCommuteAt :: Int → Operator → Operator → Operator -- {{{
multiplyByIfAntiCommuteAt column a b = multiplyByIf (antiCommuteAt column a b) a b
{-# INLINE multiplyByIfAntiCommuteAt #-}
-- }}}

multiplyByIfAntiCommuteWith :: Operator → Operator → Operator → Operator -- {{{
multiplyByIfAntiCommuteWith multiplier commuter op = multiplyByIf (antiCommute commuter op) multiplier op
{-# INLINE multiplyByIfAntiCommuteWith #-}
-- }}}

multiplyByIfAgreeAt :: Int → Operator → Operator → Operator -- {{{
multiplyByIfAgreeAt column a b = multiplyByIf (agreeAt column a b) a b
{-# INLINE multiplyByIfAgreeAt #-}
-- }}}

-- multiplyByIfHas(X/Z/XZ)BitAt {{{
multiplyByIfHasXBitAt :: Int → Operator → Operator → Operator
multiplyByIfHasXBitAt column a b = multiplyByIf (hasXBitAt column b) a b
{-# INLINE multiplyByIfHasXBitAt #-}

multiplyByIfHasZBitAt :: Int → Operator → Operator → Operator
multiplyByIfHasZBitAt column a b = multiplyByIf (hasZBitAt column b) a b
{-# INLINE multiplyByIfHasZBitAt #-}

multiplyByIfHasXZBitAt :: Int → Operator → Operator → Operator → Operator
multiplyByIfHasXZBitAt column x_op z_op =
    multiplyByIfHasXBitAt column x_op
    .
    multiplyByIfHasZBitAt column z_op
{-# INLINE multiplyByIfHasXZBitAt #-}
-- }}}

pauliToChar :: Pauli → Char -- {{{
pauliToChar I = 'I'
pauliToChar X = 'X'
pauliToChar Z = 'Z'
pauliToChar Y = 'Y'
{-# INLINE pauliToChar #-}
-- }}}

isIdentity :: Operator → Bool -- {{{
isIdentity (Operator 0 0) = True
isIdentity _ = False
{-# INLINE isIdentity #-}
-- }}}

isNotIdentity :: Operator → Bool -- {{{
isNotIdentity = not . isIdentity
{-# INLINE isNotIdentity #-}
-- }}}

-- nonTrivialAt / trivialAt -- {{{
nonTrivialAt :: Int → Operator → Bool
nonTrivialAt i (Operator x z) = testBit x i || testBit z i
{-# INLINE nonTrivialAt #-}

trivialAt :: Int → Operator → Bool
trivialAt i = not . nonTrivialAt i
{-# INLINE trivialAt #-}
-- }}}

setPauliAt :: Int → Pauli → Operator → Operator -- {{{
setPauliAt column I (Operator x z) = Operator (clearBit x column) (clearBit z column)
setPauliAt column X (Operator x z) = Operator (setBit   x column) (clearBit z column)
setPauliAt column Z (Operator x z) = Operator (clearBit x column) (setBit   z column)
setPauliAt column Y (Operator x z) = Operator (setBit   x column) (setBit   z column)
{-# INLINE setPauliAt #-}
-- }}}

toPauliList :: Int → Operator → [Pauli] -- {{{
toPauliList 0 _ = []
toPauliList n (Operator 0 0) = replicate n I
toPauliList n op@(Operator x z) = getPauliAt 0 op : toPauliList (n-1) (Operator (shiftR x 1) (shiftR z 1))
-- }}}

-- }}}
