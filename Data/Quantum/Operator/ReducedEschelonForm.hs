-- Language extensions {{{

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}

-- }}} Language extensions

module Data.Quantum.Operator.ReducedEschelonForm where

-- Imports {{{

import Control.Arrow (first,second)
import Control.DeepSeq (NFData())
import Control.Monad (MonadPlus(..))

import Data.Bits
import qualified Data.Foldable as Fold
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Monoid (Monoid(..),Sum(..))
import Data.Word

import Data.Quantum.Operator

-- }}} Imports

-- Types {{{

data PseudoGenerator α = -- {{{
    PGX (Operator α)
  | PGZ (Operator α)
  | PGXZ (Operator α) (Operator α)
  deriving (Eq,Ord,Show)
-- }}}

newtype ReducedEschelonForm α = -- {{{
    ReducedEschelonForm { unwrapReducedEschelonForm :: IntMap (PseudoGenerator α) }
    deriving (Eq,Ord,Show)
-- }}}

-- }}} Types

-- Instances {{{

instance Bits α => Monoid (ReducedEschelonForm α) where -- {{{
    mempty = ReducedEschelonForm mempty
    x `mappend` y = addAllToReducedEschelonForm (operatorsInReducedEschelonForm y) x
-- }}}

instance NFData (ReducedEschelonForm α)

-- }}} Instances

-- Functions {{{

addToReducedEschelonForm :: Bits α ⇒ Operator α → ReducedEschelonForm α → ReducedEschelonForm α -- {{{
addToReducedEschelonForm op form = fst (addToReducedEschelonFormWithSuccessTag op form)
{-# SPECIALIZE addToReducedEschelonForm :: Operator Word8 → ReducedEschelonForm Word8 → ReducedEschelonForm Word8 #-}
{-# SPECIALIZE addToReducedEschelonForm :: Operator Word16 → ReducedEschelonForm Word16 → ReducedEschelonForm Word16 #-}
{-# SPECIALIZE addToReducedEschelonForm :: Operator Word32 → ReducedEschelonForm Word32 → ReducedEschelonForm Word32 #-}
{-# SPECIALIZE addToReducedEschelonForm :: Operator Word64 → ReducedEschelonForm Word64 → ReducedEschelonForm Word64 #-}
{-# SPECIALIZE addToReducedEschelonForm :: Operator Integer → ReducedEschelonForm Integer → ReducedEschelonForm Integer #-}
-- }}}

addAllToReducedEschelonForm :: Bits α ⇒ [Operator α] → ReducedEschelonForm α → ReducedEschelonForm α -- {{{
addAllToReducedEschelonForm operators form =
    foldl'
        (flip addToReducedEschelonForm)
        form
        operators
{-# SPECIALIZE addAllToReducedEschelonForm :: [Operator Word8] → ReducedEschelonForm Word8 → ReducedEschelonForm Word8 #-}
{-# SPECIALIZE addAllToReducedEschelonForm :: [Operator Word16] → ReducedEschelonForm Word16 → ReducedEschelonForm Word16 #-}
{-# SPECIALIZE addAllToReducedEschelonForm :: [Operator Word32] → ReducedEschelonForm Word32 → ReducedEschelonForm Word32 #-}
{-# SPECIALIZE addAllToReducedEschelonForm :: [Operator Word64] → ReducedEschelonForm Word64 → ReducedEschelonForm Word64 #-}
{-# SPECIALIZE addAllToReducedEschelonForm :: [Operator Integer] → ReducedEschelonForm Integer → ReducedEschelonForm Integer #-}
-- }}}

addToReducedEschelonFormWithSuccessTag :: forall α. Bits α ⇒ Operator α → ReducedEschelonForm α → (ReducedEschelonForm α, Bool) -- {{{
addToReducedEschelonFormWithSuccessTag original_operator original_form = (new_form,not contained)
  where
    op = orthogonalizeWithReducedEschelonForm original_form original_operator
    contained = isIdentity op
    new_form
     | contained = original_form
     | otherwise = go (IntMap.assocs . unwrapReducedEschelonForm $ original_form)

    addToForm :: Int → PseudoGenerator α → ReducedEschelonForm α
    addToForm column new_pseudo_generator =
        ReducedEschelonForm
        .
        IntMap.insert column new_pseudo_generator
        .
        IntMap.map (mapPseudoGenerator $ orthogonalizeWithPseudoGeneratorAt column new_pseudo_generator)
        .
        IntMap.delete column
        .
        unwrapReducedEschelonForm
        $
        original_form

    go [] =
        let first_non_trivial_column = fromJust $ maybeFirstNonTrivialColumnOf op
        in addToForm
            first_non_trivial_column
            (makeSingletonPseudoGeneratorFromColumn first_non_trivial_column op)
    go ((column,pseudo_generator):rest)
     | trivialAt column op = go rest
     | otherwise = addToForm column $ case pseudo_generator of
        PGX opx
          | hasZBitAt column opx → PGXZ (opx `mappend` op) op
          | otherwise → PGXZ opx op
        PGZ opz
          | hasXBitAt column opz → PGXZ op (opz `mappend` op)
          | otherwise → PGXZ op opz
        PGXZ _ _ → error $ "pseudo-generator " ++ show pseudo_generator ++ " failed to make operator " ++ show original_operator ++ " trivial at column " ++ show column ++ ";  instead the result was " ++ show op
{-# SPECIALIZE addToReducedEschelonFormWithSuccessTag :: Operator Word8 → ReducedEschelonForm Word8 → (ReducedEschelonForm Word8,Bool) #-}
{-# SPECIALIZE addToReducedEschelonFormWithSuccessTag :: Operator Word16 → ReducedEschelonForm Word16 → (ReducedEschelonForm Word16,Bool) #-}
{-# SPECIALIZE addToReducedEschelonFormWithSuccessTag :: Operator Word32 → ReducedEschelonForm Word32 → (ReducedEschelonForm Word32,Bool) #-}
{-# SPECIALIZE addToReducedEschelonFormWithSuccessTag :: Operator Word64 → ReducedEschelonForm Word64 → (ReducedEschelonForm Word64,Bool) #-}
{-# SPECIALIZE addToReducedEschelonFormWithSuccessTag :: Operator Integer → ReducedEschelonForm Integer → (ReducedEschelonForm Integer,Bool) #-}
-- }}}

constructReducedEschelonForm :: Bits α ⇒ [Operator α] → ReducedEschelonForm α -- {{{
constructReducedEschelonForm = flip addAllToReducedEschelonForm mempty
{-# SPECIALIZE constructReducedEschelonForm :: [Operator Word8] → ReducedEschelonForm Word8 #-}
{-# SPECIALIZE constructReducedEschelonForm :: [Operator Word16] → ReducedEschelonForm Word16 #-}
{-# SPECIALIZE constructReducedEschelonForm :: [Operator Word32] → ReducedEschelonForm Word32 #-}
{-# SPECIALIZE constructReducedEschelonForm :: [Operator Word64] → ReducedEschelonForm Word64 #-}
{-# SPECIALIZE constructReducedEschelonForm :: [Operator Integer] → ReducedEschelonForm Integer #-}
-- }}}

mapPseudoGenerator :: (Operator α → Operator α) → PseudoGenerator α → PseudoGenerator α -- {{{
mapPseudoGenerator f (PGX op) = PGX (f op)
mapPseudoGenerator f (PGZ op) = PGZ (f op)
mapPseudoGenerator f (PGXZ opx opz) = PGXZ (f opx) (f opz)
{-# INLINE mapPseudoGenerator #-}
-- }}}

makeSingletonPseudoGeneratorFromColumn :: Bits α ⇒ Int → Operator α → PseudoGenerator α -- {{{
makeSingletonPseudoGeneratorFromColumn column op =
    case getPauliAt column op of
        X → PGX op
        Y → PGX op
        Z → PGZ op
        _ → error $ "tried to make a pseudo-generator using trivial column " ++ show column ++ " of operator " ++ show op
{-# SPECIALIZE INLINE makeSingletonPseudoGeneratorFromColumn :: Int → Operator Word8 → PseudoGenerator Word8 #-}
{-# INLINE makeSingletonPseudoGeneratorFromColumn #-}
-- }}}

numberOfOperatorsInPseudoGenerator :: PseudoGenerator α → Int -- {{{
numberOfOperatorsInPseudoGenerator (PGX _) = 1
numberOfOperatorsInPseudoGenerator (PGZ _) = 1
numberOfOperatorsInPseudoGenerator (PGXZ _ _) = 2
{-# INLINE numberOfOperatorsInPseudoGenerator #-}
-- }}}

numberOfOperatorsInReducedEschelonForm :: ReducedEschelonForm α → Int -- {{{
numberOfOperatorsInReducedEschelonForm =
    getSum
    .
    Fold.foldMap (
        Sum
        .
        numberOfOperatorsInPseudoGenerator
    )
    .
    unwrapReducedEschelonForm
-- }}}

operatorsInPseudoGenerator :: PseudoGenerator α → [Operator α] -- {{{
operatorsInPseudoGenerator (PGX op) = [op]
operatorsInPseudoGenerator (PGZ op) = [op]
operatorsInPseudoGenerator (PGXZ opx opz) = [opx,opz]
{-# INLINE operatorsInPseudoGenerator #-}
-- }}}

operatorsInReducedEschelonForm :: ReducedEschelonForm α → [Operator α] -- {{{
operatorsInReducedEschelonForm =
    concatMap operatorsInPseudoGenerator
    .
    IntMap.elems
    .
    unwrapReducedEschelonForm
-- }}}

orthogonalizeWithPseudoGenerators :: Bits α ⇒ [(Int,PseudoGenerator α)] → Operator α → Operator α -- {{{
orthogonalizeWithPseudoGenerators _ op@(Operator 0 0) = op
orthogonalizeWithPseudoGenerators [] op = op
orthogonalizeWithPseudoGenerators ((column,pseudo_generator):rest) op =
    orthogonalizeWithPseudoGenerators rest (orthogonalizeWithPseudoGeneratorAt column pseudo_generator op)
{-# SPECIALIZE orthogonalizeWithPseudoGenerators :: [(Int,PseudoGenerator Word8)] → Operator Word8 → Operator Word8 #-}
{-# SPECIALIZE orthogonalizeWithPseudoGenerators :: [(Int,PseudoGenerator Word16)] → Operator Word16 → Operator Word16 #-}
{-# SPECIALIZE orthogonalizeWithPseudoGenerators :: [(Int,PseudoGenerator Word32)] → Operator Word32 → Operator Word32 #-}
{-# SPECIALIZE orthogonalizeWithPseudoGenerators :: [(Int,PseudoGenerator Word64)] → Operator Word64 → Operator Word64 #-}
{-# SPECIALIZE orthogonalizeWithPseudoGenerators :: [(Int,PseudoGenerator Integer)] → Operator Integer → Operator Integer #-}
-- }}}

orthogonalizeWithPseudoGeneratorAt :: Bits α ⇒ Int → PseudoGenerator α → Operator α → Operator α -- {{{
orthogonalizeWithPseudoGeneratorAt column (PGX op) = multiplyByIfHasXBitAt column op
orthogonalizeWithPseudoGeneratorAt column (PGZ op) = multiplyByIfHasZBitAt column op
orthogonalizeWithPseudoGeneratorAt column (PGXZ ox oz) = multiplyByIfHasXZBitAt column ox oz
{-# SPECIALIZE INLINE orthogonalizeWithPseudoGeneratorAt :: Int → PseudoGenerator Word8 → Operator Word8 → Operator Word8 #-}
{-# SPECIALIZE INLINE orthogonalizeWithPseudoGeneratorAt :: Int → PseudoGenerator Word16 → Operator Word16 → Operator Word16 #-}
{-# SPECIALIZE INLINE orthogonalizeWithPseudoGeneratorAt :: Int → PseudoGenerator Word32 → Operator Word32 → Operator Word32 #-}
{-# SPECIALIZE INLINE orthogonalizeWithPseudoGeneratorAt :: Int → PseudoGenerator Word64 → Operator Word64 → Operator Word64 #-}
{-# SPECIALIZE INLINE orthogonalizeWithPseudoGeneratorAt :: Int → PseudoGenerator Integer → Operator Integer → Operator Integer #-}
{-# INLINE orthogonalizeWithPseudoGeneratorAt #-}
-- }}}

orthogonalizeWithReducedEschelonForm :: Bits α ⇒ ReducedEschelonForm α → Operator α → Operator α -- {{{
orthogonalizeWithReducedEschelonForm = orthogonalizeWithPseudoGenerators . IntMap.toList . unwrapReducedEschelonForm
{-# SPECIALIZE INLINE orthogonalizeWithReducedEschelonForm :: ReducedEschelonForm Word8 → Operator Word8 → Operator Word8 #-}
{-# SPECIALIZE INLINE orthogonalizeWithReducedEschelonForm :: ReducedEschelonForm Word16 → Operator Word16 → Operator Word16 #-}
{-# SPECIALIZE INLINE orthogonalizeWithReducedEschelonForm :: ReducedEschelonForm Word32 → Operator Word32 → Operator Word32 #-}
{-# SPECIALIZE INLINE orthogonalizeWithReducedEschelonForm :: ReducedEschelonForm Word64 → Operator Word64 → Operator Word64 #-}
{-# SPECIALIZE INLINE orthogonalizeWithReducedEschelonForm :: ReducedEschelonForm Integer → Operator Integer → Operator Integer #-}
{-# INLINE orthogonalizeWithReducedEschelonForm #-}
-- }}}

-- }}} Functions
