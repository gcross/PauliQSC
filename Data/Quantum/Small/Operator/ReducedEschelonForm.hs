-- Language extensions {{{

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}

-- }}} Language extensions

module Data.Quantum.Small.Operator.ReducedEschelonForm where

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

import Data.Quantum.Small.Operator

-- }}} Imports

-- Types {{{

data PseudoGenerator = -- {{{
    PGX {-# UNPACK #-} !Operator
  | PGZ {-# UNPACK #-} !Operator
  | PGXZ {-# UNPACK #-} !Operator {-# UNPACK #-} !Operator
  deriving (Eq,Ord,Show)
-- }}}

newtype ReducedEschelonForm = -- {{{
    ReducedEschelonForm { unwrapReducedEschelonForm :: IntMap PseudoGenerator }
    deriving (Eq,Ord,Show)
-- }}}

-- }}} Types

-- Instances {{{

instance Monoid ReducedEschelonForm where -- {{{
    mempty = ReducedEschelonForm mempty
    x `mappend` y = addAllToReducedEschelonForm (operatorsInReducedEschelonForm y) x
-- }}}

instance NFData ReducedEschelonForm

-- }}} Instances

-- Functions {{{

addToReducedEschelonForm :: Operator → ReducedEschelonForm → ReducedEschelonForm -- {{{
addToReducedEschelonForm op form = fst (addToReducedEschelonFormWithSuccessTag op form)
{-# INLINE addToReducedEschelonForm #-}
-- }}}

addAllToReducedEschelonForm :: [Operator] → ReducedEschelonForm → ReducedEschelonForm -- {{{
addAllToReducedEschelonForm operators form =
    foldl'
        (flip addToReducedEschelonForm)
        form
        operators
{-# INLINE addAllToReducedEschelonForm #-}
-- }}}

addToReducedEschelonFormWithSuccessTag :: Operator → ReducedEschelonForm → (ReducedEschelonForm, Bool) -- {{{
addToReducedEschelonFormWithSuccessTag original_operator original_form = (new_form,not contained)
  where
    op = orthogonalizeWithReducedEschelonForm original_form original_operator
    contained = isIdentity op
    new_form
     | contained = original_form
     | otherwise = go (IntMap.assocs . unwrapReducedEschelonForm $ original_form)

    addToForm :: Int → PseudoGenerator → ReducedEschelonForm
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
-- }}}

constructReducedEschelonForm :: [Operator] → ReducedEschelonForm -- {{{
constructReducedEschelonForm = flip addAllToReducedEschelonForm mempty
-- }}}

mapPseudoGenerator :: (Operator → Operator) → PseudoGenerator → PseudoGenerator -- {{{
mapPseudoGenerator f (PGX op) = PGX (f op)
mapPseudoGenerator f (PGZ op) = PGZ (f op)
mapPseudoGenerator f (PGXZ opx opz) = PGXZ (f opx) (f opz)
{-# INLINE mapPseudoGenerator #-}
-- }}}

makeSingletonPseudoGeneratorFromColumn :: Int → Operator → PseudoGenerator -- {{{
makeSingletonPseudoGeneratorFromColumn column op =
    case getPauliAt column op of
        X → PGX op
        Y → PGX op
        Z → PGZ op
        _ → error $ "tried to make a pseudo-generator using trivial column " ++ show column ++ " of operator " ++ show op
{-# INLINE makeSingletonPseudoGeneratorFromColumn #-}
-- }}}

numberOfOperatorsInPseudoGenerator :: PseudoGenerator → Int -- {{{
numberOfOperatorsInPseudoGenerator (PGX _) = 1
numberOfOperatorsInPseudoGenerator (PGZ _) = 1
numberOfOperatorsInPseudoGenerator (PGXZ _ _) = 2
{-# INLINE numberOfOperatorsInPseudoGenerator #-}
-- }}}

numberOfOperatorsInReducedEschelonForm :: ReducedEschelonForm → Int -- {{{
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

operatorsInPseudoGenerator :: PseudoGenerator → [Operator] -- {{{
operatorsInPseudoGenerator (PGX op) = [op]
operatorsInPseudoGenerator (PGZ op) = [op]
operatorsInPseudoGenerator (PGXZ opx opz) = [opx,opz]
{-# INLINE operatorsInPseudoGenerator #-}
-- }}}

operatorsInReducedEschelonForm :: ReducedEschelonForm → [Operator] -- {{{
operatorsInReducedEschelonForm =
    concatMap operatorsInPseudoGenerator
    .
    IntMap.elems
    .
    unwrapReducedEschelonForm
{-# INLINE operatorsInReducedEschelonForm #-}
-- }}}

orthogonalizeWithPseudoGenerators :: [(Int,PseudoGenerator)] → Operator → Operator -- {{{
orthogonalizeWithPseudoGenerators _ op@(Operator 0 0) = op
orthogonalizeWithPseudoGenerators pseudogenerators op = go pseudogenerators op
  where
    go [] op = op
    go ((column,pseudo_generator):rest) op = go rest (orthogonalizeWithPseudoGeneratorAt column pseudo_generator op) 
{-# INLINE orthogonalizeWithPseudoGenerators #-}
-- }}}

orthogonalizeWithPseudoGeneratorAt :: Int → PseudoGenerator → Operator → Operator -- {{{
orthogonalizeWithPseudoGeneratorAt column (PGX op) = multiplyByIfHasXBitAt column op
orthogonalizeWithPseudoGeneratorAt column (PGZ op) = multiplyByIfHasZBitAt column op
orthogonalizeWithPseudoGeneratorAt column (PGXZ ox oz) = multiplyByIfHasXZBitAt column ox oz
{-# INLINE orthogonalizeWithPseudoGeneratorAt #-}
-- }}}

orthogonalizeWithReducedEschelonForm :: ReducedEschelonForm → Operator → Operator -- {{{
orthogonalizeWithReducedEschelonForm = orthogonalizeWithPseudoGenerators . IntMap.toList . unwrapReducedEschelonForm
{-# INLINE orthogonalizeWithReducedEschelonForm #-}
-- }}}

-- }}} Functions
