-- Language extensions {{{

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}

-- }}} Language extensions

module Data.Quantum.Operator.ReducedEschelonForm where

-- Imports {{{

import Control.Arrow (first)
import Control.Monad (MonadPlus(..))

import Data.Bits
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Monoid (Monoid(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Quantum.Operator

-- }}} Imports

-- Types {{{

data PseudoGenerator α = PseudoGenerator -- {{{
    {   pgo1 :: Operator α
    ,   maybe_pgo2 :: Maybe (Operator α)
    }
-- }}}

newtype ReducedEschelonForm α = ReducedEschelonForm { unwrapReducedEschelonForm :: IntMap (PseudoGenerator α) }

-- }}} Types

-- Instances {{{

instance Bits α => Monoid (ReducedEschelonForm α) where -- {{{
    mempty = ReducedEschelonForm mempty
    x `mappend` y = -- {{{
        new_form
      where
        splitSingletsFromDoublets =
            IntMap.mapEither (\x@(PseudoGenerator pg1 maybe_pg2) →
                case maybe_pg2 of
                    Nothing → Left pg1
                    Just pg2 → Right x
            )
            .
            unwrapReducedEschelonForm
        (x_singlets, x_doublets) = splitSingletsFromDoublets x
        (y_singlets, y_doublets) = splitSingletsFromDoublets y

        leftover_doublets = x_doublets `IntMap.difference` y_doublets
        partial_form = ReducedEschelonForm (x_doublets `IntMap.union` y_doublets)

        leftover_operators = concat $
            IntMap.elems x_singlets
           :IntMap.elems y_singlets
           :map operatorsInPseudoGenerator (IntMap.elems leftover_doublets)

        new_form =
            foldl'
                (flip addToReducedEschelonForm)
                partial_form
                leftover_operators
    -- }}}
-- }}}

-- }}} Instances

-- Functions {{{

addToReducedEschelonForm :: Bits α ⇒ Operator α → ReducedEschelonForm α → ReducedEschelonForm α -- {{{
addToReducedEschelonForm op form = fst (addToReducedEschelonFormWithSuccessTag op form)
-- }}}

addToReducedEschelonFormWithSuccessTag :: Bits α ⇒ Operator α → ReducedEschelonForm α → (ReducedEschelonForm α, Bool) -- {{{
addToReducedEschelonFormWithSuccessTag original_operator (ReducedEschelonForm original_form) =
    first ReducedEschelonForm (go original_operator (IntMap.toList original_form))
  where
    go (Operator 0 0) _ = (original_form, False)
    go o [] = (IntMap.insert (fromJust $ maybeFirstNonTrivialColumnOf o) (PseudoGenerator o Nothing) original_form, True)
    go o ((column,PseudoGenerator{..}):rest) =
        case maybe_pgo2 of
            Just pgo2 →
                go  (multiplyByIfAntiCommuteAt column pgo2
                     .
                     multiplyByIfAntiCommuteAt column pgo1
                     $
                     o
                    )
                    rest
            Nothing →
                let new_o = multiplyByIfAntiCommuteAt column pgo1 o
                in if nonTrivialAt column new_o
                    then
                        (flip (IntMap.insert column) original_form
                         .
                         PseudoGenerator pgo1
                         .
                         Just
                         .
                         orthogonalizeWithPseudoGenerators rest
                         $
                         new_o
                        ,True
                        )
                    else
                        go new_o rest
-- }}}

operatorsInPseudoGenerator :: PseudoGenerator α → [Operator α] -- {{{
operatorsInPseudoGenerator (PseudoGenerator pgo1 Nothing) = [pgo1]
operatorsInPseudoGenerator (PseudoGenerator pgo1 (Just pgo2)) = [pgo1,pgo2]
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
orthogonalizeWithPseudoGenerators = flip $ foldl' (flip (uncurry orthogonalizeWithPseudoGeneratorAt))
-- }}}

orthogonalizeWithPseudoGeneratorAt :: Bits α ⇒ Int → PseudoGenerator α → Operator α → Operator α -- {{{
orthogonalizeWithPseudoGeneratorAt column PseudoGenerator{..} =
    maybe id (multiplyByIfAntiCommuteAt column) maybe_pgo2 
    .
    multiplyByIfAntiCommuteAt column pgo1
-- }}}

orthogonalizeWithReducedEschelonForm :: Bits α ⇒ ReducedEschelonForm α → Operator α → Operator α -- {{{
orthogonalizeWithReducedEschelonForm = orthogonalizeWithPseudoGenerators . IntMap.toList . unwrapReducedEschelonForm
-- }}}

-- }}} Functions
