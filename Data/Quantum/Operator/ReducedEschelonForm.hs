-- Language extensions {{{

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}

-- }}} Language extensions

module Data.Quantum.Operator.ReducedEschelonForm where

-- Imports {{{

import Control.Monad (MonadPlus(..))

import Data.Bits
import Data.Foldable (msum)
import Data.Foldable as Fold
import Data.Monoid (mappend)
import Data.Sequence (Seq,viewl,ViewL(..),(|>),(<|))
import Data.Sequence as Seq

import Data.Quantum.Operator

-- }}} Imports

-- Types {{{

-- PseudoGenerator {{{

data PseudoGenerator α = PseudoGenerator
    {   pgColumn :: Int
    ,   pgOperator1 :: Operator α
    ,   pgMaybeOperator2 :: Maybe (Operator α)
    }

-- }}}

type ReducedEschelonForm α = Seq (PseudoGenerator α)

-- }}} Types

-- Functions {{{

addToReducedEschelonForm :: Bits α ⇒ Operator α → ReducedEschelonForm α → Maybe (ReducedEschelonForm α) -- {{{
addToReducedEschelonForm original_operator original_form = go original_operator Seq.empty original_form
  where
    go (Operator 0 0) _ _ = Nothing
    go o _ (viewl → EmptyL) =
        fmap
            (\column → PseudoGenerator column o Nothing <| original_form)
            (maybeFirstNonTrivialColumnOf o)
    go o accum (viewl → generator@(PseudoGenerator{..}) :< rest) =
        case pgMaybeOperator2 of
            Just pgOperator2 →
                go  (multiplyByIfAntiCommuteAt pgColumn pgOperator2
                     .
                     multiplyByIfAntiCommuteAt pgColumn pgOperator1
                     $
                     o
                    )
                    (accum |> generator)
                    rest
            Nothing →
                let new_o = multiplyByIfAntiCommuteAt pgColumn pgOperator1 o
                in if nonTrivialAt pgColumn new_o
                    then Just $
                        PseudoGenerator
                            pgColumn
                            pgOperator1
                            (Just (orthogonalizeWithReducedEschelonForm new_o rest))
                        <| (accum >< rest)
                    else go new_o (accum |> generator) rest
-- }}}

generateFromPseudoGenerator :: (Bits α, MonadPlus m) ⇒ PseudoGenerator α → m (Operator α) -- {{{
generateFromPseudoGenerator (PseudoGenerator _ o Nothing) = return o
generateFromPseudoGenerator (PseudoGenerator _ o1 (Just o2)) = msum . map return $ [o1,o2,o1 `mappend` o2]
-- }}}

orthogonalizeWithPseudoGenerator :: Bits α ⇒ Operator α → PseudoGenerator α → Operator α -- {{{
orthogonalizeWithPseudoGenerator o PseudoGenerator{..} =
    maybe id (multiplyByIfAntiCommuteAt pgColumn) pgMaybeOperator2
    .
    multiplyByIfAntiCommuteAt pgColumn pgOperator1
    $
    o
-- }}}

orthogonalizeWithReducedEschelonForm :: Bits α ⇒ Operator α → ReducedEschelonForm α → Operator α -- {{{
orthogonalizeWithReducedEschelonForm o = Fold.foldl' orthogonalizeWithPseudoGenerator o
-- }}}

-- }}} Functions
