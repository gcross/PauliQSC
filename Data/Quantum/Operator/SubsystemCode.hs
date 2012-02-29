 -- Language extensions {{{

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

-- }}} Language extensions

module Data.Quantum.Operator.SubsystemCode where

-- Imports {{{

import Control.Arrow ((***))
import Control.DeepSeq (NFData(..))

import Data.Bits (Bits(),bit)
import Data.List (foldl')
import Data.Monoid (mempty)
import Data.Word

import Data.Quantum.Operator
import Data.Quantum.Operator.Qubit
import Data.Quantum.Operator.ReducedEschelonForm

-- }}} Imports

-- Types {{{

data SubsystemCode α = SubsystemCode
    {   subsystemCodeMeasurements :: ReducedEschelonForm α
    ,   subsystemCodeStabilizers :: [Operator α]
    ,   subsystemCodeStabilizersCount :: !Int
    ,   subsystemCodeGaugeQubits :: [Qubit α]
    ,   subsystemCodeGaugeQubitsCount :: !Int
    ,   subsystemCodeLogicalQubits :: [Qubit α]
    ,   subsystemCodeLogicalQubitsCount :: !Int
    } deriving (Eq,Ord,Show)

-- }}} Types

-- Instances {{{

instance NFData (SubsystemCode α) where
    rnf (SubsystemCode a b c d e f g) = a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` g `seq` ()

-- }}}

-- Functions {{{

addAllToSubsystemCode :: Bits α ⇒ [Operator α] → SubsystemCode α → SubsystemCode α -- {{{
addAllToSubsystemCode [] = id
addAllToSubsystemCode (operator:rest) = addAllToSubsystemCode rest . addToSubsystemCode operator
{-# SPECIALIZE addAllToSubsystemCode :: [Operator Word8] → SubsystemCode Word8 → SubsystemCode Word8 #-}
{-# SPECIALIZE addAllToSubsystemCode :: [Operator Word16] → SubsystemCode Word16 → SubsystemCode Word16 #-}
{-# SPECIALIZE addAllToSubsystemCode :: [Operator Word32] → SubsystemCode Word32 → SubsystemCode Word32 #-}
{-# SPECIALIZE addAllToSubsystemCode :: [Operator Word64] → SubsystemCode Word64 → SubsystemCode Word64 #-}
{-# SPECIALIZE addAllToSubsystemCode :: [Operator Integer] → SubsystemCode Integer → SubsystemCode Integer #-}
-- }}}

addToSubsystemCode :: Bits α ⇒ Operator α → SubsystemCode α → SubsystemCode α -- {{{
addToSubsystemCode op code = fst (addToSubsystemCodeWithSuccessTag op code)
{-# SPECIALIZE addToSubsystemCode :: Operator Word8 → SubsystemCode Word8 → SubsystemCode Word8 #-}
{-# SPECIALIZE addToSubsystemCode :: Operator Word16 → SubsystemCode Word16 → SubsystemCode Word16 #-}
{-# SPECIALIZE addToSubsystemCode :: Operator Word32 → SubsystemCode Word32 → SubsystemCode Word32 #-}
{-# SPECIALIZE addToSubsystemCode :: Operator Word64 → SubsystemCode Word64 → SubsystemCode Word64 #-}
{-# SPECIALIZE addToSubsystemCode :: Operator Integer → SubsystemCode Integer → SubsystemCode Integer #-}
-- }}}

addToSubsystemCodeWithSuccessTag :: Bits α ⇒ Operator α → SubsystemCode α → (SubsystemCode α,Bool) -- {{{
addToSubsystemCodeWithSuccessTag op old_code@SubsystemCode{..} =
  case addToReducedEschelonFormWithSuccessTag op subsystemCodeMeasurements of
      (_,False) → (old_code,False)
      (new_measurements,True) → (SubsystemCode
        {   subsystemCodeMeasurements = new_measurements
        ,   subsystemCodeStabilizers = new_stabilizers
        ,   subsystemCodeStabilizersCount = new_stabilizers_count
        ,   subsystemCodeGaugeQubits = new_gauge_qubits
        ,   subsystemCodeGaugeQubitsCount = new_gauge_qubits_count
        ,   subsystemCodeLogicalQubits = new_logical_qubits
        ,   subsystemCodeLogicalQubitsCount = new_logical_qubits_count
        },True)
          where
            op_commuting_with_gauge_qubits =
                foldl' 
                    makeOperatorCommuteWithQubit
                    op
                    subsystemCodeGaugeQubits

            (maybe_conjugal_partner,new_stabilizers) =
                go subsystemCodeStabilizers []
              where
                go [] new_stabilizers =
                    (Nothing, op_commuting_with_gauge_qubits:new_stabilizers)
                go (stabilizer:rest) new_stabilizers
                 | op_commuting_with_gauge_qubits `commute` stabilizer =
                    go rest (stabilizer:new_stabilizers)
                 | otherwise =
                    (Just stabilizer
                    ,(new_stabilizers ++)
                     .
                     map
                        (multiplyByIfAntiCommuteWith
                            stabilizer
                            op_commuting_with_gauge_qubits
                        )
                     $
                     rest
                    )

            (new_stabilizers_count,(new_gauge_qubits_count,new_logical_qubits_count)) =
                case maybe_conjugal_partner of
                    Nothing → ((\i→i+1) *** id *** (\i→i-1))
                    Just _ →  ((\i→i-1) *** (\i→i+1) *** id)
                $
                (subsystemCodeStabilizersCount,(subsystemCodeGaugeQubitsCount,subsystemCodeLogicalQubitsCount))

            new_gauge_qubits =
                maybe
                    id
                    ((:) . Qubit op_commuting_with_gauge_qubits)
                    maybe_conjugal_partner
                $
                subsystemCodeGaugeQubits

            new_logical_qubits =
                case maybe_conjugal_partner of
                    Just former_stabilizer →
                        map (multiplyQubitByIfAntiCommuteWith former_stabilizer op_commuting_with_gauge_qubits)
                    Nothing → go []
                      where
                        go _ [] = error "internal consistency error:  increasing a new stabilizer did not decrease the number of logical qubits"
                        go new_logical_qubits (qubit@(Qubit x z):rest) =
                            case (x `commute` op_commuting_with_gauge_qubits, z `commute` op_commuting_with_gauge_qubits) of
                                (True,True) → go (qubit:new_logical_qubits) rest
                                (False,_) → new_logical_qubits ++ map (multiplyQubitByIfAntiCommuteWith x op_commuting_with_gauge_qubits) rest
                                (_,False) → new_logical_qubits ++ map (multiplyQubitByIfAntiCommuteWith z op_commuting_with_gauge_qubits) rest
                $
                subsystemCodeLogicalQubits
{-# SPECIALIZE addToSubsystemCodeWithSuccessTag :: Operator Word8 → SubsystemCode Word8 → (SubsystemCode Word8, Bool) #-}
{-# SPECIALIZE addToSubsystemCodeWithSuccessTag :: Operator Word16 → SubsystemCode Word16 → (SubsystemCode Word16, Bool) #-}
{-# SPECIALIZE addToSubsystemCodeWithSuccessTag :: Operator Word32 → SubsystemCode Word32 → (SubsystemCode Word32, Bool) #-}
{-# SPECIALIZE addToSubsystemCodeWithSuccessTag :: Operator Word64 → SubsystemCode Word64 → (SubsystemCode Word64, Bool) #-}
{-# SPECIALIZE addToSubsystemCodeWithSuccessTag :: Operator Integer → SubsystemCode Integer → (SubsystemCode Integer, Bool) #-}
-- }}}

constructSubsystemCodeFromMeasurements :: Bits α ⇒ Int → [Operator α] → SubsystemCode α -- {{{
constructSubsystemCodeFromMeasurements number_of_physical_qubits operators = addAllToSubsystemCode operators (initialSubsystemCode number_of_physical_qubits)
{-# SPECIALIZE constructSubsystemCodeFromMeasurements :: Int → [Operator Word8] → SubsystemCode Word8 #-}
{-# SPECIALIZE constructSubsystemCodeFromMeasurements :: Int → [Operator Word16] → SubsystemCode Word16 #-}
{-# SPECIALIZE constructSubsystemCodeFromMeasurements :: Int → [Operator Word32] → SubsystemCode Word32 #-}
{-# SPECIALIZE constructSubsystemCodeFromMeasurements :: Int → [Operator Word64] → SubsystemCode Word64 #-}
{-# SPECIALIZE constructSubsystemCodeFromMeasurements :: Int → [Operator Integer] → SubsystemCode Integer #-}
-- }}}

initialSubsystemCode :: Bits α ⇒ Int → SubsystemCode α -- {{{
initialSubsystemCode number_of_physical_qubits = SubsystemCode{..}
  where
    subsystemCodeMeasurements = mempty
    subsystemCodeStabilizers = []
    subsystemCodeStabilizersCount = 0
    subsystemCodeGaugeQubits = []
    subsystemCodeGaugeQubitsCount = 0
    subsystemCodeLogicalQubits = [Qubit (Operator (bit i) 0) (Operator 0 (bit i)) | i ← [0..number_of_physical_qubits-1]]
    subsystemCodeLogicalQubitsCount = number_of_physical_qubits 
{-# SPECIALIZE initialSubsystemCode :: Int → SubsystemCode Word8 #-}
{-# SPECIALIZE initialSubsystemCode :: Int → SubsystemCode Word16 #-}
{-# SPECIALIZE initialSubsystemCode :: Int → SubsystemCode Word32 #-}
{-# SPECIALIZE initialSubsystemCode :: Int → SubsystemCode Word64 #-}
{-# SPECIALIZE initialSubsystemCode :: Int → SubsystemCode Integer #-}
-- }}}

numberOfMeasurementOperatorsInCode :: SubsystemCode α → Int -- {{{
numberOfMeasurementOperatorsInCode SubsystemCode{..} = subsystemCodeStabilizersCount + 2*subsystemCodeGaugeQubitsCount
-- }}}

numberOfPhysicalQubitsInCode :: SubsystemCode α → Int -- {{{
numberOfPhysicalQubitsInCode SubsystemCode{..} = subsystemCodeStabilizersCount + subsystemCodeGaugeQubitsCount + subsystemCodeLogicalQubitsCount
-- }}}

-- }}} Functions
