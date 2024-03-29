 -- Language extensions {{{

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

-- }}} Language extensions

module Data.Quantum.Small.Operator.SubsystemCode where

-- Imports {{{

import Control.Arrow ((***))
import Control.DeepSeq (NFData(..))

import Data.Bits (Bits(),bit)
import Data.List (foldl')
import Data.Monoid (mempty)
import Data.Word

import Data.Quantum.Small.Operator
import Data.Quantum.Small.Operator.Qubit
import Data.Quantum.Small.Operator.ReducedEschelonForm

-- }}} Imports

-- Types {{{

data SubsystemCode = SubsystemCode
    {   subsystemCodeMeasurements :: ReducedEschelonForm
    ,   subsystemCodeStabilizers :: [Operator]
    ,   subsystemCodeStabilizersCount :: !Int
    ,   subsystemCodeGaugeQubits :: [Qubit]
    ,   subsystemCodeGaugeQubitsCount :: !Int
    ,   subsystemCodeLogicalQubits :: [Qubit]
    ,   subsystemCodeLogicalQubitsCount :: !Int
    } deriving (Eq,Ord,Show)

-- }}} Types

-- Instances {{{

instance NFData (SubsystemCode) where
    rnf (SubsystemCode a b c d e f g) = a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` g `seq` ()

-- }}}

-- Functions {{{

addAllToSubsystemCode :: [Operator] → SubsystemCode → SubsystemCode -- {{{
addAllToSubsystemCode [] = id
addAllToSubsystemCode (operator:rest) = addAllToSubsystemCode rest . addToSubsystemCode operator
-- }}}

addToSubsystemCode :: Operator → SubsystemCode → SubsystemCode -- {{{
addToSubsystemCode op code = fst (addToSubsystemCodeWithSuccessTag op code)
-- }}}

addToSubsystemCodeWithSuccessTag :: Operator → SubsystemCode → (SubsystemCode,Bool) -- {{{
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
-- }}}

constructSubsystemCodeFromMeasurements :: Int → [Operator] → SubsystemCode -- {{{
constructSubsystemCodeFromMeasurements number_of_physical_qubits operators = addAllToSubsystemCode operators (initialSubsystemCode number_of_physical_qubits)
-- }}}

initialSubsystemCode :: Int → SubsystemCode -- {{{
initialSubsystemCode number_of_physical_qubits = SubsystemCode{..}
  where
    subsystemCodeMeasurements = mempty
    subsystemCodeStabilizers = []
    subsystemCodeStabilizersCount = 0
    subsystemCodeGaugeQubits = []
    subsystemCodeGaugeQubitsCount = 0
    subsystemCodeLogicalQubits = [Qubit (Operator (bit i) 0) (Operator 0 (bit i)) | i ← [0..number_of_physical_qubits-1]]
    subsystemCodeLogicalQubitsCount = number_of_physical_qubits
-- }}}

numberOfMeasurementOperatorsInCode :: SubsystemCode → Int -- {{{
numberOfMeasurementOperatorsInCode SubsystemCode{..} = subsystemCodeStabilizersCount + 2*subsystemCodeGaugeQubitsCount
-- }}}

numberOfPhysicalQubitsInCode :: SubsystemCode → Int -- {{{
numberOfPhysicalQubitsInCode SubsystemCode{..} = subsystemCodeStabilizersCount + subsystemCodeGaugeQubitsCount + subsystemCodeLogicalQubitsCount
-- }}}

-- }}} Functions
