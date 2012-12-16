-- Language extensions {{{

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

-- }}} Language extensions

-- Imports {{{

import Control.Applicative
import Control.Monad

import Data.Bits
import Data.Function (on)
import Data.Functor ((<$))
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.List (delete,subsequences,sort)
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set
import Data.Word

import System.IO.Unsafe (unsafePerformIO)

import Text.Printf

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Debug.Trace

import Data.Quantum.Small.Operator
import Data.Quantum.Small.Operator.Qubit
import Data.Quantum.Small.Operator.ReducedEschelonForm
import Data.Quantum.Small.Operator.SubsystemCode

-- }}} Imports

-- Types {{{

data NonTrivialOperatorAndSize = NonTrivialOperatorAndSize Operator Int deriving (Eq)

data OperatorAndSize = OperatorAndSize Operator Int deriving (Eq,Show)

data OperatorAndSizeAndIndex = OperatorAndSizeAndIndex Operator Int Int deriving (Eq,Show)

-- }}} Types

-- Instances {{{

-- Arbitrary {{{

instance Arbitrary Pauli where arbitrary = elements [I,X,Y,Z]

instance Arbitrary NonTrivialOperatorAndSize where -- {{{
    arbitrary = do
        n ← choose (1,bitSize (undefined :: Word))
        o ← generateNonTrivialOperatorOfSize n
        return $ NonTrivialOperatorAndSize o n
-- }}}

instance Arbitrary Operator where -- {{{
    arbitrary =
        fmap fromPauliList
             (resize (bitSize (undefined :: Word)) (listOf arbitrary))
-- }}}

instance Arbitrary OperatorAndSize where -- {{{
    arbitrary = do
        n ← choose (0,bitSize (undefined :: Word))
        o ← generateOperatorOfSize n
        return $ OperatorAndSize o n
-- }}}

instance Arbitrary OperatorAndSizeAndIndex where -- {{{
    arbitrary = do
        n ← choose (1,bitSize (undefined :: Word))
        i ← choose (0,n-1)
        o ← generateOperatorOfSize n
        return $ OperatorAndSizeAndIndex o n i
-- }}}

-- }}} Arbitrary

instance Show Operator ⇒ Show NonTrivialOperatorAndSize where
    show (NonTrivialOperatorAndSize op size) = printf ("[size = %i, operator = '%s']") size (show op)

-- }}} Instances

-- Functions {{{

assertAntiCommute :: String → Operator → Operator → Assertion -- {{{
assertAntiCommute message a b =
    assertBool
        (message ++ " {" ++ show a ++ "," ++ show b ++ "}")
        (antiCommute a b)
-- }}}

assertCommute :: String → Operator → Operator → Assertion -- {{{
assertCommute message a b =
    assertBool
        (message ++ " [" ++ show a ++ "," ++ show b ++ "]")
        (commute a b)
-- }}}

assertNotEqual :: (Eq a, Show a) => String -- ^ The message prefix -- {{{
                                 -> a      -- ^ The expected value
                                 -> a      -- ^ The actual value
                                 -> Assertion
assertNotEqual preface expected actual =
  when (actual == expected) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected to *not* get: " ++ show expected ++ "\n"
-- }}}

generateNonTrivialOperatorOfSize :: Int → Gen (Operator) -- {{{
generateNonTrivialOperatorOfSize 0 = error "it is impossible to generate a non-trivial operator of size zero"
generateNonTrivialOperatorOfSize n =
    let upper_bound = bit n - 1
        go = do
            x ← choose (0,upper_bound::Int)
            z ← choose (0,upper_bound::Int)
            if (x == 0) && (z == 0)
                then go
                else return (fromIntegral x,fromIntegral z)
    in fmap (uncurry Operator) go
-- }}}

generateOperatorOfSize :: Int → Gen (Operator) -- {{{
generateOperatorOfSize n =
    let upper_bound = bit n - 1
    in liftM2 Operator
        (fmap fromIntegral $ choose (0,upper_bound :: Int))
        (fmap fromIntegral $ choose (0,upper_bound))
-- }}}

validateCode number_of_physical_qubits code@SubsystemCode{..} = do -- {{{
  -- Check that all the cached counts are valid {{{
    assertEqual
        "number of stabilizers matches cached count"
        (length subsystemCodeStabilizers)
        subsystemCodeStabilizersCount
    assertEqual
        "number of gauge qubits matches cached count"
        (length subsystemCodeGaugeQubits)
        subsystemCodeGaugeQubitsCount
    assertEqual
        "number of logical qubits matches cached count"
        (length subsystemCodeLogicalQubits)
        subsystemCodeLogicalQubitsCount
    assertEqual
        "number of physical qubits in the code is valid"
        number_of_physical_qubits
        (numberOfPhysicalQubitsInCode code)
  -- }}}
  -- Check that the stabilizers commute with all other operators {{{
    forM_ subsystemCodeStabilizers $ \stabilizer → do
        forM_ subsystemCodeStabilizers $ \other_stabilizer → do
            assertCommute
                "stabilizers"
                stabilizer
                other_stabilizer
        forM_ all_qubit_operators $ \(qubit_type,_,opname,op) →
                assertCommute
                    ("stabilizer with " ++ qubit_type ++ " qubit's " ++ opname ++ " operator")
                    stabilizer
                    op
  -- }}}
  -- Check that each qubit is a pair of anti-commuting operators {{{
    forM_ all_qubits $ \(qubit_type, qubit_index, Qubit x z) →
        assertAntiCommute
            (printf "%ith %s qubit" qubit_index qubit_type)
            x z
  -- }}}
  -- Check that the qubits all commute -- {{{
    sequence $
        [assertCommute
            (printf
                "%ith %s qubit with %ith %s qubit"
                qubit_index_1 qubit_type_1
                qubit_index_2 qubit_type_2
            )
            op1
            op2
        |(qubit_type_1,qubit_index_1,opname1,op1) ← all_qubit_operators
        ,(qubit_type_2,qubit_index_2,opname2,op2) ← all_qubit_operators
        ,qubit_type_1 /= qubit_type_2 || qubit_index_1 /= qubit_index_2
        ]
  -- }}}
  -- Check that every measurement operator is in the reduced eschelon form {{{
    forM_ (zip [(0::Int)..] subsystemCodeStabilizers) $ \(i,stabilizer) →
        assertEqual
            (printf "%ith stabilizer (%s) is in the reduced eschelon form" i (show stabilizer))
            mempty
            (orthogonalizeWithMeasurements stabilizer)
    forM_ gauge_qubit_operators $ \(qubit_type,qubit_index,opname,op) →
        assertEqual
            (printf "%s operator of %ith %s qubit (%s) is in the reduced eschelon form" opname qubit_index qubit_type (show op))
            mempty
            (orthogonalizeWithMeasurements op)
    assertEqual
        "number of operators in the reduced eschelon form"
        (numberOfMeasurementOperatorsInCode code)
        (numberOfOperatorsInReducedEschelonForm subsystemCodeMeasurements)
  -- }}}
  -- Check that every logical qubit operator is *not* in the reduced eschelon form {{{
    forM_ logical_qubit_operators $ \(qubit_type,qubit_index,opname,op) →
        assertNotEqual
            (printf "%s operator of %ith %s qubit (%s) is in the reduced eschelon form" opname qubit_index qubit_type (show op))
            mempty
            (orthogonalizeWithMeasurements op)
  -- }}}
  where
    tagQubits qubit_type qubits = zip3 (repeat qubit_type) [(0::Int)..] qubits
    gauge_qubits = tagQubits "gauge" subsystemCodeGaugeQubits
    logical_qubits = tagQubits "logical" subsystemCodeLogicalQubits
    all_qubits = gauge_qubits ++ logical_qubits

    gatherQubitOperators qubits = -- {{{
        [ (qubit_type, qubit_index, opname, op)
        | (qubit_type, qubit_index, Qubit x z) ← qubits
        , (opname, op) ← [("X",x),("Z",z)]
        ]
    -- }}}
    gauge_qubit_operators = gatherQubitOperators gauge_qubits
    logical_qubit_operators = gatherQubitOperators logical_qubits
    all_qubit_operators = gauge_qubit_operators ++ logical_qubit_operators

    orthogonalizeWithMeasurements op = orthogonalizeWithReducedEschelonForm subsystemCodeMeasurements op
-- }}}

-- }}} Functions

main = defaultMain
    -- Tests {{{
    [testGroup "Data.Quantum.Operator" -- {{{
        [testGroup "Functions" $ -- {{{
            [testProperty "agreeAt" $ \o1 o2 → do -- {{{
                i ← choose (0,7)
                return $ agreeAt i o1 o2 == (getPauliAt i o1 == getPauliAt i o2)
             -- }}}
            ,testProperty "antiCommuteAt" $ do -- {{{
                n ← choose (1,8)
                components1 :: [Pauli] ← vector n
                components2 :: [Pauli] ← vector n
                i ← choose (0,n-1)
                return $
                    antiCommuteAt i (fromPauliList components1) (fromPauliList components2)
                 == antiCommute (components1 !! i) (components2 !! i)
             -- }}}
            ,testProperty "commuteAt" $ do -- {{{
                n ← choose (1,8)
                components1 :: [Pauli] ← vector n
                components2 :: [Pauli] ← vector n
                i ← choose (0,n-1)
                return $
                    commuteAt i (fromPauliList components1) (fromPauliList components2)
                 == commute (components1 !! i) (components2 !! i)
             -- }}}
            ,testGroup "fromPauliList" -- {{{
                [testCase "identity" $ forM_ [0..8] $ \n → Operator 0 0 @=? fromPauliList (replicate n I)
                ,testCase "IXYZ" $ Operator 6 12 @=? fromPauliList [I,X,Y,Z]
                ,testProperty ". toPauliList = identity function" $ \(OperatorAndSize op n) → -- {{{
                    liftA2 (==) id (fromPauliList . toPauliList n) op
                 -- }}}
                ]
             -- }}}
            ,testProperty "getPauliAt" $ \(OperatorAndSizeAndIndex o n i) → -- {{{
                getPauliAt i o == (toPauliList n o) !! i
             -- }}}
            ,testCase "has(X/Z)Bit" $ -- {{{
                forM_ [I .. Y] $ \pauli → do
                    hasXBit pauli @?= (testBit (fromEnum pauli) 0)
                    hasZBit pauli @?= (testBit (fromEnum pauli) 1)
             -- }}}
            ,testGroup "maybeFirstNonTrivialColumnOf" -- {{{
                [testCase "identity" $ -- {{{
                    forM_ [0..8] $ \n →
                        assertBool ("column " ++ show n) $
                            isNothing (maybeFirstNonTrivialColumnOf . fromPauliList $ replicate n I)
                 -- }}}
                ,testProperty "non-identity" $ do -- {{{
                    n ← choose(1,8)
                    first_non_trivial_column ← choose (0,n-1)
                    operator ←
                        fmap (fromPauliList . concat) . sequence $
                            [return (replicate first_non_trivial_column I)
                            ,fmap (:[]) (elements [X,Y,Z])
                            ,vector (n-first_non_trivial_column-1)
                            ]
                    return $ maybeFirstNonTrivialColumnOf operator == Just first_non_trivial_column
                 -- }}}
                ]
             -- }}}
            ,testProperty "multiplyByIf" $ \b x y → -- {{{
                multiplyByIf b x y == if b then (x `mappend` y) else y
             -- }}}
            ,testProperty "multiplyByIfAntiCommuteAt" $ do -- {{{
                n ← choose (1,8)
                components1 :: [Pauli] ← vector n
                let operator1 = fromPauliList components1
                components2 :: [Pauli] ← vector n
                let operator2 = fromPauliList components2
                i ← choose (0,n-1)
                return $
                    multiplyByIfAntiCommuteAt i operator1 operator2
                 == if antiCommuteAt i operator1 operator2 then (operator1 `mappend` operator2) else operator2
             -- }}}
            ,testProperty "nonTrivialAt" $ \(OperatorAndSizeAndIndex o n i) → -- {{{
                nonTrivialAt i o == (toPauliList n o !! i /= I)
             -- }}}
            ,testProperty "setPauliAt" $ \(OperatorAndSizeAndIndex o _ i) p → -- {{{
                    (getPauliAt i . setPauliAt i p) o == p
             -- }}}
            ,testGroup "toPauliList" -- {{{
                [testCase "identity" $ forM_ [0..8] $ \n → toPauliList n (Operator 0 0) @?= replicate n I
                ,testCase "IXYZ" $ toPauliList 4 (Operator 6 12) @?= [I,X,Y,Z]
                ,testProperty ". fromPauliList = identity function" $ do -- {{{
                    n ← choose(0,8)
                    components :: [Pauli] ← vector n
                    return $ liftA2 (==) id (toPauliList n . fromPauliList) components
                 -- }}}
                ]
             -- }}}
            ]
         -- }}}
        ,testGroup "Instances" -- {{{
            [testGroup "Commutable" -- {{{
                [testGroup "Operator" -- {{{
                    [testProperty "correct property" $ \(x :: Operator, y :: Operator) → commute x y /= antiCommute x y
                    ,testProperty "correct result" $ do -- {{{
                        n ← choose(0,8)
                        components1 :: [Pauli] ← vector n
                        components2 :: [Pauli] ← vector n
                        return $
                            commute (fromPauliList components1) (fromPauliList components2)
                            ==
                            (length (filter id (zipWith antiCommute components1 components2)) `mod` 2 == 0)
                     -- }}}
                    ]
                 -- }}}
                ,testGroup "Pauli" -- {{{
                    [testProperty "correct property" $ \(x :: Pauli, y :: Pauli) → commute x y /= antiCommute x y
                    ,testCase "correct result" $ do -- {{{
                        assertBool "[I,I]" $ commute I I

                        assertBool "[I,X]" $ commute I X
                        assertBool "[I,Y]" $ commute I Y
                        assertBool "[I,Z]" $ commute I Z

                        assertBool "[X,I]" $ commute X I
                        assertBool "[Y,I]" $ commute Y I
                        assertBool "[Z,I]" $ commute Z I

                        assertBool "[X,X]" $ commute X X
                        assertBool "[Y,Y]" $ commute Y Y
                        assertBool "[Z,Z]" $ commute Z Z

                        assertBool "[X,Y]" $ antiCommute X Y
                        assertBool "[Y,Z]" $ antiCommute Y Z
                        assertBool "[Z,X]" $ antiCommute Z X

                        assertBool "[Z,Y]" $ antiCommute Z Y
                        assertBool "[Y,X]" $ antiCommute Y X
                        assertBool "[X,Z]" $ antiCommute X Z
                     -- }}}
                    ]
                 -- }}}
                ]
             -- }}}
            ,testGroup "Monoid" -- {{{
                [testProperty "Operator" $ do -- {{{
                    n ← choose(0,8)
                    components1 :: [Pauli] ← vector n
                    components2 :: [Pauli] ← vector n
                    return $
                        (fromPauliList components1 `mappend` fromPauliList components2)
                        ==
                        fromPauliList (zipWith mappend components1 components2)
                 -- }}}
                ,testCase "Pauli" $ do -- {{{
                    assertEqual "I*I=I" I (I `mappend` I)
                    assertEqual "I*X=X" X (I `mappend` X)
                    assertEqual "I*Y=Y" Y (I `mappend` Y)
                    assertEqual "I*Z=Z" Z (I `mappend` Z)

                    assertEqual "X*I=X" X (X `mappend` I)
                    assertEqual "X*X=I" I (X `mappend` X)
                    assertEqual "X*Y=Z" Z (X `mappend` Y)
                    assertEqual "X*Z=Y" Y (X `mappend` Z)

                    assertEqual "Y*I=Y" Y (Y `mappend` I)
                    assertEqual "Y*X=Z" Z (Y `mappend` X)
                    assertEqual "Y*Y=I" I (Y `mappend` Y)
                    assertEqual "Y*Z=X" X (Y `mappend` Z)

                    assertEqual "Z*I=Z" Z (Z `mappend` I)
                    assertEqual "Z*X=Y" Y (Z `mappend` X)
                    assertEqual "Z*Y=X" X (Z `mappend` Y)
                    assertEqual "Z*Z=I" I (Z `mappend` Z)
                 -- }}}
                ]
             -- }}}
            ,testGroup "Read" -- {{{
                [testProperty "Operator" $ \(op :: Operator) → (read . show) op == op
                ]
             -- }}}
            ]
         -- }}}
        ]
     -- }}}
    ,testGroup "Data.Quantum.Operator.ReducedEschelonForm" -- {{{
        [testProperty "add one operator to an empty form" $ \op → -- {{{
            if op == mempty
            then
                let (ReducedEschelonForm new_form,success) = addToReducedEschelonFormWithSuccessTag op mempty
                in not success
                && IntMap.null new_form
                && Nothing == maybeFirstNonTrivialColumnOf op 
            else
                let (ReducedEschelonForm new_form,success) = addToReducedEschelonFormWithSuccessTag op mempty
                in success
                && IntMap.size new_form == 1
                && Just (head (IntMap.keys new_form)) == maybeFirstNonTrivialColumnOf op 
         -- }}}
        ,testProperty "add two operators with non-trivial first bit to an empty form" $ \o1_ o2_ → do -- {{{
            p1 ← elements [X,Y,Z]
            p2 ← elements (delete p1 [X,Y,Z])
            let o1 = setPauliAt 0 p1 o1_
                o2 = setPauliAt 0 p2 o2_
                (form1,success1) = addToReducedEschelonFormWithSuccessTag o1_ mempty
                (form2,success2) = addToReducedEschelonFormWithSuccessTag o2_ form1
            return . const True . unsafePerformIO $ do
                assertBool "first addition was successful" success1
                assertEqual "size of form after first addition is correct" 1 (IntMap.size (unwrapReducedEschelonForm form1))
                assertBool "second addition was succesful" success1
                assertEqual "size of form after second addition is correct" 1 (IntMap.size (unwrapReducedEschelonForm form2))
                let pseudo_generator = (head . IntMap.elems . unwrapReducedEschelonForm $ form2)
                case pseudo_generator of
                    PGXZ (Operator xx xz) (Operator zx zz) → do
                        assertBool "pseudo-generator x op have a 1 bit at column 0 of its x component" (testBit xx 0)
                        assertBool "pseudo-generator x op have a 0 bit at column 0 of its z component" (testBit xz 0)
                        assertBool "pseudo-generator z op have a 0 bit at column 0 of its z component" (testBit zx 0)
                        assertBool "pseudo-generator z op have a 1 bit at column 0 of its x component" (testBit zz 0)
                    _ → assertFailure $ "pseudo-generator has incorrect form: " ++ show pseudo_generator
         -- }}}
        ,testProperty "add the same operator twice" $ \o → o /= mempty ==> -- {{{
            let form1 = addToReducedEschelonForm o mempty
                (form2,successful) = addToReducedEschelonFormWithSuccessTag o form1
            in form1 == form2 && not successful
         -- }}}
        ,testProperty "every pseudo-generator is independent of all others in its assigned column" $ \operators → -- {{{
            let ReducedEschelonForm form = addAllToReducedEschelonForm operators mempty
            in unsafePerformIO . (>> return True) $
                forM_ (IntMap.assocs form) $ \(i,pg1) →
                    forM_ (IntMap.assocs . IntMap.delete i $ form) $ \(j,pg2) →
                        forM_ (operatorsInPseudoGenerator pg1) $ \op1 →
                            forM_ (operatorsInPseudoGenerator pg2) $ \op2 →
                                assertBool
                                    ("is the pseudo-generator at column " ++ show i ++ " independent from the pseudo-generator at column " ++ show j ++ "? (" ++ show op1 ++ " * " ++ show op2 ++ ")")
                                    (nonTrivialAt i (op1 `mappend` op2))
         -- }}}
        ,testProperty "every pseudo-generator has the correct paulis in the assigned column" $ \operators → -- {{{
            let ReducedEschelonForm form = addAllToReducedEschelonForm operators mempty
            in unsafePerformIO . (>> return True) $ do
                forM_ (IntMap.assocs form) $ \(column,pseudo_generator) → do
                    case pseudo_generator of
                        PGX op →
                            assertBool
                                ("pseudo-generator at column " ++ show column ++ " has X bit set")
                                (hasXBitAt column op)
                        PGZ op →
                            assertBool
                                ("pseudo-generator at column " ++ show column ++ " has Z bit set")
                                (hasZBitAt column op)
                        PGXZ opx opz → do
                            assertEqual
                                ("case PGXZ, X operator, for pseudo-generator at column " ++ show column)
                                X
                                (getPauliAt column opx)
                            assertEqual
                                ("case PGXZ, Z operator, for pseudo-generator at column " ++ show column)
                                Z
                                (getPauliAt column opz)
         -- }}}
        ,testProperty "adding a previously seen operator always fails" $ \operators_ operator → do -- {{{
            pos ← choose (0,length operators_)
            let (l,r) = splitAt pos operators_
                operators = l ++ [operator] ++ r
                form = addAllToReducedEschelonForm operators mempty
            operator ← elements operators
            let (new_form, successful) = addToReducedEschelonFormWithSuccessTag operator form 
            return . unsafePerformIO . (>> return True) $ do
                assertEqual "Is the new form the same as the old form?" form new_form
                assertBool "Was the addition reported as being unsuccessful" (not successful)
         -- }}}
        ,testProperty "the pseudo-generators generate the same set as the original operators" $ do -- {{{
            n ← choose (0,8)
            operators ← vectorOf n arbitrary
            let form = addAllToReducedEschelonForm operators mempty
            return . unsafePerformIO . (>> return True) $
                (on
                    (assertEqual "Does the reduced eschelon form generate the same set as the original?")
                    (Set.fromList . map mconcat . subsequences)
                )
                    operators 
                    (operatorsInReducedEschelonForm form)
         -- }}}
        ,testProperty "the mappend function results in a form that generates the same set of operators" $ do -- {{{
            form1 ← fmap (flip addAllToReducedEschelonForm mempty) (choose (0,4) >>= flip vectorOf arbitrary)
            form2 ← fmap (flip addAllToReducedEschelonForm mempty) (choose (0,4) >>= flip vectorOf arbitrary)
            return . unsafePerformIO . (>> return True) $
                (on
                    (assertEqual "Does the reduced eschelon form generate the same set as the original?")
                    (Set.fromList . map mconcat . subsequences)
                )
                    (operatorsInReducedEschelonForm form1 `mappend` operatorsInReducedEschelonForm form2)
                    (operatorsInReducedEschelonForm (form1 `mappend` form2))
         -- }}}
        ]
     -- }}}
    ,testGroup "Data.Quantum.Operator.SubsystemCode" -- {{{
        [testProperty "initialSubsystemCode" $ do -- {{{
            number_of_physical_qubits ← choose (1,16)
            let code = initialSubsystemCode number_of_physical_qubits
            return . unsafePerformIO . (True <$) $ validateCode number_of_physical_qubits code
         -- }}}
        ,testProperty "adding one operator" $ \(NonTrivialOperatorAndSize op number_of_physical_qubits) → -- {{{
            unsafePerformIO . (True <$) $ do
                let (code@SubsystemCode{..},success) = addToSubsystemCodeWithSuccessTag op (initialSubsystemCode number_of_physical_qubits)
                assertBool "success of adding operator to the code" success
                validateCode number_of_physical_qubits code
                assertEqual "number of stabilizers" 1 subsystemCodeStabilizersCount
                assertEqual "number of gauge qubits" 0 subsystemCodeGaugeQubitsCount
                assertEqual "number of logical qubits" (number_of_physical_qubits-1) subsystemCodeLogicalQubitsCount
         -- }}}
        ,testProperty "adding two anti-commuting operators" $ do -- {{{
            number_of_physical_qubits ← choose (1,16)
            op1 ← generateNonTrivialOperatorOfSize number_of_physical_qubits
            let go = do
                    op2 ← generateNonTrivialOperatorOfSize number_of_physical_qubits
                    if antiCommute op1 op2
                        then return op2
                        else go
            op2 ← go
            let (code@SubsystemCode{..},success) =
                    addToSubsystemCodeWithSuccessTag op2
                    .
                    addToSubsystemCode op1
                    .
                    initialSubsystemCode
                    $
                    number_of_physical_qubits
            return . unsafePerformIO . (True <$) $ do
                assertBool "success of adding operator to the code" success
                validateCode number_of_physical_qubits code
                assertEqual "number of stabilizers" 0 subsystemCodeStabilizersCount
                assertEqual "number of gauge qubits" 1 subsystemCodeGaugeQubitsCount
                assertEqual "number of logical qubits" (number_of_physical_qubits-1) subsystemCodeLogicalQubitsCount
         -- }}}
        ,testProperty "adding two commuting operators" $ do -- {{{
            number_of_physical_qubits ← choose (2,16)
            op1 ← generateNonTrivialOperatorOfSize number_of_physical_qubits
            let go = do
                    op2 ← generateNonTrivialOperatorOfSize number_of_physical_qubits
                    if commute op1 op2 && op1 /= op2
                        then return op2
                        else go
            op2 ← go
            let (code@SubsystemCode{..},success) =
                    addToSubsystemCodeWithSuccessTag op2
                    .
                    addToSubsystemCode op1
                    .
                    initialSubsystemCode
                    $
                    number_of_physical_qubits
            return . unsafePerformIO . (True <$) $ do
                assertBool "success of adding operator to the code" success
                validateCode number_of_physical_qubits code
                assertEqual "number of stabilizers" 2 subsystemCodeStabilizersCount
                assertEqual "number of gauge qubits" 0 subsystemCodeGaugeQubitsCount
                assertEqual "number of logical qubits" (number_of_physical_qubits-2) subsystemCodeLogicalQubitsCount
         -- }}}
        ,testProperty "construction from random sets of operators preserves code validity" $ \operators →
            let number_of_physical_qubits = bitSize (undefined :: Word)
            in  unsafePerformIO
                .
                (True <$)
                .
                validateCode number_of_physical_qubits
                .
                addAllToSubsystemCode operators
                .
                initialSubsystemCode
                $
                number_of_physical_qubits
        ]
     -- }}}
    ]
    -- }}}
