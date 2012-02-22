-- Language extensions {{{

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- }}} Language extensions

-- Imports {{{

import Control.Applicative
import Control.Monad

import Data.Bits
import Data.Function (on)
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

import Data.Quantum.Operator
import Data.Quantum.Operator.ReducedEschelonForm

-- }}} Imports

-- Types {{{

data OperatorAndSize α = OperatorAndSize (Operator α) Int deriving (Eq,Show)

data OperatorAndSizeAndIndex α = OperatorAndSizeAndIndex (Operator α) Int Int deriving (Eq,Show)

-- }}} Types

-- Instances {{{

instance Arbitrary Pauli where arbitrary = elements [I,X,Y,Z]

instance Bits α ⇒ Arbitrary (Operator α) where -- {{{
    arbitrary =
        fmap fromPauliList
             (resize (bitSize (undefined :: α)) (listOf arbitrary))
-- }}}

instance Bits α ⇒ Arbitrary (OperatorAndSize α) where -- {{{
    arbitrary = do
        n ← choose (0,bitSize (undefined :: α))
        o ← generateOperatorOfSize n
        return $ OperatorAndSize o n
-- }}}

instance Bits α ⇒ Arbitrary (OperatorAndSizeAndIndex α) where -- {{{
    arbitrary = do
        n ← choose (1,bitSize (undefined :: α))
        i ← choose (0,n-1)
        o ← generateOperatorOfSize n
        return $ OperatorAndSizeAndIndex o n i
-- }}}

-- }}} Instances

-- Functions {{{

assertNotEqual :: (Eq a, Show a) => String -- ^ The message prefix 
                                 -> a      -- ^ The expected value 
                                 -> a      -- ^ The actual value
                                 -> Assertion
assertNotEqual preface expected actual =
  when (actual == expected) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "expected to *not* get: " ++ show expected ++ "\n"

generateOperatorOfSize :: Bits α ⇒ Int → Gen (Operator α) -- {{{
generateOperatorOfSize n =
    let upper_bound = bit n - 1
    in liftM2 Operator
        (fmap fromIntegral $ choose (0,upper_bound :: Int))
        (fmap fromIntegral $ choose (0,upper_bound))
-- }}}

-- }}} Functions

main = defaultMain
    -- Tests {{{
    [testGroup "Data.Quantum.Operator" -- {{{
        [testGroup "Functions" $ -- {{{
            [testProperty "agreeAt" $ \(o1 :: Operator Word8) (o2 :: Operator Word8) → do -- {{{
                i ← choose (0,7)
                return $ agreeAt i o1 o2 == (getPauliAt i o1 == getPauliAt i o2)
             -- }}}
            ,testProperty "antiCommuteAt" $ do -- {{{
                n ← choose (1,8)
                components1 :: [Pauli] ← vector n
                components2 :: [Pauli] ← vector n
                i ← choose (0,n-1)
                return $
                    antiCommuteAt i (fromPauliList components1 :: Operator Word8) (fromPauliList components2)
                 == antiCommute (components1 !! i) (components2 !! i)
             -- }}}
            ,testProperty "commuteAt" $ do -- {{{
                n ← choose (1,8)
                components1 :: [Pauli] ← vector n
                components2 :: [Pauli] ← vector n
                i ← choose (0,n-1)
                return $
                    commuteAt i (fromPauliList components1 :: Operator Word8) (fromPauliList components2)
                 == commute (components1 !! i) (components2 !! i)
             -- }}}
            ,testProperty "countBits" $ \(x :: Word8) → countBits x == length [() | i ← [0..7], testBit x i]
            ,testGroup "fromPauliList" -- {{{
                [testCase "identity" $ forM_ [0..8] $ \n → Operator 0 (0 :: Word8) @=? fromPauliList (replicate n I)
                ,testCase "IXYZ" $ Operator 6 (12 :: Word8) @=? fromPauliList [I,X,Y,Z]
                ,testProperty ". toPauliList = identity function" $ \(OperatorAndSize (op :: Operator Word16) n) → -- {{{
                    liftA2 (==) id (fromPauliList . toPauliList n) op
                 -- }}}
                ]
             -- }}}
            ,testProperty "getPauliAt" $ \(OperatorAndSizeAndIndex (o :: Operator Word16) n i) → -- {{{
                getPauliAt i o == (toPauliList n o) !! i
             -- }}}
            ,testCase "has(X/Z)Bit" $ -- {{{
                forM_ [I .. Y] $ \pauli → do
                    hasXBit pauli @?= (testBit (fromEnum pauli) 0)
                    hasZBit pauli @?= (testBit (fromEnum pauli) 1)
             -- }}}
            ,testGroup "maybeFirstNonTrivialColumnOf" -- {{{
                [testCase "identity" $ -- {{{
                    forM_ [0..8] $ \(n :: Int) →
                        assertBool ("column " ++ show n) $
                            isNothing (maybeFirstNonTrivialColumnOf . (fromPauliList :: [Pauli] → Operator Word8) $ replicate n I)
                 -- }}}
                ,testProperty "non-identity" $ do -- {{{
                    n ← choose(1,8)
                    first_non_trivial_column ← choose (0,n-1)
                    operator :: Operator Word8 ←
                        fmap (fromPauliList . concat) . sequence $
                            [return (replicate first_non_trivial_column I)
                            ,fmap (:[]) (elements [X,Y,Z])
                            ,vector (n-first_non_trivial_column-1)
                            ]
                    return $ maybeFirstNonTrivialColumnOf operator == Just first_non_trivial_column
                 -- }}}
                ]
             -- }}}
            ,testProperty "multiplyByIf" $ \(b :: Bool, x :: Operator Word8, y :: Operator Word8) → -- {{{
                multiplyByIf b x y == if b then (x `mappend` y) else y
             -- }}}
            ,testProperty "multiplyByIfAntiCommuteAt" $ do -- {{{
                n ← choose (1,8)
                components1 :: [Pauli] ← vector n
                let operator1 :: Operator Word8 = fromPauliList components1
                components2 :: [Pauli] ← vector n
                let operator2 :: Operator Word8 = fromPauliList components2
                i ← choose (0,n-1)
                return $
                    multiplyByIfAntiCommuteAt i operator1 operator2
                 == if antiCommuteAt i operator1 operator2 then (operator1 `mappend` operator2) else operator2
             -- }}}
            ,testProperty "nonTrivialAt" $ \(OperatorAndSizeAndIndex (o :: Operator Word16) n i) → -- {{{
                nonTrivialAt i o == (toPauliList n o !! i /= I)
             -- }}}
            ,testProperty "setPauliAt" $ \(OperatorAndSizeAndIndex (o :: Operator Word16) _ i) (p :: Pauli) → -- {{{
                    (getPauliAt i . setPauliAt i p) o == p
             -- }}}
            ,testGroup "toPauliList" -- {{{
                [testCase "identity" $ forM_ [0..8] $ \n → toPauliList n (Operator 0 (0 :: Word8)) @?= replicate n I
                ,testCase "IXYZ" $ toPauliList 4 (Operator 6 (12 :: Word8)) @?= [I,X,Y,Z]
                ,testProperty ". fromPauliList = identity function" $ do -- {{{
                    n ← choose(0,8)
                    components :: [Pauli] ← vector n
                    return $ liftA2 (==) id (toPauliList n . (fromPauliList :: [Pauli] → Operator Word8)) components
                 -- }}}
                ]
             -- }}}
            ]
         -- }}}
        ,testGroup "Instances" -- {{{
            [testGroup "Commutable" -- {{{
                [testGroup "Operator" -- {{{
                    [testProperty "correct property" $ \(x :: Operator Word8, y :: Operator Word8) → commute x y /= antiCommute x y
                    ,testProperty "correct result" $ do -- {{{
                        n ← choose(0,8)
                        components1 :: [Pauli] ← vector n
                        components2 :: [Pauli] ← vector n
                        return $
                            commute (fromPauliList components1 :: Operator Word8) (fromPauliList components2)
                            ==
                            (length (filter id (zipWith antiCommute components1 components2)) `mod` 2 == 0)
                     -- }}}
                    ]
                 -- }}}
                ,testGroup "Pauli"
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
                        (fromPauliList (zipWith mappend components1 components2) :: Operator Word8)
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
            ]
         -- }}}
        ]
     -- }}} Data.Quantum.Operator
    ,testGroup "Data.Quantum.Operator.ReducedEschelonForm" -- {{{
        [testProperty "add one operator to an empty form" $ -- {{{
            \(op :: Operator Word8) →
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
        ,testProperty "add two operators with non-trivial first bit to an empty form" $ -- {{{
            \(o1_ :: Operator Word8) (o2_ :: Operator Word8) → do
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
        ,testProperty "add the same operator twice" $ -- {{{
            \(o :: Operator Word8) → o /= mempty ==>
                let form1 = addToReducedEschelonForm o mempty
                    (form2,successful) = addToReducedEschelonFormWithSuccessTag o form1
                in form1 == form2 && not successful
         -- }}}
        ,testProperty "every pseudo-generator is independent of all others in its assigned column" $ -- {{{
         \(operators :: [Operator Word8]) →
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
        ,testProperty "every pseudo-generator has the correct paulis in the assigned column" $ -- {{{
          \(operators :: [Operator Word8]) →
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
        ,testProperty "adding a previously seen operator always fails" $ -- {{{
          \(operators_ :: [Operator Word8]) (operator :: Operator Word8) → do
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
            operators :: [Operator Word8] ← vectorOf n arbitrary
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
            form1 ← fmap (flip addAllToReducedEschelonForm mempty) (choose (0,4) >>= flip vectorOf (arbitrary :: Gen (Operator Word8)))
            form2 ← fmap (flip addAllToReducedEschelonForm mempty) (choose (0,4) >>= flip vectorOf (arbitrary :: Gen (Operator Word8)))
            return . unsafePerformIO . (>> return True) $
                (on
                    (assertEqual "Does the reduced eschelon form generate the same set as the original?")
                    (Set.fromList . map mconcat . subsequences)
                )
                    (operatorsInReducedEschelonForm form1 `mappend` operatorsInReducedEschelonForm form2)
                    (operatorsInReducedEschelonForm (form1 `mappend` form2))
         -- }}}
        ]
     -- }}} Data.Quantum.Operator.ReducedEschelonForm
    ]
    -- }}} Tests
