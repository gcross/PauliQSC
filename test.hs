-- Language extensions {{{

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

-- }}} Language extensions

-- Imports {{{

import Control.Applicative
import Control.Monad

import Data.Bits
import Data.Maybe
import Data.Monoid
import Data.Word

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

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
            [testProperty "antiCommuteAt" $ do -- {{{
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
    ]
    -- }}} Tests
