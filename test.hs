-- @+leo-ver=5-thin
-- @+node:gcross.20110918102335.1159: * @thin test.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20101114125204.1281: ** << Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-<< Language extensions >>

-- @+<< Import needed modules >>
-- @+node:gcross.20101114125204.1260: ** << Import needed modules >>
import Control.Applicative
import Control.Monad

import Data.Bits
import Data.Monoid
import Data.Word

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Data.Quantum.Operator
import Data.Quantum.Operator.ReducedEschelonForm
-- @-<< Import needed modules >>

-- @+others
-- @+node:gcross.20110918102335.1183: ** Instances
-- @+node:gcross.20110918102335.1184: *3* Arbitrary Pauli
instance Arbitrary Pauli where arbitrary = elements [I,X,Y,Z]
-- @+node:gcross.20110918102335.1195: *3* Arbitrary Operator
instance Bits α ⇒ Arbitrary (Operator α) where
    arbitrary =
        fmap fromPauliList
             (resize (bitSize (undefined :: α)) (listOf arbitrary))
-- @+node:gcross.20110918102335.1197: ** Functions
-- @+node:gcross.20110918102335.1198: *3* generateOperator
generateOperator :: Bits α ⇒ Int → Gen (Operator α)
generateOperator = fmap fromPauliList . vector
-- @-others

main = defaultMain
    -- @+<< Tests >>
    -- @+node:gcross.20101114125204.1267: ** << Tests >>
    -- @+others
    -- @+node:gcross.20110918102335.1170: *3* Data.Quantum.Operator
    [testGroup "Data.Quantum.Operator"
        -- @+others
        -- @+node:gcross.20110918102335.1189: *4* Functions
        [testGroup "Functions" $
            -- @+others
            -- @+node:gcross.20110918102335.1191: *5* countBits
            [testProperty "countBits" $ \(x :: Word8) → countBits x == length [() | i ← [0..7], testBit x i]
            -- @+node:gcross.20110918102335.1171: *5* fromPauliList
            ,testGroup "fromPauliList"
                -- @+others
                -- @+node:gcross.20110918102335.1172: *6* identity
                [testCase "identity" $ forM_ [0..8] $ \n → Operator 0 (0 :: Word8) @=? fromPauliList (replicate n I)
                -- @+node:gcross.20110918102335.1177: *6* IXYZ
                ,testCase "IXYZ" $ Operator 6 (12 :: Word8) @=? fromPauliList [I,X,Y,Z]
                -- @+node:gcross.20110918102335.1180: *6* . toPauliList = identity function
                ,testProperty ". toPauliList = identity function" $ do
                    n ← choose(0,8)
                    let upper_bound = bit n - 1
                    op :: Operator Word8 ← liftM2 Operator
                            (fmap fromIntegral $ choose (0,upper_bound :: Int))
                            (fmap fromIntegral $ choose (0,upper_bound))
                    return $ liftA2 (==) id (fromPauliList . toPauliList n) op
                -- @-others
                ]
            -- @+node:gcross.20110918102335.1192: *5* nonTrivialAt
            ,testProperty "nonTrivialAt" $ do
                n ← choose (1,8)
                o :: Operator Word8 ← generateOperator n
                i ← choose (0,n-1)
                return $ nonTrivialAt i o == (toPauliList n o !! i /= I)
            -- @+node:gcross.20110918102335.1175: *5* toPauliList
            ,testGroup "toPauliList"
                -- @+others
                -- @+node:gcross.20110918102335.1176: *6* identity
                [testCase "identity" $ forM_ [0..8] $ \n → toPauliList n (Operator 0 (0 :: Word8)) @?= replicate n I
                -- @+node:gcross.20110918102335.1179: *6* IXYZ
                ,testCase "IXYZ" $ toPauliList 4 (Operator 6 (12 :: Word8)) @?= [I,X,Y,Z]
                -- @+node:gcross.20110918102335.1182: *6* . fromPauliList = identity function
                ,testProperty ". fromPauliList = identity function" $ do
                    n ← choose(0,8)
                    components :: [Pauli] ← vector n
                    return $ liftA2 (==) id (toPauliList n . (fromPauliList :: [Pauli] → Operator Word8)) components
                -- @-others
                ]
            -- @-others
            ]
        -- @+node:gcross.20110918102335.1201: *4* Instances
        ,testGroup "Instances"
            -- @+others
            -- @+node:gcross.20110918102335.1202: *5* Commutable
            [testGroup "Commutable"
                -- @+others
                -- @+node:gcross.20110918102335.1221: *6* Operator
                [testGroup "Operator"
                    -- @+others
                    -- @+node:gcross.20110918102335.1223: *7* correct property
                    [testProperty "correct property" $ \(x :: Operator Word8, y :: Operator Word8) → commute x y /= antiCommute x y
                    -- @+node:gcross.20110918102335.1222: *7* correct result
                    ,testProperty "correct result" $ do
                        n ← choose(0,8)
                        components1 :: [Pauli] ← vector n
                        components2 :: [Pauli] ← vector n
                        return $
                            commute (fromPauliList components1 :: Operator Word8) (fromPauliList components2)
                            ==
                            (length (filter id (zipWith antiCommute components1 components2)) `mod` 2 == 0)
                    -- @-others
                    ]
                -- @+node:gcross.20110918102335.1213: *6* Pauli
                ,testGroup "Pauli"
                    -- @+others
                    -- @+node:gcross.20110918102335.1215: *7* correct property
                    [testProperty "correct property" $ \(x :: Pauli, y :: Pauli) → commute x y /= antiCommute x y
                    -- @+node:gcross.20110918102335.1204: *7* correct result
                    ,testCase "correct result" $ do
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
                    -- @-others
                    ]
                -- @-others
                ]
            -- @+node:gcross.20110918102335.1185: *5* Monoid
            ,testGroup "Monoid"
                -- @+others
                -- @+node:gcross.20110918102335.1187: *6* Operator
                [testProperty "Operator" $ do
                    n ← choose(0,8)
                    components1 :: [Pauli] ← vector n
                    components2 :: [Pauli] ← vector n
                    return $
                        (fromPauliList components1 `mappend` fromPauliList components2)
                        ==
                        (fromPauliList (zipWith mappend components1 components2) :: Operator Word8)
                -- @+node:gcross.20110918102335.1186: *6* Pauli
                ,testCase "Pauli" $ do
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
                -- @-others
                ]
            -- @-others
            ]
        -- @-others
        ]
    -- @-others
    -- @-<< Tests >>
    ]
-- @-leo
