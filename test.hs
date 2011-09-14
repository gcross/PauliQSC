-- @+leo-ver=5-thin
-- @+node:gcross.20110918102335.1159: * @thin test.hs
-- @@language haskell

-- @+<< Language extensions >>
-- @+node:gcross.20101114125204.1281: ** << Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-<< Language extensions >>

-- @+<< Import needed modules >>
-- @+node:gcross.20101114125204.1260: ** << Import needed modules >>
import Control.Monad

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.Quantum.Operator
import Data.Quantum.Operator.ReducedEschelonForm
-- @-<< Import needed modules >>

-- @+others
-- @-others

main = defaultMain
    -- @+<< Tests >>
    -- @+node:gcross.20101114125204.1267: ** << Tests >>
    -- @+others
    -- @-others
    [
    -- @-<< Tests >>
    ]
-- @-leo
