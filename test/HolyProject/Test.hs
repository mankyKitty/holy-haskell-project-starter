module HolyProject.Test
       (stringUtilsSuite
       ) where

import Test.Tasty
import Test.Tasty.SmallCheck

import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC

import Test.SmallCheck.Series

import HolyProject

stringUtilsSuite :: TestTree
stringUtilsSuite = testGroup "String Utils"
                   [ SC.testProperty "SC projectNameFromString idempotent" $
                     idempotent projectNameFromString
                   , SC.testProperty "SC capitalise idempotent" $
                     deeperIdempotent capitalise
                   , QC.testProperty "QC projectNameFromString idempotent" $
                     idempotent capitalise
                   ]

idempotent f = \s -> f s == f (f s)

deeperIdempotent :: (Eq a, Show a, Serial m a) => (a -> a) -> SC.Property m
deeperIdempotent f= forAll $ SC.changeDepth1 (+1) $ \s -> f s == f (f s)
