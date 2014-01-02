module HolyProject.HolyGitQueries.Test
       (githubAPISuite
       ) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit

import HolyProject.HolyGitQueries

githubAPISuite :: TestTree
githubAPISuite = testGroup "GithubAPI"
                 [ testCase "Sean" $ ioTestEq
                   (getGHUser "sclhiannan@gmail.com")
                   (Just "\"mankyKitty\"")
                 , testCase "Name" $ ioTestEq
                   (getGHUser "Sean Chalmers")
                   (Just "\"mankyKitty\"")
                 ]

-- | Test if some IO action returns some expected value
ioTestEq :: (Eq a, Show a) => IO a -> a -> Assertion
ioTestEq action expected = action >>= assertEqual "" expected
