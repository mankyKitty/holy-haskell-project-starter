module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)

import HolyProject.HolyGitQueries.Test
import HolyProject.Test

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
        [githubAPISuite
         , stringUtilsSuite
        ]
