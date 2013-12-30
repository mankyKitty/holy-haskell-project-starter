module HolyProject.HolyActions where

import System.Cmd (system)

runSystemActions :: [String]-> IO ()
runSystemActions = mapM_ system

initialiseGitAndCabal :: String -> IO ()
initialiseGitAndCabal projectName = runSystemActions ["git init ."
                                                     , "cabal sandbox init"
                                                     , "cabal install"
                                                     , "cabal test"
                                                     , "./.cabal-sandbox/bin/test" ++ projectName
                                                     ]
