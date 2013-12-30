{-# LANGUAGE DeriveDataTypeable #-}

module HolyProject.HolyFiles where

import Paths_holy_haskell_sc

import Data.Data
import Text.Hastache
import Text.Hastache.Context

import System.Directory

import System.FilePath.Posix (takeDirectory, (</>))

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as LZ

-- | Nice little package for our data to be passed around.
data Project = Project {
                       projectName  :: String
                       , moduleName :: String
                       , author     :: String
                       , mail       :: String
                       , ghaccout   :: String
                       , synopsis   :: String
                       , year       :: String
                       } deriving (Data, Typeable)

-- | This is the list of files that will be parsed by the Hastache
                                  -- system and saved into the given
                                  -- location with in the project.
-- | The first elem is the path to the template, and the second elem
                                  -- is the destination for the
                                  -- processed file.
genFileList :: Project -> [(FilePath,FilePath)]
genFileList p = [
                ("gitignore", ".gitignore")
                , ("project.cabal", (projectName p) ++ ".cabal")
                , ("Setup.hs", "Setup.hs")
                , ("src/Main.hs", "src" </> "Main.hs")
                ]

-- | Creates the actual folder structure and populates it with the
                -- files from the genFileList function.
createProject :: Project -> IO ()
createProject p = do
  let context = mkGenericContext p
  createDirectory $ projectName p -- mkdir
  setCurrentDirectory $ projectName p -- cd
  mapM_ (\(x,y) -> genFile context x y) $ genFileList p

-- | Using the current context and Hastache, process a file and save
    -- the output into the new project directory tree.
genFile :: MuContext IO -> FilePath -> FilePath -> IO ()
genFile context filename outputFilename = do
  pkgfileName <- getDataFileName ("scaffold/" ++ filename)
  template <- BS.readFile pkgfileName
  transformedFile <- hastacheStr defaultConfig template context -- hastache magic here
  createDirectoryIfMissing True (takeDirectory outputFilename)
  LZ.writeFile outputFilename transformedFile
