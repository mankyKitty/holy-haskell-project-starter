 {-# LANGUAGE OverloadedStrings #-}

module HolyProject.HolyGitQueries where

import System.Environment (getEnv)
import Control.Exception
import System.IO.Error
import Control.Monad      (guard,(<=<))

import Control.Lens.Operators ((^?))
import Control.Lens.Aeson

import Data.Aeson.Encode (fromValue)

import Network.HTTP.Conduit

import qualified Data.Text.Lazy         as TLZ
import qualified Data.Text.Lazy.Builder as TLB

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as LZ

-- | Safely read the gitconfig file that is stored in the $HOME folder
  -- on the current filesystem.
safeReadGitConfig :: IO LZ.ByteString
safeReadGitConfig = do
  e <- tryJust (guard . isDoesNotExistError)
       (do
           home <- getEnv "HOME"
           LZ.readFile $ home ++ "/.gitconfig")
  return $ either (const (LZ.empty)) id e

-- | Retrieve the name and email address from the provided gitconfig file.
getNameAndMail :: LZ.ByteString -> (Maybe String, Maybe String)
getNameAndMail gitConfigContent = (getFirstValueFor splitted "name",
                                   getFirstValueFor splitted "email")
  where
    -- make lines of words
    splitted :: [[LZ.ByteString]]
    splitted = map LZ.words (LZ.lines gitConfigContent)

-- | Get the first line which start with 'elem = ' and return the third
    -- field (value)
getFirstValueFor :: [[LZ.ByteString]] -> String -> Maybe String
getFirstValueFor splitted itemKey = firstJust (map (getValueForKey itemKey) splitted)

-- | Return the first Just value of a list of Maybe.
firstJust :: (Eq a) => [Maybe a] -> Maybe a
firstJust l = case dropWhile (== Nothing) l of
                [] -> Nothing
                (j:_) -> j

-- | Given a line of words ("word1":"word2":rest) getValue will return
              -- rest if word1 == key 'elem =' or Nothing otherwise
getValueForKey :: String -- key
               -> [LZ.ByteString] -- line of words
               -> Maybe String -- the value if found
getValueForKey el (n:e:xs) = if (n == (LZ.pack el)) && (e == (LZ.pack "="))
                                then Just (LZ.unpack (LZ.unwords xs))
                                else Nothing
getValueForKey _ _ = Nothing                                     

-- | Given the url string, retrieve the body of the response.
simpleHTTPWithUserAgent :: String -> IO LZ.ByteString
simpleHTTPWithUserAgent url = do
  r <- parseUrl url
  let request = r { requestHeaders = [("User-Agent", "HTTP-Conduit")]}
  withManager $ (return . responseBody) <=< httpLbs request

-- | Get the current github user name based on the provided email address.
getGHUser :: String -> IO (Maybe String)
getGHUser "" = return Nothing
getGHUser email = do
  let url = "https://api.github.com/search/users?q=" ++ email
  body <- simpleHTTPWithUserAgent url
  let login = body ^? key "items" . nth 0 . key "login"
  return $ fmap jsonValueToString login
  where
    jsonValueToString = TLZ.unpack . TLB.toLazyText . fromValue
