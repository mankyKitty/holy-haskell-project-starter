module HolyProject where

-- | Particular Imports
import System.IO (hFlush, stdout)
import Data.List (intercalate)
import Data.List.Split (splitOneOf)
import Data.Char (toLower, toUpper, isLetter, isNumber)

-- | Time Related Imports
import Data.Time (getCurrentTime, formatTime)
import System.Locale (defaultTimeLocale)

-- | General Imports
import System.Random (randomIO)
import System.Console.ANSI
import Control.Concurrent

-- | This module imports
import HolyProject.HolyFiles
import HolyProject.HolyActions
import HolyProject.HolyGitQueries

-- | Print a String to the screen using the given Color.
-- | This is using a Dull foreground for the colour.
colorPutStr :: Color -> String -> IO ()
colorPutStr color str = do
  setSGR [ SetColor Foreground Dull color
         , SetConsoleIntensity NormalIntensity
         ]
  putStr str
  setSGR []

-- | Convert the given project name input into a filesystem safe string.
projectNameFromString :: String -> String
projectNameFromString = intercalate "-" . splitOneOf " -" . map toLower

-- | Capitalise all the words in a String.
capitalise :: String -> String
capitalise [] = []
capitalise st = (concatMap capitaliseWord . splitOneOf " -" ) st
  where
    capitaliseWord :: String -> String
    capitaliseWord []     = []
    capitaliseWord (x:xs) = [toUpper x] ++ xs

-- | Input assertion function for when questions are asked of the user.
ioassert :: Bool -> String -> IO ()
ioassert True _ = return ()
ioassert False str = holyError str

-- | Retrieve the current year.
-- | Not keen on the usage of old-locale but I couldn't find an alternative. :(
getCurrentYear :: IO String
getCurrentYear = getCurrentTime >>= return . formatTime defaultTimeLocale "%Y"

-- | Verify if project name is acceptable
checkProjectName :: String -> Bool
checkProjectName [] = False
checkProjectName str =
  all (\c -> isLetter c || isNumber c || c == '-' || c == ' ') str

-- | Ask a question of the user, and provide hints if we have one.
ask :: String -> Maybe String -> IO String
ask info hint = do
  bk $ "What is your " ++ info ++ "?" ++ (maybe "" (\h -> " (" ++ h ++ ")") hint)
  putStr "> "
  hFlush stdout -- Because we want to ask on the same line
  getLine

-- | These functions represent the various actors in the scene.
bk :: String -> IO ()
bk str = colorPutStr Green ("Bridgekeeper: " ++ str ++ "\n")

bkn :: String -> IO ()
bkn str = colorPutStr Green ("Bridgekeeper: " ++ str)

you :: String -> IO ()
you str = colorPutStr Yellow ("You: " ++ str ++ "\n")
-- End actor functions.

-- | Play the intro spiel for the user.
intro :: IO ()
intro = do
  bk "Stop!"
  bk "Who would cross the Bridge of Death"
  bk "must answer me these questions three,"
  bk "ere the other side they see."
  you "Ask me the questions, Bridgekeeper, I am not afraid.\n"

-- | GET ON WITH IT!
end :: IO ()
end = do
  putStrLn "\n\n"
  bk "What... is the air-speed velocity of an unladen swallow?"
  you "What do you mean? An African or European swallow?"
  bk "Huh? I... I don't know that."
  putStrLn "[the bridgekeeper is thrown over]"
  bk "Auuuuuuuuuuuugh"
  putStrLn "Sir Bedevere: How do you know so much about swallows?"
  you "Well, you have to know these things when you're a king, you know."

-- | Our custom error handling function to have a sooky-la-la at the
-- | user and print the actual error message to the screen.
holyError :: String -> IO ()
holyError str = do
  r <- randomIO
  if r
     then
      do
        bk "What... is your favourite colour?"
        you "Blue. No, yel..."
        putStrLn "[You are thrown over the edge into the volcano]"
        you "You: Auuuuuuuuuuuugh"
        bk " Hee hee heh."
     else
      do
        bk "What is the capital of Assyria?"
        you "I don't know that!"
        putStrLn "[You are thrown over the edge into the volcano]"
        you "Auuuuuuuuuuuugh"
  error ('\n':str)

-- | You really shouldn't need to ask here ! ;)
holyStarter :: IO ()
holyStarter = do
  intro -- play intro cutscene
  
  gitconfig <- safeReadGitConfig
  let (name,email) = getNameAndMail gitconfig
  
  earlyhint <- newEmptyMVar
  maybe (putMVar earlyhint Nothing) -- if no email found put Nothing
    (\hintmail -> do
        forkIO (putMVar earlyhint =<< getGHUser hintmail)
        return ())
    email

  project <- ask "project name" Nothing
  ioassert (checkProjectName project)
    "Use only letters, numbers, spaces and dashes please."
    
  let projectname = projectNameFromString project
      modulename = capitalise project

  in_author <- ask "name" name
  in_email <- ask "email" email

  ghUserHint <- if (maybe "" id email) /= in_email
                then getGHUser in_email
                else takeMVar earlyhint
                     
  in_ghaccount <- ask "github account" ghUserHint
  in_synopsis <- ask "project in less than a dozen words" Nothing

  current_year <- getCurrentYear
  
  -- Create the actual project and populate it with goodies!
  createProject $ Project projectname modulename in_author in_email
    in_ghaccount in_synopsis current_year

  -- init the cabal and Git steps
  initialiseGitAndCabal projectname
  -- Print the end of the scene.
  end
