{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (threadDelay)
import System.IO.Unsafe
import Data.IORef
import Options.Applicative

{-# NOINLINE theLang #-}
theLang :: IORef Language
theLang = unsafePerformIO $ newIORef (language "xy")

{-# NOINLINE getLang #-}
getLang :: Language
getLang = unsafePerformIO $ readIORef theLang

main :: IO ()
main = do
  (args, lang) <- execParser opts
  writeIORef theLang (language lang)
  main' args

opts = info p fullDesc
  where
    p = (,)
        <$> many (argument str (metavar "NAME"))
        <*> strOption (long "language" <> metavar "LANGUAGE" <> value "en")

main' :: [String] -> IO ()
main' args = do
  let phrases = mkPhrases args
  runPhrases phrases

runPhrases :: [String] -> IO ()
runPhrases [] = pure ()
runPhrases (x:rest) = putStrLn x >> threadDelay 1000000 >> runPhrases rest
  where Language{..} = getLang

mkPhrases :: [String] -> [String]
mkPhrases names
  | null names      = [ unwords [hello, world], phrase, bye ]
  | [name] <- names = [ unwords [hello, name] , phrase, unwords  [bye, name]]
  | otherwise       = greetMany names
    where Language{..} = getLang

greetMany :: [String] -> [String]
greetMany names =
  [ unwords [hello, some] , phrase, unwords  [bye, everyone]]
  where
    some | length names > 3 = everyone
         | otherwise        = foldr (\n acc -> n ++ ',':acc) (last names) (init names)
    Language{..} = getLang

en = Language
  { hello    = "Hello"
  , world    = "world"
  , phrase   = "Sorry, gotta go"
  , bye      = "Bye"
  , everyone = "everyone"
  , putStrLn = Prelude.putStrLn . ("So... " ++)
  }

data Language = Language { hello, world, phrase, bye, everyone :: String
                         , putStrLn :: String -> IO () }

de = Language "Hallo" "Welt" "Sehr erfreut!" "Auf Wiedersehen" "Leute" Prelude.putStrLn

language :: String -> Language
language = maybe en id . flip lookup langs


langs =
  [ ("en", en)
  , ("de", de)
  , ("dk", Language "Hej" "Verden" "Min glæde!" "Farvel" "Folk" $ p "dk: ")
  , ("oz", Language "G'day" "mate" "How are you?" "See ya" "me mates" $ p "")
  , ("fr", Language "Salut" "le monde" "Enchanté" "Au revoir" "les gars" $ p "Alors, ")
  , ("xy", Language "Sorry" "folks" "I don't speak many languages." "Cheers" "everyone" $ p "")
  ]
  where p prefix = Prelude.putStrLn . (prefix ++)
