{-# LANGUAGE RecordWildCards #-}

module Main (main, mkPhrases, en, de) where -- export for testing

import Control.Concurrent (threadDelay)
import System.IO.Unsafe
import Data.IORef
import Options.Applicative


main :: IO ()
main = do
  (args, lang) <- execParser opts
  let langVal = language lang -- eliminate global variable, pass as argument
  main' langVal args -- pass Language explicitly

opts = info p fullDesc
  where
    p = (,)
        <$> many (argument str (metavar "NAME"))
        <*> strOption (long "language" <> metavar "LANGUAGE" <> value "en")

main' :: Language -> [String] -> IO ()
main' lang args = do
  let phrases = mkPhrases lang args -- pass Language explicitly
  runPhrases lang phrases -- pass Language explicitly

runPhrases :: Language -> [String] -> IO ()
runPhrases _ [] = pure ()
runPhrases lang (x:rest) = putStrLn x >> threadDelay 1000000 >> runPhrases lang rest
  where putStrLn = languagePutStrLn lang -- use putStrLn from Language

mkPhrases :: Language -> [String] -> [String]
mkPhrases lang names
  | null names      = [ unwords [hello, world], phrase, bye ]
  | [name] <- names = [ unwords [hello, name] , phrase, unwords  [bye, name]]
  | otherwise       = greetMany lang names
  where Language{..} = lang -- use RecordWildcards for fields

greetMany :: Language -> [String] -> [String]
greetMany lang names =
  [ unwords [hello, some] , phrase, unwords  [bye, everyone]]
  where
    some | length names > 3 = everyone
         | otherwise        = foldr (\n acc -> n ++ ',':acc) (last names) (init names)
    Language{..} = lang -- use RecordWildcards for fields


en = Language
  { hello    = "Hello"
  , world    = "world"
  , phrase   = "Sorry, gotta go"
  , bye      = "Bye"
  , everyone = "everyone"
  , languagePutStrLn = Prelude.putStrLn . ("So... " ++)
  }

data Language = Language { hello, world, phrase, bye, everyone :: String
                         , languagePutStrLn :: String -> IO () } -- renamed field for clarity

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
