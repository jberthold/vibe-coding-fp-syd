{-# LANGUAGE RecordWildCards #-}

module Main (main, mkPhrases, en, de) where -- export for testing

import Control.Concurrent (threadDelay)
import System.IO.Unsafe
import Data.IORef
import Options.Applicative
import Control.Monad.Reader


main :: IO ()
main = do
  (args, lang) <- execParser opts
  let langVal = language lang
  runReaderT (main' args) langVal -- run the ReaderT with Language

opts = info p fullDesc
  where
    p = (,)
        <$> many (argument str (metavar "NAME"))
        <*> strOption (long "language" <> metavar "LANGUAGE" <> value "en")

main' :: [String] -> ReaderT Language IO ()
main' args = do
  phrases <- mkPhrases args
  runPhrases phrases

runPhrases :: [String] -> ReaderT Language IO ()
runPhrases [] = pure ()
runPhrases (x:rest) = do
  lang <- ask
  liftIO $ languagePutStrLn lang x
  liftIO $ threadDelay 1000000
  runPhrases rest

mkPhrases :: [String] -> ReaderT Language IO [String]
mkPhrases names = do
  Language{..} <- ask
  pure $ case names of
    []        -> [ unwords [hello, world], phrase, bye ]
    [name]    -> [ unwords [hello, name] , phrase, unwords  [bye, name]]
    _         -> greetMany names

greetMany :: [String] -> [String]
greetMany names =
  [ unwords [hello, some] , phrase, unwords  [bye, everyone]]
  where
    some | length names > 3 = everyone
         | otherwise        = foldr (\n acc -> n ++ ',':acc) (last names) (init names)
    Language{..} = en -- dummy, will be replaced by Reader context


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
