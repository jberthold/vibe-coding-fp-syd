import Test.Tasty
import Test.Tasty.HUnit
import AwkwardGreeter

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "AwkwardGreeter"
  [ testCase "English, no name" $ do
      out <- runReaderT (mkPhrases []) en
      out @?= ["Hello world","Sorry, gotta go","Bye"]
  , testCase "English, one name" $ do
      out <- runReaderT (mkPhrases ["Alice"]) en
      out @?= ["Hello Alice","Sorry, gotta go","Bye Alice"]
  , testCase "English, many names" $ do
      out <- runReaderT (mkPhrases ["Alice","Bob","Carol"]) en
      out @?= ["Hello Alice,Bob,Carol","Sorry, gotta go","Bye everyone"]
  , testCase "German, no name" $ do
      out <- runReaderT (mkPhrases []) de
      out @?= ["Hallo Welt","Sehr erfreut!","Auf Wiedersehen"]
  ]
