import Test.Tasty
import Test.Tasty.HUnit
import AwkwardGreeter

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "AwkwardGreeter"
  [ testCase "English, no name" $ do
      let out = mkPhrases en []
      out @?= ["Hello world","Sorry, gotta go","Bye"]
  , testCase "English, one name" $ do
      let out = mkPhrases en ["Alice"]
      out @?= ["Hello Alice","Sorry, gotta go","Bye Alice"]
  , testCase "English, many names" $ do
      let out = mkPhrases en ["Alice","Bob","Carol"]
      out @?= ["Hello Alice,Bob,Carol","Sorry, gotta go","Bye everyone"]
  , testCase "German, no name" $ do
      let out = mkPhrases de []
      out @?= ["Hallo Welt","Sehr erfreut!","Auf Wiedersehen"]
  ]
