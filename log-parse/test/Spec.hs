module Main (main) where

import Test.Hspec
import LogParser
import Visualizer

main :: IO ()
main = hspec $ do
  describe "LogParser" $ do
    it "parses test start lines" $ do
      let line = "src/tests/integration/test_cli.py::test_cli_show_printers_snapshot"
      parseLogLine line `shouldBe` Just (TestStarted line)
    
    it "parses test end lines" $ do
      let line = "[gw3] [  0%] PASSED src/tests/integration/test_integration.py::test_schema_parse[rvalueaggregate]"
      parseLogLine line `shouldBe` Just (TestFinished "3" Passed "src/tests/integration/test_integration.py::test_schema_parse[rvalueaggregate]")
    
    it "ignores irrelevant lines" $ do
      parseLogLine "============================= test session starts ==============================" `shouldBe` Nothing
      parseLogLine "platform linux -- Python 3.10.12, pytest-8.4.1" `shouldBe` Nothing
    
  describe "Visualizer" $ do
    it "builds execution timeline" $ do
      let events = [ TestStarted "test1"
                   , TestStarted "test2"
                   , TestFinished "1" Passed "test1"
                   , TestStarted "test3"
                   , TestFinished "2" Failed "test2"
                   ]
      let timeline = buildExecutionTimeline events
      length timeline `shouldBe` 3
      testName (head timeline) `shouldBe` "test1"