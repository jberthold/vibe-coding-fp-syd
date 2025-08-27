import Test.Hspec
import SinkNodeGraph
import qualified Data.Set as Set

main :: IO ()
main = hspec $ do
  describe "SinkNodeGraph" $ do
    it "empty graph returns empty set for any query" $ do
      leavesFrom empty "a" `shouldBe` Set.empty
    
    it "single node with no edges is a leaf" $ do
      let graph = addEdge empty ("a", "b")
      leavesFrom graph "b" `shouldBe` Set.singleton "b"
    
    it "finds leaves in a simple chain" $ do
      let graph = foldl addEdge empty [("a", "b"), ("b", "c")]
      leavesFrom graph "a" `shouldBe` Set.singleton "c"
    
    it "finds multiple leaves" $ do
      let graph = foldl addEdge empty [("a", "b"), ("a", "c")]
      leavesFrom graph "a" `shouldBe` Set.fromList ["b", "c"]
    
    it "handles cycles correctly" $ do
      let graph = foldl addEdge empty [("a", "b"), ("b", "c"), ("c", "a")]
      leavesFrom graph "a" `shouldBe` Set.empty