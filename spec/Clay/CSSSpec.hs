module Clay.CSSSpec where

import           Clay
import           Clay.AspectRatio
import           Common
import           Data.Ratio
import           Test.Hspec
import Data.Either
import Text.CSS.Parse ( parseNestedBlocks )
import Test.Hspec.QuickCheck
import qualified Data.Text.Lazy as Lazy
import Generators

spec :: Spec
spec = do
  describe "Css" $ do
    prop "generated css can be parsed correctly" $
      \(ArbitraryCss css) -> let rendered = Lazy.toStrict $ render css in
      isRight $ parseNestedBlocks rendered