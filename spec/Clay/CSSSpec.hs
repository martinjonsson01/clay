module Clay.CSSSpec where

import Clay
import Data.Either
import qualified Data.Text.Lazy as Lazy
import Generators
import Test.Hspec
import Test.Hspec.QuickCheck
import Text.CSS.Parse (parseNestedBlocks)

spec :: Spec
spec = do
  describe "Css" $ do
    prop "generated css can be parsed correctly"
      $ \(ArbitraryCss css) ->
        let rendered = Lazy.toStrict $ render css
         in isRight $ parseNestedBlocks rendered