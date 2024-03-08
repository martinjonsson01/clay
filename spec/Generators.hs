module Generators where

{-# LANGUAGE FlexibleInstances #-}

import           Clay.Selector
import qualified Data.Text       as T
import           Test.QuickCheck
import Prelude hiding ((**))
import Clay.Stylesheet
import Clay.Render
import Clay.Geometry
import Data.Ratio
import Data.Bifunctor
import qualified Data.Text.Lazy

newtype ArbitraryCss = ArbitraryCss Css

instance Show ArbitraryCss where
  show (ArbitraryCss css) = Data.Text.Lazy.unpack $ render css

instance Arbitrary ArbitraryCss where
  arbitrary = ArbitraryCss <$> genCss

genAspectRatio :: Gen Css
genAspectRatio = do
  r <- uncurry (%) . bimap getPositive getPositive <$> (arbitrary :: Gen(Positive Integer, Positive Integer))
  return $ aspectRatio (ratio r)

genSelector :: Gen Selector
genSelector = sized genSelectorTree
 where
  genSelectorTree :: Int -> Gen Selector
  genSelectorTree size
   | size <= 1 = genSelectorLeaf
   | otherwise = do
    ctr <- elements [ (|+),  (|>), (**), (|~)]
    tree1 <- genSelectorTree (size `div` 2)
    tree2 <- genSelectorTree (size `div` 2)
    return (tree1 `ctr` tree2)

genCss :: Gen Css
genCss = do
  p <- genAspectRatio
  s <- genSelector
  combinator <- elements [root, (?), (<?)]
  return (s `combinator` p)

test = mapM_ putCss =<< sample' genCss

genSelectorLeaf :: Gen Selector
genSelectorLeaf = oneof [genElement, genStar]

elementNames = ["h1", "p", "div", "form", "br", "plupp", "onion", "span", "ng-for"]

genElement :: Gen Selector
genElement = element . T.pack <$> elements elementNames

genStar :: Gen Selector
genStar = pure star
