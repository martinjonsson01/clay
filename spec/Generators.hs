module Generators where

{-# LANGUAGE FlexibleInstances #-}

import           Clay.Selector
import qualified Data.Text       as T
import           Test.QuickCheck
import Prelude hiding ((**))
import Clay.Stylesheet
import Clay.Render
import Clay.Geometry hiding (size)
import Data.Ratio
import Data.Bifunctor
import qualified Data.Text.Lazy
import Clay.Size (Angle, Deg, LengthUnit, Size, deg, cm, mm, inches, px, pt, pc)
import Clay.Transform
import Clay (Number)
import Data.Foldable (fold)

newtype ArbitraryCss = ArbitraryCss Css

instance Show ArbitraryCss where
  show (ArbitraryCss css) = Data.Text.Lazy.unpack $ render css

instance Arbitrary ArbitraryCss where
  arbitrary = ArbitraryCss <$> genCss

-- | Generates a CSS document.
genCss :: Gen Css
genCss = do
  p <- genProperties
  s <- genSelector
  combinator <- elements [root, (?), (<?)]
  return (s `combinator` p)

-- | Generate a CSS document containing zero or more properties.
genProperties :: Gen Css
genProperties = do
  propGens <- sublistOf [genMargin, genAspectRatio, genTransforms]
  props <- sequence propGens
  return $ fold props

-- | Generates a completely random selector.
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

-- | Generates a CSS document containing a single aspect-ratio property.
genAspectRatio :: Gen Css
genAspectRatio = do
  r <- uncurry (%) . bimap getPositive getPositive <$> (arbitrary :: Gen (Positive Integer, Positive Integer))
  return $ aspectRatio (ratio r)

-- | Generates a transform property.
genTransforms :: Gen Css
genTransforms = do
  scales <- mapM (<$> genNumber) [scaleX, scaleY, scaleZ]
  rotates <- mapM (<$> genAngleDeg) [rotateX, rotateY, rotateZ]
  translates <- mapM (<$> genSize) [translateX, translateY, translateZ]
  transformations <- sublistOf (scales <> rotates <> translates)
  return $ transforms transformations

-- | Generates a margin in a random direction.
genMargin :: Gen Css
genMargin = do
  marginFn <- elements [marginTop, marginLeft, marginRight, marginBottom]
  marginFn <$> genSize

-- | Generates a size with a random unit.
genSize :: Gen (Size LengthUnit)
genSize = do
  sizeFn <- elements [cm, mm, inches, px, pt, pc]
  sizeFn <$> genNumber

-- | Generates a random fixed-precision (to 5 decimal places) numbe
genNumber :: Gen Number
genNumber = realToFrac <$> (arbitrary :: Gen Int)

genAngleDeg :: Gen (Angle Deg)
genAngleDeg = deg <$> genNumber

-- | Generates selectors that terminate.
genSelectorLeaf :: Gen Selector
genSelectorLeaf = oneof [genElement, genStar]

-- | Element names to generate.
elementNames :: [String]
elementNames = ["h1", "p", "div", "form", "br", "plupp", "onion", "span", "ng-for"]

-- | Chooses between a set of element names to generate.
genElement :: Gen Selector
genElement = element . T.pack <$> elements elementNames

-- | Always generates a star selector.
genStar :: Gen Selector
genStar = pure star