{-# LANGUAGE OverloadedStrings #-}
module Clay.AspectRatioSpec where

import           Clay
import           Clay.AspectRatio
import           Common
import           Data.Ratio
import           Test.Hspec

spec :: Spec
spec = do
  describe "aspect-ratio" $ do

    describe "ratio between two numbers" $ do
        "{aspect-ratio:16/9}" `shouldRenderFrom` aspectRatio $ ratio (16%9)
    describe "ratio as decimal" $ do
        "{aspect-ratio:1/2}" `shouldRenderFrom` aspectRatio $ ratio (0.5 :: Rational)
    describe "ratio as integer" $ do
        "{aspect-ratio:2/1}" `shouldRenderFrom` aspectRatio $ ratio (2 :: Rational)

    describe "<ratio> || auto" $ do
      "{aspect-ratio:16/9 auto}" `shouldRenderFrom` aspectRatio $ fallbackRatio (16 % 9)
    describe "0.5 || auto" $ do
      "{aspect-ratio:1/2 auto}" `shouldRenderFrom` aspectRatio $ fallbackRatio (0.5 :: Rational)
    describe "2" $ do
      "{aspect-ratio:2/1 auto}" `shouldRenderFrom` aspectRatio $ fallbackRatio (2 :: Rational)
