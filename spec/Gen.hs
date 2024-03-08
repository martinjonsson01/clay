module Generators () where

import Clay.Selector

genSelector :: Gen Selector 
genSelector = oneof [genElement, genChild]

genElement :: Int -> Gen Selector
genElement size = element size <$> arbitrary

genChild :: Int -> Gen Selector
genChild size = do
  s1 <- genSelector
  s2 <- genSelector
  return (s1 |> s2)
