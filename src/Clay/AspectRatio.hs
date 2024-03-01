{-# LANGUAGE OverloadedStrings #-}

module Clay.AspectRatio (
  -- * Aspect ratio type.
  AspectRatio,
  AspectRatioValue (..),

  -- * Aspect ratio constructors.
  ratio,
  fallbackRatio,
) where

import Clay.Common
import Clay.Property
import Data.Ratio (Ratio, denominator, numerator)

data AspectRatio
  = OtherAspectRatio Value
  | RatioAspectRatio Rational
  | FallbackAspectRatio AspectRatio

class AspectRatioValue a where
  toAspectRatio :: a -> AspectRatio

instance Val AspectRatio where
  value (OtherAspectRatio v) = v
  value (FallbackAspectRatio a) = "auto " <> value a
  value (RatioAspectRatio r) =
    Value
      $ Plain
      $ mconcat
        [ cssNumberText (realToFrac $ numerator r)
        , "/"
        , cssNumberText (realToFrac $ denominator r)
        ]

instance (Integral a) => AspectRatioValue (Ratio a) where
  toAspectRatio = RatioAspectRatio . toRational

instance AspectRatioValue Value where
  toAspectRatio = OtherAspectRatio

instance (Auto a, AspectRatioValue v) => AspectRatioValue (v, a) where
  toAspectRatio (v, _) = FallbackAspectRatio (toAspectRatio v)

instance Auto AspectRatio where auto = OtherAspectRatio autoValue
instance Initial AspectRatio where initial = OtherAspectRatio initialValue

ratio :: Rational -> AspectRatio
ratio = RatioAspectRatio

fallbackRatio :: (AspectRatioValue v) => v -> AspectRatio -> AspectRatio
fallbackRatio = curry toAspectRatio