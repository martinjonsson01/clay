{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Clay.AspectRatio (
  -- * Aspect ratio type.
  AspectRatio,

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
  value (FallbackAspectRatio a) = value a <> " auto" 
  value (RatioAspectRatio r) =
    Value
      $ Plain
      $ mconcat
        [ cssNumberText (realToFrac $ numerator r)
        , "/"
        , cssNumberText (realToFrac $ denominator r)
        ]

instance (Integral a, Integer ~ a) => AspectRatioValue (Ratio a) where
  toAspectRatio = RatioAspectRatio . toRational

instance AspectRatioValue Value where
  toAspectRatio = OtherAspectRatio

instance Auto AspectRatio where auto = OtherAspectRatio autoValue
instance Initial AspectRatio where initial = OtherAspectRatio initialValue

-- | Equivalent to <number [0,∞]> [ / <number [0,∞]> ]?
ratio :: (AspectRatioValue v) => v -> AspectRatio
ratio = toAspectRatio

-- | Equivalent to <ratio> || auto
fallbackRatio :: (AspectRatioValue v) => v -> AspectRatio
fallbackRatio = FallbackAspectRatio . toAspectRatio