module HSL
  ( hue
  , sat
  , HSL
  ) where

import Data.Tuple.Nested ((/\))
import Data.Show (class Show)
import Data.TemplateString ((<->))

data HSL = HSL Int Int Int

instance showHSL :: Show HSL where
  show (HSL _hue _sat _lit) = "hsl(${hue}deg, ${sat}%, ${lit}%)"
                                 <-> [ "hue" /\ _hue
                                     , "sat" /\ _sat
                                     , "lit" /\ _lit
                                     ]

-- | create HSL color by providing hue, saturation and lightness
hsl :: Int -> Int -> Int -> HSL
hsl _hue _sat _lit = HSL _hue _sat _lit

-- | create normal HSL color by providing hue
hue :: Int -> HSL
hue _hue = hsl _hue 100 50

-- | create HSL color of specific hue by providing saturation
sat :: Int -> Int -> HSL
sat _hue _sat = hsl _hue _sat 50
