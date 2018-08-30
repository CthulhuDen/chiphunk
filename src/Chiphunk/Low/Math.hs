module Chiphunk.Low.Math where

import Data.VectorSpace

fClamp :: Double -> Double -> Double -> Double
fClamp x a b
  | x < a     = a
  | x > b     = b
  | otherwise = x

fLerp :: Double -> Double -> Double -> Double
fLerp = lerp

fLerpConst :: Double -> Double -> Double -> Double
fLerpConst x y a = x + fClamp (y - x) (-a) a
