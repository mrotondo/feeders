module Math where

clamp :: Float -> Float -> Float -> Float
clamp minVal maxVal valToBeClamped = min maxVal $ max minVal valToBeClamped