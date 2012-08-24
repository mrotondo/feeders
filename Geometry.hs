module Geometry where
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

add :: Vector -> Vector -> Vector
add (xA, yA) (xB, yB) = (xA + xB, yA + yB)

lerp :: Vector -> Vector -> Float -> Vector
lerp (xA, yA) (xB, yB) percent = ((xB - xA) * percent, (yB - yA) * percent)

difference :: Point -> Point -> Vector
difference (xA, yA) (xB, yB) = (xA - xB, yA - yB)

distance :: Point -> Point -> Float
distance pA pB = magV $ difference pA pB
