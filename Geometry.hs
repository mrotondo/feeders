module Geometry where

type Vector2 = (Float, Float)
type Location = Vector2
type Movement = Vector2

add :: Vector2 -> Vector2 -> Vector2
add (xA, yA) (xB, yB) = (xA + xB, yA + yB)

lerp :: Vector2 -> Vector2 -> Float -> Vector2
lerp (xA, yA) (xB, yB) percent = ((xB - xA) * percent, (yB - yA) * percent)