module Main where
import World
import Feeder
import Field
import Plant
import Geometry
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = do
    let width = 1680
    let height = 1050
    field <- randomField (width, height)
    feeders <- randomFeeders 100 field
    play (FullScreen (1680, 1050)) (light black) 60 (initialWorld field feeders) drawWorld handleEvent iterateWorld

drawWorld :: World -> Picture
drawWorld world = Pictures [drawField (worldField world), drawFeeders (worldFeeders world)]

drawField :: Field -> Picture
drawField field = Pictures $ map drawPlant (plants field)

drawPlant :: Plant -> Picture
drawPlant (Plant (Food amount) (x, y)) = 
    Translate x y $ Color (makeColor 0 1 0 0.5) $ ThickCircle (amount * 10) (amount * 20)

drawFeeders :: Feeders -> Picture
drawFeeders feeders = Pictures $ map drawFeeder feeders

drawFeeder :: Feeder -> Picture
drawFeeder feeder = let 
    (x, y) = feederLocation feeder
    in Translate x y $ Pictures $ [ Color (dark $ dark red) $ ThickCircle 10 5
                                  , Color green $ ThickArc 180 (180 + (180 * (feederFood feeder))) 10 5
                                  , Color (dark cyan) $ ThickArc 0 180 10 5
                                  ]

handleEvent :: Event -> World -> World
handleEvent (EventKey key state modifiers newMouseLocation) world = world
handleEvent (EventMotion newMouseLocation) world = world { worldMouseLocation = newMouseLocation }

iterateWorld :: Float -> World -> World
iterateWorld seconds world = let 
    oldFeeders = (worldFeeders world)
    oldField = (worldField world)
    (iteratedFeeders, iteratedField) = foldr (\feeder (feedersAccum, fieldAccum) -> let 
        (newFeeder, newField) = iterateFeeder fieldAccum (seconds * 3) feeder
        in (newFeeder:feedersAccum, newField)) ([], oldField) oldFeeders
    movedFeeders = map (moveFeeder (worldField world) (seconds * 3)) iteratedFeeders
  in
    world { worldFeeders = movedFeeders, worldField = iteratedField }