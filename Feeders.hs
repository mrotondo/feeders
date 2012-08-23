module Main where
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
    play (FullScreen (1680, 1050)) (light black) 60 (initialWorld field) drawWorld handleEvent iterateWorld

data World = World { worldField :: Field, worldFeeders :: Feeders, worldMouseLocation :: Location}

initialWorld :: Field -> World
initialWorld field = World { worldField = field, worldFeeders = initialFeeders, worldMouseLocation = (0, 0) }

initialFeeders :: Feeders
initialFeeders = map newFeeder [(0, 0), (100, 100), (200, 200), (300, 300), (400, 400)]

drawWorld :: World -> Picture
drawWorld world = Pictures [drawField (worldField world), drawFeeders (worldFeeders world)]

drawField :: Field -> Picture
drawField field = let 
    offsetX = - fromIntegral (fieldWidth field)  / 2
    offsetY = - fromIntegral (fieldHeight field) / 2 
  in   
    Translate offsetX offsetY
    $ Pictures 
    $ map drawPlant (fieldPlants field)

drawPlant :: Plant -> Picture
drawPlant (Plant (Food amount) (x, y)) = Translate x y $ Color green $ Circle (amount * 10)

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
    iteratedFeeders = map (iterateFeeder seconds) oldFeeders
    movedFeeders = map (moveTowardsLocation (worldMouseLocation world) seconds) iteratedFeeders
  in
    world { worldFeeders = movedFeeders }