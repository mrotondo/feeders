module Main where
import Types
import Feeder
import Field
import Plant
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (randomIO, mkStdGen)
import Data.Map (empty, elems, foldrWithKey, insert)

import Debug.Trace

main :: IO ()
main = do
    let width = 1680
    let height = 1050
    randomGenSeed <- randomIO
    let randomGen = mkStdGen randomGenSeed
    let (field, randomGen') = randomField (width, height) randomGen
    let worldWithPlants = initialWorld field randomGen'
    let worldWithFeeders = addRandomFeeders 10 worldWithPlants
    play (FullScreen (width, height)) (light black) 60 (initialAppState worldWithFeeders) drawWorld handleEvent iterateAppState

initialWorld field randomGen = World { worldFeeders = empty
                                     , worldNextFeederID = 1
                                     , worldField = field
                                     , worldRandomGen = randomGen
                                     }

initialAppState world = AppState world (0, 0)

-- Graphics 

drawWorld :: AppState -> Picture
drawWorld (AppState world mouseLocation) = Pictures [drawField (worldField world), drawFeeders (worldFeeders world)]

drawField :: Field -> Picture
drawField field = Pictures $ map drawPlant (plants field)

drawPlant :: Plant -> Picture
drawPlant plant@(Plant _ _ (x, y)) = let
    size = plantSize plant
    color = plantColor plant
  in
    Translate x y $ Color color $ ThickCircle size (size * 2)

drawFeeders :: Feeders -> Picture
drawFeeders feeders = Pictures $ map drawFeeder (elems feeders)

drawFeeder :: Feeder -> Picture
drawFeeder feeder = let 
    (x, y) = feederLocation feeder
    in Translate x y $ Pictures $ [ Color (dark $ dark red) $ ThickCircle 10 5
                                  , Color green $ ThickArc 180 (180 + (180 * (feederFood feeder))) 10 5
                                  , Color (dark cyan) $ ThickArc 180 (180 - (180 * (feederWater feeder))) 10 5
                                  ]

-- Iteration

handleEvent :: Event -> AppState -> AppState
handleEvent (EventKey key state modifiers newMouseLocation) (AppState world oldMouseLocation) = AppState world newMouseLocation
handleEvent (EventMotion newMouseLocation) (AppState world oldMouseLocation) = AppState world newMouseLocation

iterateAppState :: TimeInterval -> AppState -> AppState
iterateAppState seconds (AppState world mouseLocation) = let 
    iteratedWorld = iterateWorld seconds world
  in
    AppState iteratedWorld mouseLocation

iterateWorld :: TimeInterval -> World -> World
iterateWorld seconds world = foldrWithKey (\feederID feeder worldAccum -> let 
    (iteratedFeeder, affectedField) = iterateFeeder world (worldFeeders worldAccum) (worldField worldAccum) feeder seconds
    (iteratedField, newGen) = iterateField affectedField (worldRandomGen worldAccum)
    newFeeders = insert feederID iteratedFeeder (worldFeeders worldAccum)
    in (world { worldFeeders = newFeeders, worldField = iteratedField, worldRandomGen = newGen })) 
    (world { worldFeeders = empty }) (worldFeeders world)