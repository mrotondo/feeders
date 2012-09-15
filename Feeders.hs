module Main where
import Types
import Feeder
import Field
import Plant
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (randomIO, mkStdGen)

import Debug.Trace

main :: IO ()
main = do
    let width = 1680
    let height = 1050
    randomGenSeed <- randomIO
    let randomGen = mkStdGen randomGenSeed
    let (field, newRandomGen) = randomField (width, height) randomGen
    feeders <- randomFeeders 10 field
    let world = initialWorld feeders field newRandomGen
    play (FullScreen (1680, 1050)) (light black) 60 (initialAppState world) drawWorld handleEvent iterateAppState

initialWorld feeders field randomGen = World { worldFeeders = feeders
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
drawFeeders feeders = Pictures $ map drawFeeder feeders

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
iterateWorld seconds world = foldr (\feeder worldAccum -> let 
    (iteratedFeeder, affectedField) = iterateFeeder world (worldFeeders worldAccum) (worldField worldAccum) feeder seconds
    (iteratedField, newGen) = iterateField affectedField (worldRandomGen worldAccum)
    in (World { worldFeeders = iteratedFeeder:(worldFeeders worldAccum), worldField = iteratedField, worldRandomGen = newGen })) 
    (world { worldFeeders = [] }) (worldFeeders world)