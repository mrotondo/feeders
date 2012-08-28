module Main where
import Types
import Feeder
import Field
import Plant
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Debug.Trace

main :: IO ()
main = do
    let width = 1680
    let height = 1050
    field <- randomField (width, height)
    feeders <- randomFeeders 1 field
    play (FullScreen (1680, 1050)) (light black) 60 (initialAppState feeders field) drawWorld handleEvent iterateAppState

initialAppState feeders field = AppState (World feeders field) (0, 0)

drawWorld :: AppState -> Picture
drawWorld (AppState (World feeders field) mouseLocation) = Pictures [drawField field, drawFeeders feeders]

drawField :: Field -> Picture
drawField field = Pictures $ map drawPlant (plants field)

drawPlant :: Plant -> Picture
drawPlant plant@(Plant plantType (x, y)) = let
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

handleEvent :: Event -> AppState -> AppState
handleEvent (EventKey key state modifiers newMouseLocation) (AppState world oldMouseLocation) = AppState world newMouseLocation
handleEvent (EventMotion newMouseLocation) (AppState world oldMouseLocation) = AppState world newMouseLocation

iterateAppState :: TimeInterval -> AppState -> AppState
iterateAppState seconds (AppState world mouseLocation) = let 
    iteratedWorld = iterateWorld seconds world
  in
    AppState iteratedWorld mouseLocation

iterateWorld :: TimeInterval -> World -> World
iterateWorld seconds world@(World feeders field) = foldr (\feeder (World feedersAccum fieldAccum) -> let 
    (iteratedFeeder, affectedField) = iterateFeeder world fieldAccum feeder seconds
    iteratedField = iterateField affectedField
    in (World (iteratedFeeder:feedersAccum) iteratedField)) (World [] field) feeders