module Main where
import Types
import Feeder
import Field
import Plant
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = do
    let width = 1680
    let height = 1050
    field <- randomField (width, height)
    feeders <- randomFeeders 100 field
    play (FullScreen (1680, 1050)) (light black) 60 (initialAppState feeders field) drawWorld handleEvent iterateAppState

initialAppState feeders field = AppState (World feeders field) (0, 0)

drawWorld :: AppState -> Picture
drawWorld (AppState (World feeders field) mouseLocation) = Pictures [drawField field, drawFeeders feeders]

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

handleEvent :: Event -> AppState -> AppState
handleEvent (EventKey key state modifiers newMouseLocation) (AppState world oldMouseLocation) = AppState world newMouseLocation
handleEvent (EventMotion newMouseLocation) (AppState world oldMouseLocation) = AppState world newMouseLocation

iterateAppState :: Float -> AppState -> AppState
iterateAppState seconds (AppState world mouseLocation) = let 
    iteratedWorld = iterateWorld (seconds * 3) world
  in
    AppState iteratedWorld mouseLocation

iterateWorld :: Float -> World -> World
iterateWorld seconds (World feeders field) = foldr (\feeder (World feedersAccum fieldAccum) -> let 
    (iteratedFeeder, iteratedField) = iterateFeeder fieldAccum seconds feeder
    movedFeeder = moveFeeder iteratedField seconds iteratedFeeder
    in (World (movedFeeder:feedersAccum) iteratedField)) (World [] field) feeders
