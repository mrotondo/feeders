module Main where
import Types
import World
import Feeder
import Field
import Predator
import Plant
import Geometry
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import System.Random (randomIO, mkStdGen)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
    let width = 1680
    let height = 1050
    randomGenSeed <- randomIO
    let randomGen = mkStdGen randomGenSeed
    let world = createWorld randomGen (width, height)
    play (FullScreen (width, height)) (light black) 60 (initialAppState world) drawWorld handleEvent iterateAppState

initialAppState world = AppState world (0, 0)

-- Graphics 

drawWorld :: AppState -> Picture
drawWorld (AppState world mouseLocation) = Pictures [drawField (worldField world), drawFeeders (worldFeeders world), drawPredators (worldPredators world)]

drawField :: Field -> Picture
drawField field = Pictures $ map drawPlant (plants field)

drawPlant :: Plant -> Picture
drawPlant plant@(Plant _ _ (x, y)) = let
    size = plantSize plant
    color = plantColor plant
  in
    Translate x y $ Color color $ ThickCircle size (size * 2)

drawFeeders :: Feeders -> Picture
drawFeeders feeders = Pictures $ map drawFeeder (Map.elems feeders)

drawFeeder :: Feeder -> Picture
drawFeeder feeder = let 
    (x, y) = feederLocation feeder
    in Translate x y $ Pictures $ [ Color (dark $ dark red) $ ThickCircle 10 10
                                  , Color green $ ThickArc 180 (180 + (180 * (feederFood feeder))) 10 5
                                  , Color (dark cyan) $ ThickArc 180 (180 - (180 * (feederWater feeder))) 10 5
                                  , Color white $ Line [(0, 0), mulSV 20.0 (feederThreatDirection feeder)]
                                  ]

drawPredators :: Predators -> Picture
drawPredators predators = Pictures $ map drawPredator (Map.elems predators)

drawPredator :: Predator -> Picture
drawPredator predator = let 
    (x, y) = predatorLocation predator
    in Translate x y $ Color (bright red) $ ThickCircle 20 20

-- Iteration

handleEvent :: Event -> AppState -> AppState
handleEvent (EventKey key state modifiers newMouseLocation) (AppState world oldMouseLocation) = AppState world newMouseLocation
handleEvent (EventMotion newMouseLocation) (AppState world oldMouseLocation) = AppState world newMouseLocation

iterateAppState :: TimeInterval -> AppState -> AppState
iterateAppState seconds (AppState world mouseLocation) = let 
    iteratedWorld = iterateWorld (seconds * 10) world
  in
    AppState iteratedWorld mouseLocation

iterateWorld :: TimeInterval -> World -> World
iterateWorld seconds world = let
    changesFromFeeders = Map.mapWithKey (\feederID feeder -> changesFromFeeder world feederID feeder seconds) (worldFeeders world)
    world' = Map.foldl (\worldAccum changesFromAction -> foldl (\worldAccum' change -> change worldAccum') worldAccum changesFromAction) world changesFromFeeders
    (iteratedField, newGen) = iterateField (worldField world') (worldRandomGen world')
    world'' = world' { worldField = iteratedField, worldRandomGen = newGen }
    changesFromPredators = Map.mapWithKey (\predatorID predator -> changesFromPredator world'' predatorID predator seconds) (worldPredators world'')
    world''' = Map.foldl (\worldAccum changesFromAction -> foldl (\worldAccum' change -> change worldAccum') worldAccum changesFromAction) world'' changesFromPredators
  in 
    world'''
