module Predator where
import Graphics.Gloss
import Types
import System.Random (random)
import qualified Data.Map as Map

newPredator :: Point -> Predator
newPredator loc = Predator { predatorLocation = loc
                           , predatorTargetFeederID = Nothing
                           }

addRandomPredators :: Int -> World -> World
addRandomPredators numPredators world = case numPredators of
    0 -> world
    _ -> addRandomPredators (numPredators - 1) newWorld
  where
    width = fromIntegral $ fieldWidth (worldField world)
    height = fromIntegral $ fieldHeight (worldField world)
    scaleX = ((-) (width / 2)) . ((*) width)
    scaleY = ((-) (height / 2)) . ((*) height)
    randomGen = worldRandomGen world
    (x, randomGen')  = random randomGen
    (y, randomGen'') = random randomGen'
    scaledX = scaleX x
    scaledY = scaleY y
    predatorID = worldNextPredatorID world
    newPredators = Map.insert predatorID (newPredator (scaledX, scaledY)) (worldPredators world)
    newWorld = world { worldPredators = newPredators, worldNextPredatorID = predatorID + 1, worldRandomGen = randomGen''}
