module World where

import Types
import System.Random (StdGen)
import Field
import Feeder
import Predator
import Data.Map (empty, insert, delete)
import Geometry

createWorld :: StdGen -> (Int, Int) -> World
createWorld randomGen size = let
    world = initialWorld randomGen size
    worldWithField = addRandomField world
    worldWithFeeders = addRandomFeeders 100 worldWithField
    worldWithPredators = addRandomPredators 5 worldWithFeeders
  in
    worldWithPredators

initialWorld randomGen size = World { worldFeeders = empty
                                    , worldNextFeederID = 1
                                    , worldPredators = empty
                                    , worldNextPredatorID = 1
                                    , worldField = emptyField size
                                    , worldRandomGen = randomGen
                                    , worldTargetedPlants = empty
                                    }
