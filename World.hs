module World where

import Types
import System.Random (StdGen)
import Field
import Feeder
import Data.Map (empty, insert, delete)
import Geometry

createWorld :: StdGen -> (Int, Int) -> World
createWorld randomGen size = let
    (field, randomGen') = randomField size randomGen
    worldWithPlants = initialWorld field randomGen'
    worldWithFeeders = addRandomFeeders 100 worldWithPlants
  in
    worldWithFeeders

initialWorld field randomGen = World { worldFeeders = empty
                                     , worldNextFeederID = 1
                                     , worldField = field
                                     , worldRandomGen = randomGen
                                     , worldTargetedPlants = empty
                                     }