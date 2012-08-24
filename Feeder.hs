module Feeder where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Field
import Plant
import Geometry
import Math
import qualified Data.Map as Map
import Data.List (minimumBy)

type Feeders = [Feeder]
data Feeder = Feeder { feederLocation       :: Point
                     , feederFood           :: Float
                     , feederWater          :: Float
                     , feederTargetPlantID  :: PlantID
                     }

newFeeder :: Field -> Point -> Feeder
newFeeder field loc = Feeder { feederLocation = loc
                             , feederFood = 1.0
                             , feederWater = 1.0
                             , feederTargetPlantID = closestPlantID field loc
                             }

initialFeeders :: Field -> Feeders
initialFeeders field = map (newFeeder field) [(0, 0), (100, 100), (200, 200), (300, 300), (400, 400)]

closestPlantID :: Field -> Point -> PlantID
closestPlantID field feederLoc = fst $ minimumBy comparePlantDistances (Map.toList (fieldPlants field))
  where
    comparePlantDistances (plantIDA, plantA) (plantIDB, plantB) = compare (distanceToPlant plantA) (distanceToPlant plantB)
    distanceToPlant plant = distance feederLoc (plantLocation plant)

targetPlant :: Field -> Feeder -> Maybe Plant
targetPlant field feeder = Map.lookup (feederTargetPlantID feeder) (fieldPlants field)

targetPlantLocation :: Field -> Feeder -> Point
targetPlantLocation field feeder  = case (targetPlant field feeder) of
                                      Just plant -> plantLocation plant
                                      Nothing -> (0, 0)

vectorToTargetPlant :: Field -> Feeder -> Vector
vectorToTargetPlant field feeder = difference (feederLocation feeder) (targetPlantLocation field feeder)

distanceToTargetPlant :: Field -> Feeder -> Float
distanceToTargetPlant field feeder = magV $ vectorToTargetPlant field feeder

moveTowardsLocation :: Point -> Float -> Feeder -> Feeder
moveTowardsLocation targetLocation seconds feeder = feeder { feederLocation=oldLocation `moveBy` movement }
  where
    oldLocation = feederLocation feeder
    speedPerSecond = 1.8
    movement = lerp oldLocation targetLocation (speedPerSecond * seconds)

moveBy :: Point -> Vector -> Point
moveBy = add

moveFeeder :: Field -> Float -> Feeder -> Feeder
moveFeeder field seconds feeder = moveTowardsLocation (targetPlantLocation field feeder) seconds feeder

iterateFeeder :: Field -> Float -> Feeder -> (Feeder, Field)
iterateFeeder field seconds feeder = foldl (\(feederAccum, fieldAccum) effect -> effect field seconds feederAccum) (feeder, field) lifeEffects

type Effect = Field -> Float -> Feeder -> (Feeder, Field)

lifeEffects :: [Effect]
lifeEffects = [getHungrier, getThirstier, maybeEat]--, maybeDrink]

getHungrier :: Effect
getHungrier field seconds feeder = (feeder { feederFood = clamp 0.0 1.0 newFeederFood }, field)
  where
    oldFeederFood = (feederFood feeder)
    hungerPerSecond = 0.13
    newHunger = hungerPerSecond * seconds
    newFeederFood = oldFeederFood - newHunger

getThirstier :: Effect
getThirstier field seconds feeder = (feeder { feederWater = clamp 0.0 1.0 newFeederWater }, field)
  where
    oldFeederWater = (feederWater feeder)
    thirstPerSecond = 0.1
    newThirst = thirstPerSecond * seconds
    newFeederWater = oldFeederWater - newThirst

maybeEat :: Effect
maybeEat field seconds feeder = (feeder { feederFood = clamp 0.0 1.0 newFeederFood, feederTargetPlantID = newTargetPlantID }, newField)
  where
    oldFeederFood = (feederFood feeder)
    foodEatenPerSecond = 0.17
    foodEaten = case (distanceToTargetPlant field feeder) of
                  dist | dist < 2.5  -> (foodEatenPerSecond * seconds)
                  _                  -> 0
    newFeederFood = oldFeederFood + foodEaten
    newPlant = case (targetPlant field feeder) of
                 Just (Plant (Food amount) location) -> Plant (Food $ clamp 0.0 1.0 (amount - foodEaten)) location
                 Nothing -> Plant (Food 0) (0, 0)
    oldPlants = fieldPlants field
    newField = case newPlant of
                Plant (Food amount) location | amount > 0.01 -> field { fieldPlants = Map.insert (feederTargetPlantID feeder) newPlant oldPlants }
                _                                            -> field { fieldPlants = Map.delete (feederTargetPlantID feeder) oldPlants}
    oldTargetPlantID = feederTargetPlantID feeder
    newTargetPlantID = case newPlant of
                        Plant (Food amount) location | amount > 0.01 -> oldTargetPlantID
                        _                                            -> closestPlantID newField (feederLocation feeder)
