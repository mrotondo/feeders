module Feeder where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Types
import Field
import Plant
import Geometry
import Math
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (replicateM)
import System.Random (randomIO)
import Data.List (minimumBy, sortBy)
import Data.Function (on)
import Data.Maybe (fromJust)

newFeeder :: Field -> Point -> Feeder
newFeeder field loc = Feeder { feederLocation = loc
                             , feederFood = 1.0
                             , feederWater = 1.0
                             , feederTargetPlantID = Nothing
                             , feederBehaviorName = DoingNothing
                             , feederBehaviorPersistencePreference = 0.0
                             }

randomFeeders :: Int -> Field -> IO Feeders
randomFeeders numFeeders field = do
    let width = fromIntegral $ fieldWidth field
    let height = fromIntegral $ fieldHeight field
    let scaleX = ((-) (width / 2)) . ((*) width)
    let scaleY = ((-) (height / 2)) . ((*) height)
    xs <- replicateM numFeeders (randomIO :: IO Float)
    ys <- replicateM numFeeders (randomIO :: IO Float)
    let scaledXs = map scaleX xs
    let scaledYs = map scaleY ys
    let locations = zip scaledXs scaledYs
    return $ map (newFeeder field) locations

closestPlantID :: Map PlantID Plant -> Point -> PlantID
closestPlantID plantMap feederLoc = fst $ minimumBy comparePlantDistances (Map.toList plantMap)
  where
    comparePlantDistances (plantIDA, plantA) (plantIDB, plantB) = compare (distanceToPlant plantA) (distanceToPlant plantB)
    distanceToPlant plant = distance feederLoc (plantLocation plant)

closestFoodPlantID :: Field -> Point -> PlantID
closestFoodPlantID field feederLoc = closestPlantID foodPlants feederLoc
  where
    foodPlants = Map.filter (\plant -> case plant of
                                         Plant (Food amount) location -> True
                                         _                            -> False) (fieldPlants field)

closestWaterPlantID :: Field -> Point -> PlantID
closestWaterPlantID field feederLoc = closestPlantID waterPlants feederLoc
  where
    waterPlants = Map.filter (\plant -> case plant of
                                          Plant (Water amount) location -> True
                                          _                             -> False) (fieldPlants field)

targetPlant :: Field -> Feeder -> Maybe Plant
targetPlant field feeder = case (feederTargetPlantID feeder) of
                             Just targetPlantID -> Map.lookup targetPlantID (fieldPlants field)
                             Nothing            -> Nothing

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

iterateFeeder :: World -> Feeders -> Field -> Feeder -> TimeInterval -> (Feeder, Field)
iterateFeeder previousWorld feedersSoFar fieldSoFar feeder seconds = let
    affectedFeeder = foldr (\effect feederAccum -> effect previousWorld feederAccum seconds) feeder lifeEffects
    urgencyBehaviorTuples = generatePossibleBehaviors previousWorld affectedFeeder
  in 
    doMostUrgentBehavior previousWorld feedersSoFar fieldSoFar affectedFeeder seconds urgencyBehaviorTuples

lifeEffects :: [Effect]
lifeEffects = [getHungrier, getThirstier]

getHungrier :: Effect
getHungrier previousWorld feeder seconds = feeder { feederFood = clamp 0.0 1.0 newFeederFood }
  where
    oldFeederFood = (feederFood feeder)
    hungerPerSecond = 0.09
    newHunger = hungerPerSecond * seconds
    newFeederFood = oldFeederFood - newHunger

getThirstier :: Effect
getThirstier previousWorld feeder seconds = feeder { feederWater = clamp 0.0 1.0 newFeederWater }
  where
    oldFeederWater = (feederWater feeder)
    thirstPerSecond = 0.02
    newThirst = thirstPerSecond * seconds
    newFeederWater = oldFeederWater - newThirst

doMostUrgentBehavior :: World -> Feeders -> Field -> Feeder -> TimeInterval -> [(Urgency, Behavior)] -> (Feeder, Field)
doMostUrgentBehavior previousWorld feedersSoFar fieldSoFar feeder seconds urgencyBehaviorTuples = let
    mostUrgentBehavior = snd . head $ sortBy (compare `on` (((-) 1.0) . fst)) $ urgencyBehaviorTuples
  in
    doBehavior previousWorld feedersSoFar fieldSoFar feeder seconds mostUrgentBehavior

doBehavior ::  World -> Feeders -> Field -> Feeder -> TimeInterval -> Behavior -> (Feeder, Field)
doBehavior previousWorld feedersSoFar fieldSoFar feeder seconds (Behavior behaviorName actions) = let
    (modifiedFeeder, modifiedField) = foldl (\(feederAccum, fieldAccum) action -> action previousWorld feedersSoFar fieldAccum feederAccum seconds) (feeder, fieldSoFar) actions
    oldBehaviorName = feederBehaviorName feeder
    behaviorPersistencePreferenceFalloffRate = 0.8
    oldBehaviorPersistencePreference = feederBehaviorPersistencePreference feeder
    feederWithNewBehaviorName = case (oldBehaviorName == behaviorName) of
        True  -> modifiedFeeder { feederBehaviorPersistencePreference = oldBehaviorPersistencePreference * (behaviorPersistencePreferenceFalloffRate ** seconds) }
        False -> modifiedFeeder { feederBehaviorName = behaviorName
                                , feederBehaviorPersistencePreference = 1.0
                                }
  in
    (feederWithNewBehaviorName, modifiedField)

generatePossibleBehaviors :: World -> Feeder -> [(Urgency, Behavior)]
generatePossibleBehaviors previousWorld feeder = map ($ (previousWorld, feeder)) desires 

desires :: [Desire]
desires = [hunger, thirst]

hunger :: Desire
hunger (previousWorld, feeder) = (foodUrgency feeder, Behavior Eating [targetFood, moveTowardsTarget, eat])

thirst :: Desire
thirst (previousWorld, feeder) = (thirstUrgency feeder, Behavior Drinking [targetWater, moveTowardsTarget, drink])

foodUrgency :: Feeder -> Urgency
foodUrgency feeder = let
    baseUrgency = (1.0 - (feederFood feeder)) ^ 2
    persistencePreference = case ((feederBehaviorName feeder) == Eating) of
                                True  -> (feederBehaviorPersistencePreference feeder)
                                False -> 0.0
  in
    baseUrgency + persistencePreference

thirstUrgency :: Feeder -> Urgency
thirstUrgency feeder = let
    baseUrgency = (1.0 - (feederWater feeder)) ^ 4
    persistencePreference = case ((feederBehaviorName feeder) == Drinking) of
                                True  -> (feederBehaviorPersistencePreference feeder)
                                False -> 0.0
  in
    baseUrgency + persistencePreference


targetFood :: Action
targetFood previousWorld feedersSoFar fieldSoFar feeder seconds = let
    targetableField = fieldWithOnlyUntargetedPlants feedersSoFar fieldSoFar
    alreadyTargetingFood = isFood (targetPlant fieldSoFar feeder)
    currentTargetHasBeenClaimed = isTargeted feedersSoFar (feederTargetPlantID feeder)
    newTargetPlantID = case (alreadyTargetingFood && (not currentTargetHasBeenClaimed)) of
                         True  -> feederTargetPlantID feeder
                         False -> Just $ closestFoodPlantID targetableField (feederLocation feeder)
  in
    (feeder { feederTargetPlantID = newTargetPlantID }, fieldSoFar)

targetWater :: Action
targetWater previousWorld feedersSoFar fieldSoFar feeder seconds = let
    targetableField = fieldWithOnlyUntargetedPlants feedersSoFar fieldSoFar
    alreadyTargetingWater = isWater (targetPlant fieldSoFar feeder)
    currentTargetHasBeenClaimed = isTargeted feedersSoFar (feederTargetPlantID feeder)
    newTargetPlantID = case (alreadyTargetingWater && (not currentTargetHasBeenClaimed)) of
                         True  -> feederTargetPlantID feeder
                         False -> Just $ closestWaterPlantID targetableField (feederLocation feeder)
  in
    (feeder { feederTargetPlantID = newTargetPlantID }, fieldSoFar)

moveTowardsTarget :: Action
moveTowardsTarget previousWorld feedersSoFar fieldSoFar feeder seconds = let
    movedFeeder = moveTowardsLocation (targetPlantLocation fieldSoFar feeder) seconds feeder
  in
    (movedFeeder, fieldSoFar)

eat :: Action
eat previousWorld feedersSoFar fieldSoFar feeder seconds = let
    foodEatenPerSecond = 0.25
    foodEaten = if (distanceToTargetPlant fieldSoFar feeder) < 2.5 then (foodEatenPerSecond * seconds) else 0
    eatenPlant = case (targetPlant fieldSoFar feeder) of
                 Just (Plant (Food amount) location) -> Plant (Food $ clamp 0.0 1.0 (amount - foodEaten)) location
                 _ -> Plant Dead (0, 0)
    fieldWithEatenPlant = case eatenPlant of
                Plant (Food amount) location | amount > 0.01 -> fieldSoFar { fieldPlants = Map.insert (fromJust (feederTargetPlantID feeder)) eatenPlant (fieldPlants fieldSoFar) }
                _                                            -> fieldSoFar { fieldPlants = Map.delete (fromJust (feederTargetPlantID feeder)) (fieldPlants fieldSoFar)}
  in
    (feeder { feederFood = clamp 0.0 1.0 ((feederFood feeder) + foodEaten) }, fieldWithEatenPlant)

drink :: Action
drink previousWorld feedersSoFar fieldSoFar feeder seconds = let
    waterDrankPerSecond = 0.35
    waterDrank = if (distanceToTargetPlant fieldSoFar feeder) < 2.5 then (waterDrankPerSecond * seconds) else 0
    drankPlant = case (targetPlant fieldSoFar feeder) of
                 Just (Plant (Water amount) location) -> Plant (Water $ clamp 0.0 1.0 (amount - waterDrank)) location
                 _ -> Plant Dead (0, 0)
    fieldWithDrankPlant = case drankPlant of
                Plant (Water amount) location | amount > 0.01 -> fieldSoFar { fieldPlants = Map.insert (fromJust (feederTargetPlantID feeder)) drankPlant (fieldPlants fieldSoFar) }
                _                                            -> fieldSoFar { fieldPlants = Map.delete (fromJust (feederTargetPlantID feeder)) (fieldPlants fieldSoFar)}
  in
    (feeder { feederWater = clamp 0.0 1.0 ((feederWater feeder) + waterDrank) }, fieldWithDrankPlant)