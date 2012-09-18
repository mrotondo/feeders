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
import System.Random (RandomGen, random)
import Data.List (minimumBy, sortBy)
import Data.Function (on)
import Data.Maybe (fromJust, isJust)

newFeeder :: Point -> Feeder
newFeeder loc = Feeder { feederLocation = loc
                       , feederFood = 1.0
                       , feederWater = 1.0
                       , feederTargetPlantID = Nothing
                       , feederBehaviorName = DoingNothing
                       , feederBehaviorPersistencePreference = 0.0
                       }

addRandomFeeders :: Int -> World -> World
addRandomFeeders numFeeders world = case numFeeders of
    0 -> world
    _ -> addRandomFeeders (numFeeders - 1) newWorld
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
    feederID = worldNextFeederID world
    newFeeders = Map.insert feederID (newFeeder (scaledX, scaledY)) (worldFeeders world)
    newWorld = world { worldFeeders = newFeeders, worldNextFeederID = feederID + 1, worldRandomGen = randomGen''}

closestPlantID :: Plants -> Point -> PlantID
closestPlantID plantMap feederLoc = fst $ minimumBy comparePlantDistances (Map.toList plantMap)
  where
    comparePlantDistances (plantIDA, plantA) (plantIDB, plantB) = compare (distanceToPlant plantA) (distanceToPlant plantB)
    distanceToPlant plant = distance feederLoc (plantLocation plant)

closestPlantIDWithType :: PlantType -> Plants -> Point -> PlantID
closestPlantIDWithType plantType plants feederLoc = closestPlantID plantsOfType feederLoc
  where
    plantsOfType = Map.filter (isOfType plantType) plants

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

movementTowardsLocation :: Point -> Float -> Feeder -> Vector
movementTowardsLocation targetLocation seconds feeder = movement
  where
    speedPerSecond = 1.8
    vectorToTargetLocation = difference targetLocation (feederLocation feeder)
    movement = mulSV (speedPerSecond * seconds) vectorToTargetLocation

changesFromFeeder :: World -> FeederID -> Feeder -> TimeInterval -> [WorldChange]
changesFromFeeder previousWorld feederID feeder seconds = let
    urgencyBehaviorTuples = generatePossibleBehaviors previousWorld feeder
    mostUrgentBehavior = snd . head $ sortBy (compare `on` (((-) 1.0) . fst)) $ urgencyBehaviorTuples
    behaviorChanges = changesFromBehavior previousWorld feederID feeder seconds mostUrgentBehavior
    lifeEffectChanges = changesFromLifeEffects previousWorld feederID feeder seconds lifeEffects
  in 
    behaviorChanges ++ lifeEffectChanges -- We calculate urgency based on previous feeder state, so apply effects after actions

changesFromLifeEffects :: World -> FeederID -> Feeder -> TimeInterval -> [Effect] -> [WorldChange]
changesFromLifeEffects previousWorld feederID feeder seconds effects = 
    map (\effect -> effect previousWorld feederID feeder seconds) effects

changesFromBehavior ::  World -> FeederID -> Feeder -> TimeInterval -> Behavior -> [WorldChange]
changesFromBehavior previousWorld feederID feeder seconds (Behavior behaviorName actions) = let
    actionChanges = map (\action -> action previousWorld feederID seconds) actions
    behaviorPersistenceChange = changeFromBehaviorPersistence feederID feeder behaviorName seconds
  in
    behaviorPersistenceChange : actionChanges

changeFromBehaviorPersistence :: FeederID -> Feeder -> BehaviorName -> TimeInterval -> WorldChange
changeFromBehaviorPersistence feederID feeder newBehaviorName seconds = (\worldAccum -> 
    setBehaviorPersistence worldAccum feederID newBehaviorName seconds)

lifeEffects :: [Effect]
lifeEffects = [getHungrier, getThirstier]

getHungrier :: Effect
getHungrier previousWorld feederID feeder seconds = (\worldAccum -> let
    newFeeder = fromJust $ Map.lookup feederID (worldFeeders worldAccum)
    oldFeederFood = (feederFood newFeeder)
    hungerPerSecond = 0.09
    newHunger = hungerPerSecond * seconds
    newFeederFood = oldFeederFood - newHunger
    hungrierFeeder = newFeeder { feederFood = clamp 0.0 1.0 newFeederFood }
    newFeeders = Map.insert feederID hungrierFeeder (worldFeeders worldAccum)
  in
    worldAccum { worldFeeders = newFeeders }
  )

getThirstier :: Effect
getThirstier previousWorld feederID feeder seconds = (\worldAccum -> let
    newFeeder = fromJust $ Map.lookup feederID (worldFeeders worldAccum)
    oldFeederWater = (feederWater newFeeder)
    thirstPerSecond = 0.02
    newThirst = thirstPerSecond * seconds
    newFeederWater = oldFeederWater - newThirst
    thirstierFeeder = newFeeder { feederWater = clamp 0.0 1.0 newFeederWater }
    newFeeders = Map.insert feederID thirstierFeeder (worldFeeders worldAccum)
  in
    worldAccum { worldFeeders = newFeeders }
  )

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
targetFood = targetPlantType Food

targetWater :: Action
targetWater = targetPlantType Water

targetPlantType :: PlantType -> Action
targetPlantType plantType previousWorld feederID seconds = (\worldAccum -> let
    newFeeder = fromJust $ Map.lookup feederID (worldFeeders worldAccum)
    oldTargetPlantID = (feederTargetPlantID newFeeder)
    oldTargetPlant = (targetPlant (worldField previousWorld) newFeeder)
    alreadyTargetingCorrectType = if (isJust oldTargetPlant) then isOfType plantType (fromJust oldTargetPlant) else False
    targetPlantStillExists = if (isJust oldTargetPlantID) 
                             then (worldField worldAccum) `contains` (fromJust oldTargetPlantID)
                             else False
    newTargetPlantID = if (alreadyTargetingCorrectType && targetPlantStillExists)
                       then oldTargetPlantID
                       else Just $ closestPlantIDWithType plantType (untargetedPlants worldAccum) (feederLocation newFeeder)
  in
    setFeederTarget worldAccum feederID newFeeder newTargetPlantID
  )

moveTowardsTarget :: Action
moveTowardsTarget previousWorld feederID seconds = (\worldAccum -> let
    newFeeder = fromJust $ Map.lookup feederID (worldFeeders worldAccum)
    movement = movementTowardsLocation (targetPlantLocation (worldField worldAccum) newFeeder) seconds newFeeder
    newFeeder' = newFeeder { feederLocation = (feederLocation newFeeder) `add` movement }
    newFeeders = Map.insert feederID newFeeder' (worldFeeders worldAccum)
  in
    worldAccum { worldFeeders = newFeeders }
  )

eat :: Action
eat previousWorld feederID seconds = (\worldAccum -> let
    newFeeder = fromJust $ Map.lookup feederID (worldFeeders worldAccum)
    foodEatenPerSecond = 0.25
    amountToEat = if (distanceToTargetPlant (worldField worldAccum) newFeeder) < 2.5 then (foodEatenPerSecond * seconds) else 0
    plantID = (fromJust $ feederTargetPlantID newFeeder)
    plant = (targetPlant (worldField worldAccum) newFeeder)
  in
    case plant of
        Nothing     -> worldAccum
        Just plant' -> consume worldAccum feederID newFeeder plantID plant' amountToEat
  )

drink :: Action
drink previousWorld feederID seconds = (\worldAccum -> let
    newFeeder = fromJust $ Map.lookup feederID (worldFeeders worldAccum)
    waterDrankPerSecond = 0.35
    amountToDrink = if (distanceToTargetPlant (worldField worldAccum) newFeeder) < 2.5 then (waterDrankPerSecond * seconds) else 0
    plantID = (fromJust $ feederTargetPlantID newFeeder)
    plant = (targetPlant (worldField worldAccum) newFeeder)
  in
    case plant of
        Nothing     -> worldAccum
        Just plant' -> consume worldAccum feederID newFeeder plantID plant' amountToDrink
  )

--------------------------
-- WORLD CHANGE HELPERS --
--------------------------

setFeederTarget :: World -> FeederID -> Feeder -> Maybe PlantID -> World
setFeederTarget worldAccum feederID oldFeeder newTargetPlantID = let
    newFeeder = fromJust $ Map.lookup feederID (worldFeeders worldAccum)
    newFeeder' = newFeeder { feederTargetPlantID = newTargetPlantID }
    newFeeders = Map.insert feederID newFeeder' (worldFeeders worldAccum)
    oldTargetedPlants = worldTargetedPlants worldAccum
    targetedPlantsWithoutOldTarget = case (feederTargetPlantID newFeeder') of
                                          Nothing               -> oldTargetedPlants
                                          Just oldTargetPlantID -> Map.delete oldTargetPlantID oldTargetedPlants
    newTargetedPlants = Map.insert (fromJust newTargetPlantID) feederID targetedPlantsWithoutOldTarget
  in
    worldAccum { worldFeeders = newFeeders, worldTargetedPlants = newTargetedPlants }

moveFeederBy :: World -> FeederID -> Feeder -> Vector -> World
moveFeederBy worldAccum feederID oldFeeder movement = let
    newFeeder = fromJust $ Map.lookup feederID (worldFeeders worldAccum)
    newFeeder' = newFeeder { feederLocation = (feederLocation newFeeder) `add` movement }
    newFeeders = Map.insert feederID newFeeder' (worldFeeders worldAccum)
  in
    worldAccum { worldFeeders = newFeeders }

-- This only gets called if there is actually a plant to consume, so we can make some assumptions about functions that return Maybe plant
consume :: World -> FeederID -> Feeder -> PlantID -> Plant -> Float -> World
consume worldAccum feederID oldFeeder plantID plant@(Plant plantType oldAmount location) amountToConsume = let
    consumedPlant = Plant plantType (clamp 0.0 1.0 (oldAmount - amountToConsume)) location
    newPlants = Map.insert plantID consumedPlant (fieldPlants (worldField worldAccum))
    newField = (worldField worldAccum) { fieldPlants = newPlants }
    newFeeder = fromJust $ Map.lookup feederID (worldFeeders worldAccum)
    newFeeder' = case plantType of
                     Food  -> newFeeder { feederFood = clamp 0.0 1.0 ((feederFood newFeeder) + amountToConsume) }
                     Water -> newFeeder { feederWater = clamp 0.0 1.0 ((feederWater newFeeder) + amountToConsume) }
    newFeeders = Map.insert feederID newFeeder' (worldFeeders worldAccum)
  in
    worldAccum { worldField = newField, worldFeeders = newFeeders }

setBehaviorPersistence worldAccum feederID newBehaviorName seconds = let
    oldFeeder = fromJust $ Map.lookup feederID (worldFeeders worldAccum)
    oldBehaviorName = feederBehaviorName oldFeeder
    oldBehaviorPersistencePreference = feederBehaviorPersistencePreference oldFeeder
    behaviorPersistencePreferenceFalloffRate = 0.8
    newFeeder = fromJust $ Map.lookup feederID (worldFeeders worldAccum)
    feederWithNewBehaviorName = case (oldBehaviorName == newBehaviorName) of
        True  -> newFeeder { feederBehaviorPersistencePreference = oldBehaviorPersistencePreference * (behaviorPersistencePreferenceFalloffRate ** seconds) }
        False -> newFeeder { feederBehaviorName = newBehaviorName
                           , feederBehaviorPersistencePreference = 1.0
                           }
    newFeeders = Map.insert feederID feederWithNewBehaviorName (worldFeeders worldAccum)
  in
    worldAccum { worldFeeders = newFeeders }
