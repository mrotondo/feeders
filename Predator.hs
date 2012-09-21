module Predator where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Types
import Geometry
import System.Random (random)
import qualified Data.Map as Map
import Data.List (minimumBy, sortBy)
import Data.Function (on)
import Data.Maybe (fromJust, isJust)

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

changesFromPredator :: World -> PredatorID -> Predator -> TimeInterval -> [WorldChange]
changesFromPredator previousWorld predatorID predator seconds = let
    urgencyBehaviorTuples = generatePossibleBehaviors previousWorld predator
    mostUrgentBehavior = snd . head $ sortBy (compare `on` (((-) 1.0) . fst)) $ urgencyBehaviorTuples
    behaviorChanges = changesFromBehavior previousWorld predatorID seconds mostUrgentBehavior
    --lifeEffectChanges = changesFromLifeEffects previousWorld predatorID seconds lifeEffects
  in 
    behaviorChanges -- ++ lifeEffectChanges -- We calculate urgency based on previous feeder state, so apply effects after actions

--changesFromLifeEffects :: World -> PredatorID -> TimeInterval -> [Effect] -> [WorldChange]
--changesFromLifeEffects previousWorld predatorID seconds effects = 
--    map (\effect -> effect previousWorld predatorID seconds) effects

--lifeEffects = []

generatePossibleBehaviors :: World -> Predator -> [(Urgency, Behavior)]
generatePossibleBehaviors previousWorld predator = map (\desire -> desire previousWorld predator) desires

changesFromBehavior ::  World -> PredatorID -> TimeInterval -> Behavior -> [WorldChange]
changesFromBehavior previousWorld predatorID seconds (Behavior behaviorName actions) = map (\action -> action previousWorld predatorID seconds) actions

desires :: [PredatorDesire]
desires = [aggression]

aggression :: PredatorDesire
aggression previousWorld predator = (aggressionUrgency predator, Behavior Killing [targetFeeder, moveTowardsTarget, kill])
--aggression previousWorld predator = (aggressionUrgency predator, Behavior Killing [targetFeeder, moveTowardsTarget, kill])

aggressionUrgency :: Predator -> Urgency
aggressionUrgency predator = 1.0

targetFeeder :: Action
targetFeeder previousWorld predatorID seconds = (\worldAccum -> let
    predator = fromJust $ Map.lookup predatorID (worldPredators worldAccum)
    oldTargetFeederID = (predatorTargetFeederID predator)
    oldTargetFeederStillExists = Map.member (fromJust oldTargetFeederID) (worldFeeders worldAccum)
    newTargetFeederID = if ((isJust oldTargetFeederID) && oldTargetFeederStillExists)
                        then oldTargetFeederID
                        else Just $ closestFeederID (worldFeeders worldAccum) (predatorLocation predator)
  in
    setPredatorTarget worldAccum predatorID predator newTargetFeederID
  )

closestFeederID :: Feeders -> Point -> FeederID
closestFeederID feederMap predatorLoc = fst $ minimumBy compareFeederDistances (Map.toList feederMap)
  where
    compareFeederDistances (feederIDA, feederA) (feederIDB, feederB) = compare (distanceToFeeder feederA) (distanceToFeeder feederB)
    distanceToFeeder feeder = distance predatorLoc (feederLocation feeder)

setPredatorTarget :: World -> PredatorID -> Predator -> Maybe FeederID -> World
setPredatorTarget worldAccum predatorID predator newTargetFeederID = let
    newPredator = predator { predatorTargetFeederID = newTargetFeederID }
    newPredators = Map.insert predatorID newPredator (worldPredators worldAccum)
  in
    worldAccum { worldPredators = newPredators }

moveTowardsTarget :: Action
moveTowardsTarget previousWorld predatorID seconds = (\worldAccum -> let
    predator = fromJust $ Map.lookup predatorID (worldPredators worldAccum)
    movement = movementTowardsLocation (targetFeederLocation (worldFeeders worldAccum) predator) seconds predator
    newPredator = predator { predatorLocation = (predatorLocation predator) `add` movement }
    newPredators = Map.insert predatorID newPredator (worldPredators worldAccum)
  in
    worldAccum { worldPredators = newPredators }
  )

targetFeederLocation :: Feeders -> Predator -> Maybe Point
targetFeederLocation feeders predator  = do
    targetFeederID <- (predatorTargetFeederID predator)
    targetFeeder <- Map.lookup targetFeederID feeders
    return $ feederLocation targetFeeder

movementTowardsLocation :: Maybe Point -> TimeInterval -> Predator -> Vector
movementTowardsLocation targetLocation seconds predator = let
    speedPerSecond = 5.3
    direction = case targetLocation of
                     Just location -> normaliseV $ difference location (predatorLocation predator)
                     Nothing       -> (0, 0)
  in
    mulSV (speedPerSecond * seconds) direction

kill :: Action
kill previousWorld predatorID seconds = (\worldAccum -> let
    predator = fromJust $ Map.lookup predatorID (worldPredators worldAccum)
    targetFeederID = predatorTargetFeederID predator
    targetFeeder = case targetFeederID of
                        Just feederID -> Map.lookup feederID (worldFeeders worldAccum)
                        Nothing       -> Nothing
  in
    case targetFeeder of
        Nothing     -> worldAccum
        Just feeder -> destroy worldAccum predatorID predator (fromJust targetFeederID) feeder
  )

destroy :: World -> PredatorID -> Predator -> FeederID -> Feeder -> World
destroy worldAccum predatorID predator feederID feeder = let
    dist = distance (predatorLocation predator) (feederLocation feeder)
    newWorld = if dist < 20
      then let
        newFeeders = Map.delete feederID (worldFeeders worldAccum)
        newTargetedPlants = case feederTargetPlantID feeder of
            Just targetPlantID -> Map.delete targetPlantID (worldTargetedPlants worldAccum) 
            Nothing            -> (worldTargetedPlants worldAccum)
        newPredator = predator { predatorTargetFeederID = Nothing }
        newPredators = Map.insert predatorID newPredator (worldPredators worldAccum)
        in worldAccum { worldPredators = newPredators, worldFeeders = newFeeders, worldTargetedPlants = newTargetedPlants }
      else worldAccum
  in
    newWorld