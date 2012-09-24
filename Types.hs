{-# LANGUAGE ImpredicativeTypes #-}

module Types where
import Graphics.Gloss
import Data.Map (Map)
import Geometry
import System.Random (StdGen)

data AppState = AppState World Point

data World = World { worldFeeders         :: Feeders
                   , worldNextFeederID    :: FeederID
                   , worldPredators       :: Predators
                   , worldNextPredatorID  :: PredatorID
                   , worldField           :: Field
                   , worldRandomGen       :: StdGen
                   , worldTargetedPlants  :: Map PlantID FeederID
                   , worldTargetedFeeders :: Map FeederID PredatorID
                   }

type ActorID = Int
type TimeInterval = Float
type Effect = World -> FeederID -> Feeder -> TimeInterval -> WorldChange
type Urgency = Float
type FeederDesire = World -> Feeder -> (Urgency, Behavior)
type PredatorDesire = World -> Predator -> (Urgency, Behavior)
data Behavior = Behavior BehaviorName [Action]
data BehaviorName = DoingNothing | Eating | Drinking | Killing deriving (Show, Eq)
type Action = World -> ActorID -> TimeInterval -> WorldChange -- previous world state, feeder acting
type WorldChange = World -> World

type Predators = Map PredatorID Predator
type PredatorID = ActorID
data Predator = Predator { predatorLocation       :: Point
                         , predatorTargetFeederID :: Maybe FeederID
                         } deriving (Show)

type Feeders = Map FeederID Feeder
type FeederID = ActorID
data Feeder = Feeder { feederLocation                       :: Point
                     , feederFood                           :: Float
                     , feederWater                          :: Float
                     , feederTargetPlantID                  :: Maybe PlantID
                     --, feederFleeingPredatorID              :: Maybe PredatorID
                     , feederBehaviorName                   :: BehaviorName
                     , feederBehaviorPersistencePreference  :: Float
                     } deriving (Show)

data Field = Field { fieldPlants          :: Plants
                   , fieldWidth           :: Int 
                   , fieldHeight          :: Int
                   , fieldNextPlantID     :: PlantID
                   , fieldNumberOfPlants  :: Int
                   }

type Plants = Map PlantID Plant
type PlantID = Int
data Plant = Plant PlantType Float Point 
    deriving (Show, Eq)

data PlantType = Food | Water 
    deriving (Show, Eq)
