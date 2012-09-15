module Types where
import Graphics.Gloss
import Data.Map
import Geometry
import System.Random (StdGen)

data AppState = AppState World Point

data World = World { worldFeeders       :: Feeders
                   , worldNextFeederID  :: FeederID
                   , worldField         :: Field
                   , worldRandomGen     :: StdGen
                   }

type TimeInterval = Float

type Feeders = Map FeederID Feeder
type FeederID = Int
data Feeder = Feeder { feederLocation                       :: Point
                     , feederFood                           :: Float
                     , feederWater                          :: Float
                     , feederTargetPlantID                  :: Maybe PlantID
                     , feederBehaviorName                   :: BehaviorName
                     , feederBehaviorPersistencePreference  :: Float
                     }

type Effect = World -> Feeder -> TimeInterval -> Feeder
type Urgency = Float
type DesireArgs = (World, Feeder) -- (packed for passing via (map $)) previous world state, feeder being operated on
type Desire = DesireArgs -> (Urgency, Behavior)
data Behavior = Behavior BehaviorName [Action]
data BehaviorName = DoingNothing | Eating | Drinking deriving (Eq)
type Action = World -> Feeders -> Field -> Feeder -> TimeInterval -> (Feeder, Field) -- previous world state, current field (as modified by tick thus far), feeder being operated on

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
