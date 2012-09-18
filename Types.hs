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
                   , worldTargetedPlants     :: Map PlantID FeederID
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

type Effect = World -> FeederID -> Feeder -> TimeInterval -> WorldChange
type Urgency = Float
type DesireArgs = (World, Feeder) -- (packed for passing via (map $)) previous world state, feeder being operated on
type Desire = DesireArgs -> (Urgency, Behavior)
data Behavior = Behavior BehaviorName [Action]
data BehaviorName = DoingNothing | Eating | Drinking deriving (Show, Eq)
type Action = World -> FeederID -> TimeInterval -> WorldChange -- previous world state, feeder acting
type WorldChange = World -> World

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
