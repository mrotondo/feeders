module Types where
import Graphics.Gloss
import Data.Map
import Geometry
import System.Random (StdGen)

data AppState = AppState World Point

data World = World Feeders Field

type TimeInterval = Float

type Feeders = [Feeder]
data Feeder = Feeder { feederLocation       :: Point
                     , feederFood           :: Float
                     , feederWater          :: Float
                     , feederTargetPlantID  :: PlantID
                     }

data Field = Field { fieldPlants            :: Map PlantID Plant
                   , fieldWidth             :: Int 
                   , fieldHeight            :: Int
                   , fieldNextPlantID       :: PlantID
                   , fieldNumberOfPlants    :: Int
                   , fieldRandomGen         :: StdGen 
                   }

type PlantID = Int
data Plant = Plant PlantType Point 
    deriving (Show, Eq)

data PlantType = Food Float
               | Water Float
               | Dead
    deriving (Show, Eq)
