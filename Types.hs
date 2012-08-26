module Types where
import Graphics.Gloss
import Data.Map

data AppState = AppState World Point

data World = World Feeders Field

type Feeders = [Feeder]
data Feeder = Feeder { feederLocation       :: Point
                     , feederFood           :: Float
                     , feederWater          :: Float
                     , feederTargetPlantID  :: PlantID
                     }

data Field = Field { fieldPlants    :: Map PlantID Plant
                   , fieldWidth     :: Int 
                   , fieldHeight    :: Int
                   , nextPlantID    :: PlantID
                   }

type PlantID = Int
data Plant = Plant PlantType Point 
    deriving (Show, Eq)

data PlantType = Food Float
    deriving (Show, Eq)
