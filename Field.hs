module Field where
import Plant
import System.Random (randomIO)
import Control.Monad (replicateM)
import Data.Map (Map)
import qualified Data.Map as Map

data Field = Field { fieldPlants    :: Map PlantID Plant
                   , fieldWidth     :: Int 
                   , fieldHeight    :: Int
                   , nextPlantID    :: PlantID
                   }

plants :: Field -> [Plant]
plants = Map.elems . fieldPlants

randomField :: (Int, Int) -> IO Field
randomField (width, height) = do
    randomPlantPercent <- randomIO :: IO Float
    let plantPercent = 0.3 + 0.7 * randomPlantPercent
    let maxNumPlants = 0.001 * (fromIntegral width) * (fromIntegral height)
    let numPlants = floor $ plantPercent * maxNumPlants
    putStrLn $ "This many plants: " ++ (show numPlants)
    plants <- replicateM numPlants (randomPlant (width, height))
    let plantsWithKeys = zip [1..] plants
    return $ Field
        { fieldPlants   = Map.fromList plantsWithKeys
        , fieldWidth    = width
        , fieldHeight   = height
        , nextPlantID   = length plants + 1
        }