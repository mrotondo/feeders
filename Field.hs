module Field where
import Types
import Plant
import System.Random (randomIO)
import Control.Monad (replicateM)
import Data.Map (Map)
import qualified Data.Map as Map

plants :: Field -> [Plant]
plants = Map.elems . fieldPlants

randomField :: (Int, Int) -> IO Field
randomField (width, height) = do
    randomPlantPercent <- randomIO :: IO Float
    let plantPercent = 0.3 + 0.7 * randomPlantPercent
    let maxNumPlants = 0.003 * (fromIntegral width) * (fromIntegral height)
    let numPlants = floor $ plantPercent * maxNumPlants
    plants <- replicateM numPlants (randomPlant (width, height))
    let plantsWithKeys = zip [1..] plants
    return $ Field
        { fieldPlants   = Map.fromList plantsWithKeys
        , fieldWidth    = width
        , fieldHeight   = height
        , nextPlantID   = length plants + 1
        }