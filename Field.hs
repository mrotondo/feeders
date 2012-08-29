module Field where
import Types
import Plant
import System.Random (randomIO, getStdGen, mkStdGen)
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
    randomGenSeed <- randomIO :: IO Int
    let emptyField = Field { fieldPlants         = Map.empty
                           , fieldWidth          = width
                           , fieldHeight         = height
                           , fieldNextPlantID    = 1
                           , fieldNumberOfPlants = numPlants
                           , fieldRandomGen      = mkStdGen randomGenSeed
                           }
    return $ addRandomPlants numPlants emptyField

iterateField :: Field -> Field
iterateField field = let 
    oldPlants = fieldPlants field
    numberOfPlantsToCreate = (fieldNumberOfPlants field) - (Map.size oldPlants)
    in addRandomPlants numberOfPlantsToCreate field

addRandomPlants :: Int -> Field -> Field
addRandomPlants numPlants field = case numPlants of
    0 -> field
    _ -> addRandomPlants (numPlants - 1) newField
  where
    oldPlants = fieldPlants field
    plantID = fieldNextPlantID field
    (plant, newGen) = randomPlant (fieldRandomGen field) (fieldWidth field, fieldHeight field)
    newField = field { fieldPlants = Map.insert plantID plant oldPlants
                     , fieldNextPlantID = plantID + 1
                     , fieldRandomGen = newGen
                     }

fieldWithOnlyUntargetedPlants :: Feeders -> Field -> Field
fieldWithOnlyUntargetedPlants feeders field = field { fieldPlants = untargetedPlants }
  where
    untargetedPlants = Map.filterWithKey (\plantID plant -> not (isTargeted feeders (Just plantID))) (fieldPlants field)
