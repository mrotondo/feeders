module Field where
import Types
import Plant
import System.Random (RandomGen, random)
import Control.Monad (replicateM)
import Data.Map (Map)
import qualified Data.Map as Map

plants :: Field -> [Plant]
plants = Map.elems . fieldPlants

randomField :: RandomGen g => (Int, Int) -> g -> (Field, g)
randomField (width, height) randomGen = let
    (randomPlantPercent, randomGen') = (random randomGen)
    plantPercent = 0.3 + 0.7 * (randomPlantPercent :: Float)
    maxNumPlants = 0.003 * (fromIntegral width) * (fromIntegral height)
    numPlants = floor $ plantPercent * maxNumPlants
    emptyField = Field { fieldPlants         = Map.empty
                       , fieldWidth          = width
                       , fieldHeight         = height
                       , fieldNextPlantID    = 1
                       , fieldNumberOfPlants = numPlants
                       }
    in addRandomPlants numPlants emptyField randomGen'

iterateField :: RandomGen g => Field -> g -> (Field, g)
iterateField field randomGen = let 
    oldPlants = fieldPlants field
    stillAlivePlants = Map.filter (\(Plant plantType amount location) -> amount > 0) oldPlants
    numberOfPlantsToCreate = (fieldNumberOfPlants field) - (Map.size stillAlivePlants)
    in addRandomPlants numberOfPlantsToCreate (field { fieldPlants = stillAlivePlants}) randomGen

addRandomPlants :: RandomGen g => Int -> Field -> g -> (Field, g)
addRandomPlants numPlants field randomGen = case numPlants of
    0 -> (field, randomGen)
    _ -> addRandomPlants (numPlants - 1) newField newGen 
  where
    oldPlants = fieldPlants field
    plantID = fieldNextPlantID field
    (plant, newGen) = randomPlant randomGen (fieldWidth field, fieldHeight field)
    newField = field { fieldPlants = Map.insert plantID plant oldPlants
                     , fieldNextPlantID = plantID + 1
                     }

fieldWithOnlyUntargetedPlants :: Feeders -> Field -> Field
fieldWithOnlyUntargetedPlants feeders field = field { fieldPlants = untargetedPlants }
  where
    untargetedPlants = Map.filterWithKey (\plantID plant -> not (isTargeted feeders (Just plantID))) (fieldPlants field)
