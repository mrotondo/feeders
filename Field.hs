module Field where
import Types
import Plant
import System.Random (RandomGen, random)
import Control.Monad (replicateM)
import Data.Map (Map)
import qualified Data.Map as Map

plants :: Field -> [Plant]
plants = Map.elems . fieldPlants

contains :: Field -> PlantID -> Bool
field `contains` plantID = Map.member plantID (fieldPlants field)

emptyField :: (Int, Int) -> Field
emptyField (width, height) = Field { fieldPlants         = Map.empty
                                   , fieldWidth          = width
                                   , fieldHeight         = height
                                   , fieldNextPlantID    = 1
                                   , fieldNumberOfPlants = 0
                                   }

addRandomField :: World -> World
addRandomField world = let
    (field, randomGen') = populateField (worldField world) (worldRandomGen world)
  in
    world { worldField = field, worldRandomGen = randomGen' }

populateField :: RandomGen g => Field -> g -> (Field, g)
populateField field randomGen = let
    (numPlants, randomGen') = randomNumPlants (fieldWidth field, fieldHeight field) randomGen
    field' = field { fieldNumberOfPlants = numPlants }
  in 
    iterateField field' randomGen'

randomNumPlants :: RandomGen g => (Int, Int) -> g -> (Int, g)
randomNumPlants (width, height) randomGen = let
    (randomPlantPercent, randomGen') = (random randomGen)
    plantPercent = 0.3 + 0.7 * (randomPlantPercent :: Float)
    maxNumPlants = 0.006 * (fromIntegral width) * (fromIntegral height)
    numPlants = floor $ plantPercent * maxNumPlants
  in
    (numPlants, randomGen')

iterateField :: RandomGen g => Field -> g -> (Field, g)
iterateField field randomGen = let 
    stillAlivePlants = Map.filter (\(Plant plantType amount location) -> amount > 0.0001) (fieldPlants field)
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

untargetedPlants :: World -> Plants
untargetedPlants world = Map.filterWithKey (\plantID _ -> not (isTargeted world plantID)) plants
  where
    plants = (fieldPlants (worldField world))
