module Field where
import Plant
import System.Random (randomIO)
import Control.Monad (replicateM)

data Field
    = Field
    { fieldPlants       :: [Plant]
    , fieldWidth        :: Int 
    , fieldHeight       :: Int
    }

randomField :: (Int, Int) -> IO Field
randomField (width, height) = do
    plantPercent <- randomIO :: IO Float
    let maxNumPlants = 0.001 * (fromIntegral width) * (fromIntegral height)
    let numPlants = floor $ plantPercent * maxNumPlants
    putStrLn $ "This many plants: " ++ (show numPlants)
    plants <- replicateM numPlants (randomPlant (width, height))
    return $ Field
        { fieldPlants       = plants
        , fieldWidth        = width
        , fieldHeight       = height
        }