module Plant where
import System.Random (randomIO)
import Graphics.Gloss
import Geometry

type PlantID = Int
data Plant = Plant Type Point 
    deriving (Show, Eq)

data Type = Food Float
    deriving (Show, Eq)

plantLocation (Plant plantType location) = location

randomPlant :: (Int, Int) -> IO Plant
randomPlant (fieldWidth, fieldHeight) = do
    xUnscaled <- randomIO :: IO Float
    yUnscaled <- randomIO :: IO Float
    let x = (fromIntegral fieldWidth) * xUnscaled
    let y = (fromIntegral fieldHeight) * yUnscaled
    let offsetX = - fromIntegral fieldWidth / 2
    let offsetY = - fromIntegral fieldHeight / 2
    randomFoodAmount <- randomIO :: IO Float
    let foodAmount = 0.4 + 0.6 * randomFoodAmount
    return $ Plant (Food foodAmount) (x + offsetX, y + offsetY)