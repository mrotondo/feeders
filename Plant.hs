module Plant where
import System.Random (randomIO)
import Graphics.Gloss
import Geometry

data Plant = Plant Type Location 
    deriving (Show, Eq)

data Type = Food Float
    deriving (Show, Eq)

randomPlant :: (Int, Int) -> IO Plant
randomPlant (fieldWidth, fieldHeight) = do
    xUnscaled <- randomIO :: IO Float
    yUnscaled <- randomIO :: IO Float
    let x = (fromIntegral fieldWidth) * xUnscaled
    let y = (fromIntegral fieldHeight) * yUnscaled
    foodAmount <- randomIO :: IO Float
    return $ Plant (Food foodAmount) (x, y)