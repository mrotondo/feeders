module Plant where
import System.Random (RandomGen, random)
import Graphics.Gloss
import Types
import Geometry

plantLocation (Plant plantType location) = location

randomPlant :: RandomGen g => g -> (Int, Int) -> (Plant, g)
randomPlant gen (width, height) = let
    (xUnscaled, gen') = random gen
    (yUnscaled, gen'') = random gen'
    x = (fromIntegral width) * xUnscaled
    y = (fromIntegral height) * yUnscaled
    offsetX = - fromIntegral width / 2
    offsetY = - fromIntegral height / 2
    (randomFoodAmount, gen''') = random gen''
    foodAmount = 0.4 + 0.6 * randomFoodAmount
    in (Plant (Food foodAmount) (x + offsetX, y + offsetY), gen''')