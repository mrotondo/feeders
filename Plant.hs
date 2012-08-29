module Plant where
import System.Random (RandomGen, random)
import Graphics.Gloss
import Types
import Geometry
import Data.List (any)

plantLocation (Plant plantType location) = location

plantSize :: Plant -> Float
plantSize (Plant (Food amount) location) = amount * 10
plantSize (Plant (Water amount) location) = amount * 10
plantSize _ = 0

plantColor :: Plant -> Color
plantColor (Plant (Food amount) location) = (makeColor 0 1 0 0.5)
plantColor (Plant (Water amount) location) = (makeColor 0.2 0.2 1 0.5)
plantColor _ = black

randomPlant :: RandomGen g => g -> (Int, Int) -> (Plant, g)
randomPlant gen (width, height) = let
    (xUnscaled, gen') = random gen
    (yUnscaled, gen'') = random gen'
    x = (fromIntegral width) * xUnscaled
    y = (fromIntegral height) * yUnscaled
    offsetX = - fromIntegral width / 2
    offsetY = - fromIntegral height / 2
    (randomAmount, gen''') = random gen''
    amount = 0.4 + 0.6 * randomAmount
    (randomType, gen'''') = random gen'''
    plantType = case randomType of
                  False -> Food amount
                  True  -> Water amount
    in (Plant plantType (x + offsetX, y + offsetY), gen'''')

isWater :: Maybe Plant -> Bool
isWater plant = case plant of
                  Just (Plant (Water amount) location) -> True
                  _                                    -> False

isFood :: Maybe Plant -> Bool
isFood plant = case plant of
                 Just (Plant (Food amount) location) -> True
                 _                                   -> False

isTargeted :: Feeders -> Maybe PlantID -> Bool
isTargeted feeders plantID = any (\feeder -> (feederTargetPlantID feeder) == plantID) feeders