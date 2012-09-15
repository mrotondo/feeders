module Plant where
import System.Random (RandomGen, random)
import Graphics.Gloss
import Types
import Geometry
import Data.List (any)
import Data.Map (elems)

plantLocation (Plant _ _ location) = location

plantSize :: Plant -> Float
plantSize (Plant Food amount location) = amount * 10
plantSize (Plant Water amount location) = amount * 10

plantColor :: Plant -> Color
plantColor (Plant Food amount location) = (makeColor 0 1 0 0.5)
plantColor (Plant Water amount location) = (makeColor 0.2 0.2 1 0.5)

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
                  False -> Food
                  True  -> Water
    in (Plant plantType amount (x + offsetX, y + offsetY), gen'''')

isWater :: Maybe Plant -> Bool
isWater plant = case plant of
                  Just (Plant Water _ _) -> True
                  _                      -> False

isFood :: Maybe Plant -> Bool
isFood plant = case plant of
                 Just (Plant Food _ _) -> True
                 _                     -> False

isTargeted :: Feeders -> Maybe PlantID -> Bool
-- Change this to use a reverse index from PlantIDs to FeederIDs
isTargeted feeders plantID = any (\feeder -> (feederTargetPlantID feeder) == plantID) (elems feeders)