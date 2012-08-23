module Feeder where
import Geometry

type Feeders = [Feeder]
data Feeder = Feeder { feederLocation :: Location, feederFood :: Float, feederWater :: Float }
type Effect = Float -> Feeder -> Feeder

newFeeder :: Location -> Feeder
newFeeder loc = Feeder { feederLocation = loc, feederFood = 1.0, feederWater = 1.0 }

moveTowardsLocation targetLocation seconds feeder = feeder { feederLocation=oldLocation `moveBy` movement }
  where
    oldLocation = feederLocation feeder
    speedPerSecond = 0.8
    movement = lerp oldLocation targetLocation (speedPerSecond * seconds)

moveBy :: Location -> Movement -> Location
moveBy = add

iterateFeeder :: Float -> Feeder -> Feeder
iterateFeeder seconds feeder = foldl (\feederAccum effect -> effect seconds feederAccum) feeder lifeEffects

lifeEffects :: [Effect]
lifeEffects = [getHungrier, getThirstier]

getHungrier :: Effect
getHungrier seconds feeder = feeder { feederFood = newFeederFood }
  where
    oldFeederFood = (feederFood feeder)
    hungerPerSecond = 0.1
    newHunger = max 0.0 (hungerPerSecond * seconds)
    newFeederFood = oldFeederFood - newHunger

getThirstier :: Effect
getThirstier seconds feeder = feeder { feederWater = newFeederWater }
  where
    oldFeederWater = (feederWater feeder)
    thirstPerSecond = 0.1
    newThirst = max 0.0 (thirstPerSecond * seconds)
    newFeederWater = oldFeederWater - newThirst