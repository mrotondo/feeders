module World where
import Feeder
import Field
import Geometry
import Graphics.Gloss

data World = World { worldField         :: Field
                   , worldFeeders       :: Feeders
                   , worldMouseLocation :: Point
                   }

initialWorld :: Field -> Feeders -> World
initialWorld field feeders = World { worldField = field, worldFeeders = feeders, worldMouseLocation = (0, 0) }
