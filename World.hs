module World where
import Feeder
import Field
import Geometry
import Graphics.Gloss

data World = World { worldField         :: Field
                   , worldFeeders       :: Feeders
                   , worldMouseLocation :: Point
                   }

initialWorld :: Field -> World
initialWorld field = World { worldField = field, worldFeeders = initialFeeders field, worldMouseLocation = (0, 0) }
