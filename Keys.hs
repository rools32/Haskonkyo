module Keys (mapKeys) where

import qualified Data.Map.Lazy as Map
import Types(KeyBind(..))

mapKeys keys actionMap = Map.fromList $ map f keys
  where f (KeyBind k a) = (k, Map.lookup a actionMap)

