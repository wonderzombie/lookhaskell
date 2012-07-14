import qualified Data.Map as Map

type Description = String
type Direction   = String

-- A room has a description and an ExitMap.
data Room = Room { description :: Description
                 } deriving (Show, Ord, Eq)

-- An ExitMap 
type WorldMap = Map.Map Room [Exit]

data Exit = Exit { direction :: String
                 , destination :: Room
                 } deriving (Show)

makeLink :: Room -> Room -> Direction -> Direction -> WorldMap
makeLink ra rb ab ba = Map.fromList [ (ra, [Exit ab rb])
                                    , (rb, [Exit ba ra])
                                    ]

startRoom :: Room
startRoom = Room "You are in the starting room."

endRoom :: Room
endRoom = Room "You are in the ending room."

showRoom :: WorldMap -> Room -> String
showRoom m (Room d) = d ++ exitDesc
    where exits    = Map.lookup (Room d) m
          exitDesc = showExits exits

showExits :: Maybe [Exit] -> String
showExits Nothing   = ""
showExits (Just xs) = undefined


          