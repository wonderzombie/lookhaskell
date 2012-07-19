module Room
( Exit(..)
, Room(..)
, WorldMap(..)
, makeLink
, showRoom
, showExits
) where

import qualified Data.Map as M
import qualified Data.List as L

type Description = String
type Direction   = String

-- A room has a description.
data Room = Room { getDescription :: Description
                 } deriving (Ord, Eq)

instance Show Room where
  show r = getDescription r

{--
A WorldMap maps all Rooms in the world to a list of their exits.
Eventually, maybe this should involve a couple of lookups, one which is a mapping of IDs to Rooms. Then an Exit is simply a RoomId or what have you. If a Room is going to have state eventually, it won't do to have multiple copies of them floating around, as we do due to makeLink's current implementation.
--}
type WorldMap = M.Map Room [Exit]

data Exit = Exit { getDirection :: Direction
                 , getDestination :: Room
                 }

instance Show Exit where
  show x = getDirection x

makeLink :: Room -> Room -> Direction -> Direction -> WorldMap
makeLink ra rb ab ba = M.fromList   [ (ra, [Exit ab rb])
                                    , (rb, [Exit ba ra])
                                    ]

showRoom :: WorldMap -> Room -> String
showRoom m (Room d) = join "\n" [d, exitDesc]
    where exits    = M.lookup (Room d) m
          exitDesc = case exits of Nothing   -> ""
                                   Just (xs) -> showExits xs ++ "\n"

showExits :: [Exit] -> String
showExits xs = "Exits: " ++ exits
    where exits = join "," $ map show xs

join :: [a] -> [[a]] -> [a]
join d xs = concat $ L.intersperse d xs

