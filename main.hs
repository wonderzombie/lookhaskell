
module Main where

import Room
import qualified Data.Map as M

buildWorld :: WorldMap
buildWorld = makeLink startRoom endRoom "north" "south"

startRoom :: Room
startRoom = Room "You are in the starting room."

endRoom :: Room
endRoom = Room "You are in the ending room."

chooseRoom :: [Exit] -> Direction -> Maybe Room
chooseRoom m r d = undefined

{--
This part needs the most work. I want something like this:
* Do initial setup via buildWorld.
* Define a function which takes a Room as input. That function will, in the end,
call itself with a new room, representing moving from room to room.

The way to do that "right" is probably to use a State Monad, but the former seems like
a straightforward way to implement Room navigation roughly.
--}
main = forever $ do
    putStrLn $ showRoom world startRoom
    input <- getLine
    putStrLn ("You said " ++ input ++ ".")
   where
      world     = buildWorld
      exits     = M.lookup room world
      forever a = a >> forever a 
