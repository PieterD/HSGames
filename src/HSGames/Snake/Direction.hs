module HSGames.Snake.Direction (
    Direction(..),
    negdir,
    dirchange,
    isdirkey,
    dirfromkey,
    dirstep
) where

import qualified Graphics.UI.SDL as SDL

data Direction = NORTH | EAST | SOUTH | WEST deriving (Eq, Show)

negdir NORTH = SOUTH
negdir EAST = WEST
negdir SOUTH = NORTH
negdir WEST = EAST

dirchange od nd | negdir od == nd = od
                | otherwise = nd

isdirkey SDL.SDLK_UP = True
isdirkey SDL.SDLK_RIGHT = True
isdirkey SDL.SDLK_DOWN = True
isdirkey SDL.SDLK_LEFT = True
isdirkey _ = False

dirfromkey SDL.SDLK_UP = NORTH
dirfromkey SDL.SDLK_RIGHT = EAST
dirfromkey SDL.SDLK_DOWN = SOUTH
dirfromkey SDL.SDLK_LEFT = WEST

dirstep NORTH (x,y) = (x,y-1)
dirstep EAST  (x,y) = (x+1,y)
dirstep SOUTH (x,y) = (x,y+1)
dirstep WEST  (x,y) = (x-1,y)
