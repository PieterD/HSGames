module HSGames.Snake.Direction (
    Direction(..),
    dirchange,
    isdirkey,
    dirfromkey
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
