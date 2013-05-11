-----------------------------------------------------------------------------
--
-- Module      :  HSGames.Snake.Types
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module HSGames.Snake.Types (
    Coord,
    Apple,
    Snake,
    Length,
    Randoms,
    DirLog,
    SnakeData,
    GameState(..)
) where

import HSGames.Snake.Direction(Direction)

type Coord = (Int,Int)
type Apple = Coord
type Snake = [Coord]
type Length = Int
type Randoms = [Int]
type DirLog = [Direction]
type SnakeData = (Snake, Length, Direction, DirLog)

-- Both pause and dead states inherit the running state.
data GameState = InitState  Randoms
               | RunState   Randoms SnakeData Apple
               | PauseState GameState
               | DeadState  GameState



