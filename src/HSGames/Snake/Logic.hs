module HSGames.Snake.Logic (
    gridx,
    gridy,
    wrap,
    newApple,
    startNew,
    step,
    addDirLog
) where

import HSGames.Snake.Types(Coord, Snake, Randoms, GameState(..), SnakeData)
import HSGames.Snake.Direction(Direction(..), negdir, dirstep)

gridx = 80
gridy = 60

-- Wrap around the grid.
wrap :: Coord -> Coord
wrap (x,y) = (x`mod`gridx,y`mod`gridy)

-- Return a random coordinate that is not on top of the snake.
newApple :: Snake -> Randoms -> (Coord, Randoms)
newApple snake rnd
    | elem na snake = newApple snake rnd'
    | otherwise     = (na, rnd')
    where
        na = ((rnd!!0)`mod`gridx, (rnd!!1)`mod`gridy)
        rnd' = (drop 2 rnd)

-- Start up a new game.
startNew :: Randoms -> GameState
startNew rnd = RunState rnd'' ([snake], 3, NORTH, []) apple
    where
        (snake, rnd') = newApple [] rnd
        (apple, rnd'') = newApple [snake] rnd

-- Update the game state by one step.
-- This takes a RunState, and returns one as well.
step :: GameState -> GameState
step state@(RunState rnd_ (snake_, snakelen_, dir_, dirlog_) apple_)
    = stepCollision . stepApple (rnd_, apple_) . stepSnake (snake_, snakelen_) . stepDir $ (dir_, dirlog_)
    where
        -- Update direction
        stepDir (d, []) = (d, [])
        stepDir (d, d2:dr)
            | negdir d == d2 = stepDir (d, dr)
            | d == d2        = stepDir (d, dr)
            | otherwise     = (d2, dr)
        -- Walk the snake
        stepSnake (snake, snakelen) (dir, dirlog) = (snake', snakelen, dir, dirlog)
            where snake' = take snakelen . (:snake) . wrap . dirstep dir . head $ snake
        -- Check if we ate an apple
        stepApple (rnd, apple) sd@(snake, snakelen, dir, dirlog)
            | head snake == apple = RunState rnd' (snake, (snakelen+2), dir, dirlog) apple'
            | otherwise          = RunState rnd sd apple
            where (apple', rnd') = newApple snake rnd
        -- Check if we collided with ourselves
        stepCollision state@(RunState _ sd@(snake, _, _, _) _)
            | elem (head snake) (tail snake) = DeadState state
            | otherwise                      = state

-- Add a direction to the direction log.
-- Every frame, we throw away invalid directions (the direction
-- we're already going and it's inverse), and then take a single
-- direction from the log and make it our new, real direction.
addDirLog :: SnakeData -> Direction -> SnakeData
addDirLog (snake, snakelen, dir, dirlog) ndir = (snake, snakelen, dir, dirlog ++ [ndir])

