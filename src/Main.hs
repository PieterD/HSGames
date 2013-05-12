module Main (
    main
) where

import qualified HSGames.Snake as Snake
import qualified HSGames.Breakout as Breakout
import System.Environment(getArgs)

main = getArgs >>= run

run ["Snake"] = Snake.main
run ["Breakout"] = Breakout.main
run _ = putStrLn "Available games:" >> putStrLn "  Snake" >> putStrLn "  Breakout"
