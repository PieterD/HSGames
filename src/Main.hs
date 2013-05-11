module Main (
    main
) where

import qualified HSGames.Snake as Snake
import System.Environment(getArgs)

main = getArgs >>= run

run ["Snake"] = Snake.main
run _ = putStrLn "Available games:" >> putStrLn "  Snake"
