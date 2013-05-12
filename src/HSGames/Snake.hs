module HSGames.Snake (
    main
) where

import Data.Either(Either(..))
import Data.Maybe(Maybe)
import Control.Monad(forever, when)
import Control.Concurrent(forkIO, threadDelay, killThread)
import System.Random(getStdGen,randoms)
import GHC.Ptr(nullPtr)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

import HSGames.Monad(uiRun, UIData, UIState, pixel, centerMessage, fillScreen, fillRect)
import HSGames.Ticker(initTicker, startTicker, endAllTickers, TickerMaster)
import HSGames.Snake.Direction(Direction(..), isdirkey, negdir, dirstep, dirfromkey)
import HSGames.Snake.Logic(step, startNew, gridx, gridy, addDirLog)
import HSGames.Snake.Types(Coord, Apple, Snake, Length, Randoms, DirLog, SnakeData, GameState(..))

import Paths_HSGames

blue = pixel 128 128 255
green = pixel 0 255 0
white = pixel 255 255 255
black = pixel 0 0 0

main = do
    SDL.init [SDL.InitEverything]
    TTF.init
    screen <- SDL.setVideoMode (gridx*10) (gridy*10) 32 []
    SDL.setCaption "Snaaake!" "Snaaake!"
    -- Turn key repeating off
    SDL.enableKeyRepeat 0 0
    fontpath <- getDataFileName "lacuna.ttf"
    font <- TTF.openFont fontpath 32
    -- rnd will be an infinite list of random Ints
    rndgen <- getStdGen
    let rnd = randoms rndgen
    -- Set up 24 fps main ticker
    tm <- initTicker
    startTicker tm 0 (1000000`div`24)
    -- Main event loop
    let loop uid state = do
        evt <- SDL.waitEvent
        -- handleEvent returns either a string, which means we quit, or a new state.
        rv <- handleEvent evt uid state
        case rv of
            Left err -> do
                print err
                return ()
            Right state' -> do
                loop uid state'
    -- Start loop with the initial state.
     in loop (screen, font, tm) $ InitState rnd
    endAllTickers tm
    SDL.quit

-- Draw the screen for every state. This needs to be called from uiRun.
drawState :: GameState -> UIState ()
drawState (InitState _) = do
    fillScreen black
    centerMessage "Press space to start"
drawState (RunState _ (snake, snakelen, dir, dirlog) apple) = do
    fillScreen black
    sequence . map (put white) . tail $ snake
    put green apple
    put blue . head $ snake
    where put color (x,y) = fillRect (SDL.Rect (x*10) (y*10) 10 10) color
drawState (PauseState gstate) = do
    drawState gstate
    centerMessage "Game paused. Press space to continue"
drawState (DeadState gstate) = do
    drawState gstate
    centerMessage "You died! Press space to try again"

-- Handle events for every state.
-- These return a new state, or a goodbye message.
handleEvent :: SDL.Event -> UIData -> GameState -> IO (Either String GameState)
handleEvent evt uid state@(InitState rnd) = do
    case evt of
        SDL.Quit -> do
            return $ Left "Bye!"
        (SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _)) -> do
            return $ Left "Bye!"
        (SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _)) -> do
            -- Start the game.
            return . Right $ startNew rnd
        (SDL.User SDL.UID0 0 _ _) -> do
            -- Frame timer event
            draw
            return $ Right state
        SDL.VideoExpose -> do
            draw
            return $ Right state
        _ -> return $ Right state
    where
        draw = uiRun uid $ drawState state
handleEvent evt uid state@(RunState rnd sd@(snake, snakelen, dir, dirlog) apple) = do
    case evt of
        SDL.Quit -> do
            return $ Left "Bye!"
        (SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _)) -> do
            return $ Left "Bye!"
        (SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _)) -> do
            return $ Right $ PauseState state
        (SDL.KeyDown (SDL.Keysym k _ _)) -> do
            -- Key pressed (though not escape or space)
            let state' = if isdirkey k
                         then RunState rnd (addDirLog sd . dirfromkey $ k) apple
                         else state
            return $ Right state'
        (SDL.User SDL.UID0 0 _ _) -> do
            -- Frame timer event. We're running the game,
            -- so that means updating the game state.
            let state' = step state
            uiRun uid $ drawState state'
            return $ Right state'
        SDL.VideoExpose -> do
            draw
            return $ Right state
        _ -> return $ Right state
    where
        draw = uiRun uid $ drawState state
handleEvent evt uid state@(PauseState gstate) = do
    case evt of
        SDL.Quit -> do
            return $ Left "Bye!"
        (SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _)) -> do
            return $ Left "Bye!"
        (SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _)) -> do
            -- Continue playing by returning the saved game state.
            return $ Right gstate
        (SDL.User SDL.UID0 0 _ _) -> do
            draw
            return $ Right state
        SDL.VideoExpose -> do
            draw
            return $ Right state
        _ -> return $ Right state
    where
        draw = uiRun uid $ drawState state
handleEvent evt uid state@(DeadState gstate@(RunState rnd _ _)) = do
    case evt of
        SDL.Quit -> do
            return $ Left "Bye!"
        (SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _)) -> do
            return $ Left "Bye!"
        (SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _)) -> do
            -- Start a new game, like in InitState.
            return $ Right $ startNew rnd
        (SDL.User SDL.UID0 0 _ _) -> do
            draw
            return $ Right state
        SDL.VideoExpose -> do
            draw
            return $ Right state
        _ -> return $ Right state
    where
        draw = uiRun uid $ drawState state


