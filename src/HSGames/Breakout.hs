module HSGames.Breakout (
    main
) where


import Data.Either(Either(..))
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

import HSGames.Ticker(initTicker, startTicker, endAllTickers)
import HSGames.Monad(UIData, fillRect, fillScreen, centerMessage, UIState, pixel, uiRun)

import Paths_HSGames

data GameState = InitState
               | RunState Paddle Ball [Block]

black = pixel 0 0 0

main = do
    SDL.init [SDL.InitEverything]
    TTF.init
    screen <- SDL.setVideoMode 800 600 32 []
    SDL.setCaption "Breakout!" "Breakout!"
    -- Turn key repeating off
    SDL.enableKeyRepeat 0 0
    fontpath <- getDataFileName "lacuna.ttf"
    font <- TTF.openFont fontpath 32
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
     in loop (screen, font, tm) $ InitState
    endAllTickers tm
    SDL.quit

type Pos = Double
type Coord = (Pos, Pos)
type Radius = Double
type Theta = Double
data Paddle = Paddle Pos
data Ball = Ball Coord Theta
data Block = Block Coord

ballradius :: Double
ballradius = 5
blockwidth :: Double
blockwidth = 50
blockheight :: Double
blockheight = 25

drawBlock :: Block -> UIState ()
drawBlock (Block (x, y)) = do
    fillRect (SDL.Rect (round x) (round y) (round blockwidth) (round blockheight)) $ pixel 0 0 128
    fillRect (SDL.Rect (round x+2) (round y+1) (round blockwidth-4) (round blockheight-2)) $ pixel 0 0 200

--drawBall :: Ball -> UIState ()
--drawBall (Ball (x, y) _) = do


startNew :: GameState
startNew = RunState (Paddle 400) (Ball (400,400) 1) initBlocks
    where
        initBlocks = [Block (x, y) | x <- [50,100..750], y <- [25,50..300]]

drawState :: GameState -> UIState ()
drawState (InitState) = do
    fillScreen black
    centerMessage "Press space to start"

handleEvent :: SDL.Event -> UIData -> GameState -> IO (Either String GameState)
handleEvent evt uid state = do
    case evt of
        SDL.Quit -> do
            return $ Left "Bye!"
        (SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _)) -> do
            return $ Left "Bye!"
        (SDL.KeyDown (SDL.Keysym SDL.SDLK_SPACE _ _)) -> do
            -- Start the game.
            return . Right $ startNew
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





