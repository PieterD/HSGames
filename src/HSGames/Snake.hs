module HSGames.Snake (
    main
) where

import System.Random(getStdGen,randoms)
--import System.Random(randomIO)
import Data.Function(fix)
import Control.Monad(forever, when, mapM)
import Control.Concurrent(forkIO, threadDelay, killThread)
import Data.IORef(IORef, newIORef, modifyIORef, readIORef, writeIORef)
import Control.Concurrent.STM(readTVar, writeTVar, newTVarIO, readTVarIO, atomically, retry, STM)
import Control.Concurrent.STM.TVar(TVar)
import GHC.Ptr(nullPtr)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

import HSGames.Snake.Direction

main = do
    SDL.init [SDL.InitEverything]
    TTF.init

    (gd, tickers) <- gameinit
    tickerThreadIDs <- mapM (forkIO . tickerthread) tickers
    gameThreadID <- forkIO $ gamethread gd

    fix $ \loop -> do
        e <- SDL.waitEvent
        print e
        case e of
            SDL.Quit -> do
                return ()
            SDL.User SDL.UID1 0 _ _ -> do
                draw <- readTVarIO $ gddrawchan gd
                draw
                loop
            _ -> do
                atomically $ sendevent gd e
                loop
    mapM killThread tickerThreadIDs
    --TTF.quit
    SDL.quit
tickerthread (micros,index) = forever $ do
    SDL.pushEvent $ SDL.User SDL.UID0 index nullPtr nullPtr
    print "tick"
    threadDelay micros


-- Set static game data, mutable refs, and initial game state
data GameData = GameData {
    gdfont :: TTF.Font,
    gdsurface :: SDL.Surface,
    gdstateref :: IORef GameState,
    gdeventchan :: TVar [SDL.Event],
    gddrawchan :: TVar (IO ())
}
gameinit = do
    screen <- SDL.setVideoMode 800 600 32 []
    SDL.setCaption "Snaaake!" "Snaaake!"
    SDL.enableKeyRepeat 0 0
    font <- TTF.openFont "/home/pieter/lacuna.ttf" 32
    -- Initial game state
    rndgen <- getStdGen
    let rnd = randoms rndgen
    gsref <- newIORef $ initstate rnd
    eventchan <- newTVarIO []
    drawchan <- newTVarIO (print "hello")
    let gd = GameData font screen gsref eventchan drawchan
    let tickers = [(16666,0),(100000,1)]
    return (gd,tickers)
sendevent :: GameData -> SDL.Event -> STM ()
sendevent gd e = do
    l <- readTVar (gdeventchan gd)
    writeTVar (gdeventchan gd) (e:l)
    return ()
getevents :: GameData -> STM [SDL.Event]
getevents gd = do
    l <- readTVar (gdeventchan gd)
    writeTVar (gdeventchan gd) []
    return l


-- All events are handled in a seperate thread here
type Coord = (Int,Int)
type ApplePos = Coord
type Snake = [Coord]
type Length = Int
type Randoms = [Int]
data GameState = GameState Randoms ApplePos Snake Length Direction
               | DeadState Randoms ApplePos Snake
gamethread gd = forever $ do
    let eventchan = gdeventchan gd
    let drawchan = gddrawchan gd
    let stateref = gdstateref gd
    events <- atomically $ do
        events <- getevents gd
        when (events == []) retry
        return events
    --modifyIORef stateref $ gametick events
    state <- readIORef stateref
    let (mstate, newdraw) = handle_event events state
    print events
    writeIORef stateref mstate
    when newdraw $ do
        atomically $ writeTVar drawchan $ gamedraw gd state
        SDL.pushEvent $ SDL.User SDL.UID1 0 nullPtr nullPtr
    return ()
initstate :: Randoms -> GameState
initstate r = newapple $ GameState r (0,0) [(40,30)] 1 EAST
newapple state@(GameState r a s l d)
    | elem na s        = newapple $ GameState rr a s l d
    | not (contained na) = newapple $ GameState rr a s l d
    | otherwise        = GameState rr na s l d
    where
        na = ((r!!0)`mod`80, (r!!1)`mod`60)
        rr = (drop 2 r)
-- Magic numbers, ew
contained :: Coord -> Bool
contained (x,y)
    | x < 0 || x >= 80 = False
    | y < 0 || y >= 60 = False
    | otherwise = True
gamedraw :: GameData -> GameState -> IO ()
gamedraw (GameData font screen _ _ _) (GameState _ apple snake _ dir) = do
    let fmt = SDL.surfaceGetPixelFormat screen
    black <- SDL.mapRGB fmt 0 0 0
    SDL.fillRect screen Nothing black
    SDL.flip screen
    return ()
gamedraw (GameData font screen _ _ _) (DeadState _ apple snake) = do
    print "DED"
    let fmt = SDL.surfaceGetPixelFormat screen
    black <- SDL.mapRGB fmt 0 0 0
    SDL.fillRect screen Nothing black
    message <- TTF.renderTextSolid font "You're DEAD! Press space." (SDL.Color 255 0 0)
    let x = (SDL.surfaceGetWidth screen `div` 2) - (SDL.surfaceGetWidth message `div` 2)
        y = (SDL.surfaceGetHeight screen `div` 2) - (SDL.surfaceGetHeight message `div` 2)
        w = SDL.surfaceGetWidth message
        h = SDL.surfaceGetHeight message
        dst = SDL.Rect x y w h
    SDL.blitSurface message Nothing screen $ Just $ dst
    SDL.flip screen
    return ()

handle_event :: [SDL.Event] -> GameState -> (GameState, Bool)
handle_event events state = foldl handle (state, False) . reverse $ events
    where
        handle (s, d) (SDL.User SDL.UID0 0 _ _) = (s, True)
        handle (s, d) (SDL.User SDL.UID0 1 _ _) = (tick s, d)
        handle (s, d) (SDL.KeyDown (SDL.Keysym k _ _)) = (keydown k s, d)
        handle sd _ = sd
        keydown key state@(GameState r a s l d)
            | isdirkey key = GameState r a s l . dirchange d . dirfromkey $ key
            | otherwise = state
        keydown key state@(DeadState r _ _)
            | key == SDL.SDLK_SPACE = initstate r
        tick state@(GameState r a s l d) = check . tick' $ state
        tick state@(DeadState _ _ _) = state
        tick' state@(GameState r a s l d) = state -- TODO
        check state@(GameState r a s l d)
            | elem (head s) . tail $ s = DeadState r a s
            | head s == a = newapple state
            | otherwise = state


