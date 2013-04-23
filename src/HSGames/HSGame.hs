module HSGames.HSGame (
    HSGame(..),
    hsgRun
) where

import GHC.Ptr(nullPtr)
import Data.Function(fix)
import Control.Monad(forever, when, mapM)
import Control.Concurrent(forkIO, threadDelay, killThread)
import Control.Concurrent.STM(readTVar, writeTVar, readTVarIO, retry, atomically, STM)
import Control.Concurrent.STM.TVar(TVar)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

class HSGame a where
    hsgDrawSTM :: a -> TVar (IO())
    hsgEventSTM :: a -> TVar [SDL.Event]
    hsgHandle :: a -> [SDL.Event] -> IO()
    hsgGetDraw :: a -> IO()
    hsgGetDraw gd = do
        draw <- readTVarIO . hsgDrawSTM $ gd
        draw
    hsgDraw :: a -> IO() -> IO()
    hsgDraw gd draw = do
        let drawchan = hsgDrawSTM gd
        atomically $ writeTVar drawchan $ draw
        SDL.pushEvent $ SDL.User SDL.UID1 0 nullPtr nullPtr
    hsgSendEvent :: a -> SDL.Event -> STM()
    hsgSendEvent gd e = do
        let eventchan = hsgEventSTM gd
        l <- readTVar eventchan
        writeTVar eventchan (e:l)
        return ()
    hsgEvents :: a -> STM([SDL.Event])
    hsgEvents gd = do
        let eventchan = hsgEventSTM gd
        l <- readTVar eventchan
        writeTVar eventchan []
        return l

hsgRun :: HSGame a => IO(a,[(Int,Int)]) -> IO ()
hsgRun gi = do
    SDL.init [SDL.InitEverything]
    TTF.init

    (gd, tickers) <- gi
    tickerThreadIDs <- mapM (forkIO . tickerthread) tickers
    gameThreadID <- forkIO $ gamethread gd

    fix $ \loop -> do
        e <- SDL.waitEvent
        case e of
            SDL.Quit -> do
                return ()
            SDL.User SDL.UID1 0 _ _ -> do
                hsgGetDraw gd
                loop
            _ -> do
                atomically $ hsgSendEvent gd e
                loop
    mapM killThread tickerThreadIDs
    --TTF.quit
    SDL.quit
tickerthread (micros,index) = forever $ do
    SDL.pushEvent $ SDL.User SDL.UID0 index nullPtr nullPtr
    threadDelay micros
gamethread :: HSGame a => a -> IO()
gamethread gd = forever $ do
    events <- atomically $ do
        events <- hsgEvents gd
        when (events == []) retry
        return events
    hsgHandle gd events
    return ()



