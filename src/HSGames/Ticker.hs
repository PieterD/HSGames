module HSGames.Ticker (
    TickerMaster,
    initTicker,
    startTicker,
    endTicker,
    endAllTickers
) where

import Data.List(partition)
import Control.Concurrent.STM(newTVarIO, readTVar, writeTVar, readTVarIO, modifyTVar, retry, atomically, STM)
import Control.Concurrent.STM.TVar(TVar)
import Control.Concurrent(forkIO, threadDelay, killThread, ThreadId)
import Control.Monad(forever)
import GHC.Ptr(nullPtr)
import qualified Graphics.UI.SDL as SDL

type Index = Int
type Micros = Int
type TickerPair = (Index, Micros)
type TickerTriple = (ThreadId, TickerPair)
data TickerMaster = TickerMaster (TVar [TickerTriple])

initTicker :: IO TickerMaster
initTicker = do
    ref <- newTVarIO []
    return (TickerMaster ref)

startTicker :: TickerMaster -> Index -> Micros -> IO ()
startTicker (TickerMaster ref) index micros = do
    let tp = (index, micros)
    id <- forkIO $ tickerThread tp
    atomically . modifyTVar ref . (:) $ (id, tp)
    return ()

endTicker :: TickerMaster -> Index -> IO ()
endTicker (TickerMaster ref) index = do
    killable <- atomically $ do
        xs <- readTVar ref
        let (kill, rest) = partition ((== index) . fst . snd) xs
        writeTVar ref rest
        return kill
    mapM (killThread . fst) killable
    return ()

endAllTickers :: TickerMaster -> IO ()
endAllTickers (TickerMaster ref) = do
    killable <- atomically $ do
        xs <- readTVar ref
        writeTVar ref []
        return xs
    mapM (killThread . fst) killable
    return ()

tickerThread :: TickerPair -> IO ()
tickerThread (index, micros) = forever $ do
    SDL.pushEvent $ SDL.User SDL.UID0 index nullPtr nullPtr
    threadDelay micros


