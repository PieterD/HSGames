module HSGames.Monad (
    UIData,
    UIState,
    uiRun,
    uiWrap,
    centerMessage,
    fillScreen,
    fillRect,
    pixel
) where

import Control.Monad(sequence_, liftM)
import Control.Monad.State(State, runState, get, put)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF

import qualified HSGames.Ticker as Ticker

type UIData = (SDL.Surface, TTF.Font, Ticker.TickerMaster)
type UIState = State (UIData, [IO ()])

pixel r g b = SDL.Pixel (r*256*256 + g*256 + b)

uiRun :: UIData -> UIState () -> IO ()
uiRun uid uis = do
    let (_, ios) = runState (uis >> flipScreen) (uid, [])
    sequence_ . reverse . snd $ ios

uiWrap :: (UIData -> IO a) -> UIState ()
uiWrap iof = do
    (uid, ios) <- get
    let io' = do
        iof uid
        return ()
    put (uid, io':ios)

centerMessage :: String -> UIState ()
centerMessage str = uiWrap $ \(screen, font, _) -> do
    message <- TTF.renderTextSolid font str (SDL.Color 255 0 0)
    let x = (SDL.surfaceGetWidth screen `div` 2) - (SDL.surfaceGetWidth message `div` 2)
        y = (SDL.surfaceGetHeight screen `div` 2) - (SDL.surfaceGetHeight message `div` 2)
        w = SDL.surfaceGetWidth message
        h = SDL.surfaceGetHeight message
        dst = SDL.Rect x y w h
    SDL.blitSurface message Nothing screen $ Just $ dst

flipScreen :: UIState ()
flipScreen = uiWrap $ \(screen, _, _) -> do
    SDL.flip screen

fillScreen :: SDL.Pixel -> UIState ()
fillScreen pix = uiWrap $ \(screen, _, _) -> do
    SDL.fillRect screen Nothing pix

fillRect :: SDL.Rect -> SDL.Pixel -> UIState ()
fillRect rect pix = uiWrap $ \(screen, _, _) -> do
    SDL.fillRect screen (Just rect) pix




