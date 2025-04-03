{-# LANGUAGE OverloadedStrings #-}

module MyLib where

import Control.Concurrent
import Control.Monad.State.Strict
import Data.Word
import SDL

data RenderType = UnacceleratedRender | AcceleratedRender | SoftwareRender
  deriving (Show, Eq)

data VSyncType = VSync | NoVSync RenderType Word32
  deriving (Show, Eq)

data Config = Config
  { configWindowSize :: V2 Int,
    configVSync :: VSyncType
  }

defaultConfig :: Config
defaultConfig =
  Config
    { configWindowSize = V2 800 600,
      configVSync = VSync
    }

newGame :: Config -> IO GameState
newGame config = do
  initializeAll
  let size = configWindowSize config
      vsyncType = configVSync config
      windowConfig =
        defaultWindow
          { windowInitialSize = fmap fromIntegral size,
            windowResizable = True
          }
      ty = case vsyncType of
        VSync -> AcceleratedVSyncRenderer
        NoVSync renderType _ -> case renderType of
          UnacceleratedRender -> SoftwareRenderer
          AcceleratedRender -> AcceleratedRenderer
          SoftwareRender -> SoftwareRenderer
  window <- createWindow "Fisticuffs" windowConfig
  renderer <- createRenderer window (-1) (defaultRenderer {rendererType = ty})
  return GameState {gameWindow = window, gameRenderer = renderer, gameVSync = vsyncType}

data GameState = GameState
  { gameWindow :: Window,
    gameRenderer :: Renderer,
    gameVSync :: VSyncType
  }

draw :: GameState -> IO ()
draw game = do
  case gameVSync game of
    VSync -> do
      rendererDrawColor (gameRenderer game) $= V4 255 0 0 255
      fillRect (gameRenderer game) (Just (Rectangle (P (V2 10 10)) (V2 50 50)))
      present $ gameRenderer game

      draw game
    NoVSync _ frameDelayUs -> do
      startTicks <- ticks

      rendererDrawColor (gameRenderer game) $= V4 255 0 0 255
      fillRect (gameRenderer game) (Just (Rectangle (P (V2 10 10)) (V2 50 50)))
      present $ gameRenderer game

      endTicks <- ticks
      let elapsedMs = endTicks - startTicks
          elapsedUs = fromIntegral elapsedMs * 1000
          delayTime = frameDelayUs - elapsedUs
      when (delayTime > 0) . threadDelay $ fromIntegral delayTime
      draw game

run :: Config -> IO ()
run cfg = do
  gameState <- newGame cfg
  draw gameState
