{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module MyLib where

import Control.Concurrent
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Word
import SDL hiding (get)
import qualified SDL.Image as IMG

data RenderType = UnacceleratedRender | AcceleratedRender | SoftwareRender
  deriving (Show, Eq)

data VSyncType = VSync | NoVSync RenderType Word32
  deriving (Show, Eq)

data Config = Config
  { configWindowSize :: V2 Int,
    configVSync :: VSyncType,
    configCharacterTexture :: FilePath
  }

defaultConfig :: Config
defaultConfig =
  Config
    { configWindowSize = V2 800 600,
      configVSync = VSync,
      configCharacterTexture = "assets/character.png"
    }

newtype EntityID = EntityID Word64
  deriving (Show, Eq, Ord)

data Character = Character
  { characterPosition :: V2 Int,
    characterTexture :: Texture
  }

data GameState = GameState
  { gameWindow :: Window,
    gameRenderer :: Renderer,
    gameVSync :: VSyncType,
    gameCharacterTexture :: Texture,
    gameCharacters :: Map EntityID Character,
    gameNextEntityID :: EntityID
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
  texture <- IMG.loadTexture renderer $ configCharacterTexture config
  return
    GameState
      { gameWindow = window,
        gameRenderer = renderer,
        gameVSync = vsyncType,
        gameCharacterTexture = texture,
        gameCharacters = Map.empty,
        gameNextEntityID = EntityID 0
      }

draw :: GameState -> IO ()
draw game = do
  case gameVSync game of
    VSync -> do
      rendererDrawColor (gameRenderer game) $= V4 0 0 0 255
      clear $ gameRenderer game
      drawCharacters game
      present $ gameRenderer game

      draw game
    NoVSync _ frameDelayUs -> do
      startTicks <- ticks

      rendererDrawColor (gameRenderer game) $= V4 0 0 0 255
      clear $ gameRenderer game
      drawCharacters game
      present $ gameRenderer game

      endTicks <- ticks
      let elapsedMs = endTicks - startTicks
          elapsedUs = fromIntegral elapsedMs * 1000
          delayTime = frameDelayUs - elapsedUs
      when (delayTime > 0) . threadDelay $ fromIntegral delayTime
      draw game

drawCharacters :: GameState -> IO ()
drawCharacters game = do
  let characters = gameCharacters game
  forM_ (Map.toList characters) $ \(EntityID _, character) -> do
    let pos = fromIntegral <$> characterPosition character
    copy
      (gameRenderer game)
      (characterTexture character)
      (Just (Rectangle (P pos) (V2 200 200)))
      (Just (Rectangle (P pos) (V2 200 200)))

newtype Game a = Game {unGame :: StateT GameState IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

runGameT :: Game a -> GameState -> IO (a, GameState)
runGameT game = runStateT (unGame game)

addCharacter :: Game EntityID
addCharacter = Game $ do
  gameState <- get
  let EntityID e = gameNextEntityID gameState
      newCharacter = Character (V2 0 0) (gameCharacterTexture gameState)
      newCharacters = Map.insert (EntityID e) newCharacter (gameCharacters gameState)
      newGameState =
        gameState
          { gameCharacters = newCharacters,
            gameNextEntityID = EntityID $ e + 1
          }
  put newGameState
  return $ EntityID e

run :: Config -> Game a -> IO a
run cfg game = do
  gameState <- newGame cfg
  (a, gameState') <- runGameT game gameState
  draw gameState'
  return a
