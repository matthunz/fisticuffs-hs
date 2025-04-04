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

data Animation = Animation
  { animationRow :: Int,
    animationCols :: Int
  }

data Direction = East | West
  deriving (Show, Eq)

data Character = Character
  { characterPosition :: V2 Int,
    characterDirection :: Direction,
    characterSize :: V2 Int,
    characterTexture :: Texture,
    characterAnimations :: Map String Animation,
    characterAnimation :: String,
    characterAnimationStart :: Word32
  }

data GameState = GameState
  { gameWindow :: Window,
    gameRenderer :: Renderer,
    gameTime :: Word32,
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
          UnacceleratedRender -> UnacceleratedRenderer
          AcceleratedRender -> AcceleratedRenderer
          SoftwareRender -> SoftwareRenderer
  window <- createWindow "Fisticuffs" windowConfig
  renderer <- createRenderer window (-1) (defaultRenderer {rendererType = ty})
  texture <- IMG.loadTexture renderer $ configCharacterTexture config
  t <- ticks
  return
    GameState
      { gameWindow = window,
        gameRenderer = renderer,
        gameVSync = vsyncType,
        gameTime = t,
        gameCharacterTexture = texture,
        gameCharacters = Map.empty,
        gameNextEntityID = EntityID 0
      }

draw :: GameState -> IO ()
draw game = do
  start <- ticks
  let game' = game {gameTime = start}
  case gameVSync game' of
    VSync -> do
      rendererDrawColor (gameRenderer game') $= V4 0 0 0 255
      clear $ gameRenderer game'
      drawCharacters game'
      present $ gameRenderer game'

      draw game'
    NoVSync _ frameDelayUs -> do
      rendererDrawColor (gameRenderer game') $= V4 0 0 0 255
      clear $ gameRenderer game'
      drawCharacters game'
      present $ gameRenderer game'

      endTicks <- ticks
      let elapsedMs = endTicks - start
          elapsedUs = fromIntegral elapsedMs * 1000
          delayTime = frameDelayUs - elapsedUs
      when (delayTime > 0) . threadDelay $ fromIntegral delayTime
      draw game'

drawCharacters :: GameState -> IO ()
drawCharacters game = do
  let characters = gameCharacters game
  forM_ (Map.toList characters) $ \(EntityID _, character) -> do
    let pos = fromIntegral <$> characterPosition character
        a = characterAnimations character Map.! characterAnimation character
        col =
          fromIntegral (gameTime game - characterAnimationStart character)
            `div` 100
            `mod` animationCols a
        V2 width height = characterSize character
        flipped = case characterDirection character of
          East -> False
          West -> True
    copyEx
      (gameRenderer game)
      (characterTexture character)
      ( Just
          ( Rectangle
              ( P $
                  V2
                    (fromIntegral $ col * width)
                    (fromIntegral $ height * animationRow a)
              )
              (V2 (fromIntegral width) (fromIntegral height))
          )
      )
      (Just (Rectangle (P pos) (V2 1000 1000)))
      1
      Nothing
      (V2 flipped False)

newtype Game a = Game {unGame :: StateT GameState IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

runGameT :: Game a -> GameState -> IO (a, GameState)
runGameT game = runStateT (unGame game)

addCharacter :: Game EntityID
addCharacter = Game $ do
  gameState <- get
  let EntityID e = gameNextEntityID gameState
      animations = Map.fromList [("idle", Animation 0 8), ("run", Animation 1 8)]
      newCharacter =
        Character
          { characterPosition = V2 0 0,
            characterDirection = West,
            characterSize = V2 200 200,
            characterTexture = gameCharacterTexture gameState,
            characterAnimations = animations,
            characterAnimation = "idle",
            characterAnimationStart = gameTime gameState
          }
      newCharacters = Map.insert (EntityID e) newCharacter (gameCharacters gameState)
      newGameState =
        gameState
          { gameCharacters = newCharacters,
            gameNextEntityID = EntityID $ e + 1
          }
  put newGameState
  return $ EntityID e

animate :: EntityID -> String -> Game ()
animate entityID animationName = Game $ do
  gameState <- get
  let characters = gameCharacters gameState
  case Map.lookup entityID characters of
    Just character ->
      let newCharacter =
            character
              { characterAnimation = animationName,
                characterAnimationStart = gameTime gameState
              }
          newCharacters = Map.insert entityID newCharacter characters
          newGameState = gameState {gameCharacters = newCharacters}
       in put newGameState
    Nothing -> liftIO $ putStrLn $ "EntityID " ++ show entityID ++ " not found."

run :: Config -> Game a -> IO a
run cfg game = do
  gameState <- newGame cfg
  (a, gameState') <- runGameT game gameState
  draw gameState'
  return a
