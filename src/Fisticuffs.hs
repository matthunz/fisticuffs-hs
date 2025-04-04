{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Fisticuffs where

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
    { configWindowSize = V2 2000 1000,
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
    characterVelocity :: V2 Float,
    characterAcceleration :: V2 Float,
    characterDirection :: Direction,
    characterSize :: V2 Int,
    characterTexture :: Texture,
    characterAnimations :: Map String Animation,
    characterAnimation :: String,
    characterActionAnimation :: Maybe (String, Word32),
    characterAnimationStart :: Word32
  }

data GameState = GameState
  { gameWindow :: Window,
    gameRenderer :: Renderer,
    gameTick :: Word32,
    gameLastTick :: Word32,
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
        gameTick = t,
        gameLastTick = t,
        gameCharacterTexture = texture,
        gameCharacters = Map.empty,
        gameNextEntityID = EntityID 0
      }

draw :: Game ()
draw = do
  start <- ticks
  game <- Game get
  let game' = game {gameTick = start, gameLastTick = gameTick game}
  Game $ put game'
  case gameVSync game' of
    VSync -> do
      rendererDrawColor (gameRenderer game') $= V4 0 0 0 255
      clear $ gameRenderer game'
      drawCharacters
      present $ gameRenderer game'

      draw
    NoVSync _ frameDelayUs -> do
      rendererDrawColor (gameRenderer game') $= V4 0 0 0 255
      clear $ gameRenderer game'
      drawCharacters
      present $ gameRenderer game'

      endTicks <- ticks
      let elapsedMs = endTicks - start
          elapsedUs = fromIntegral elapsedMs * 1000
          delayTime = frameDelayUs - elapsedUs
      liftIO $ when (delayTime > 0) . threadDelay $ fromIntegral delayTime
      draw

drawCharacters :: Game ()
drawCharacters = do
  game <- Game get
  let characters = gameCharacters game
  characters' <- traverse drawCharacter characters
  Game . put $ game {gameCharacters = characters'}

drawCharacter :: Character -> Game Character
drawCharacter character = do
  game <- Game get
  let character' = updateCharacter game character
      pos = fromIntegral <$> characterPosition character'
      go a =
        fromIntegral (gameTick game - characterAnimationStart character')
          `div` 100
          `mod` animationCols a
      (row, col, character'') = case characterActionAnimation character' of
        Just (s, duration) ->
          let a = characterAnimations character' Map.! s
              c =
                fromIntegral (gameTick game - characterAnimationStart character') `div` 100
           in if c > duration
                then
                  let a' = characterAnimations character' Map.! characterAnimation character'
                   in ( animationRow a',
                        go a',
                        character'
                          { characterActionAnimation = Nothing,
                            characterAnimationStart = gameTick game
                          }
                      )
                else (animationRow a, fromIntegral c, character')
        Nothing ->
          let a = characterAnimations character' Map.! characterAnimation character'
           in (animationRow a, go a, character')
      V2 width height = characterSize character''
      flipped = case characterDirection character'' of
        East -> False
        West -> True
  copyEx
    (gameRenderer game)
    (characterTexture character'')
    ( Just
        ( Rectangle
            ( P $
                V2
                  (fromIntegral $ col * width)
                  (fromIntegral $ height * row)
            )
            (V2 (fromIntegral width) (fromIntegral height))
        )
    )
    (Just (Rectangle (P pos) (V2 1000 1000)))
    1
    Nothing
    (V2 flipped False)
  return character''

updateCharacter :: GameState -> Character -> Character
updateCharacter game character =
  let dt = fromIntegral $ gameTick game - gameLastTick game
      a = characterAcceleration character + V2 0 ((9.81 / 50000) * dt)
      v = characterVelocity character + a * V2 dt dt
      newPos = (fromIntegral <$> characterPosition character) + (v * V2 dt dt)
   in character
        { characterPosition = fmap (round . min 0) newPos, -- TODO
          characterVelocity = v,
          characterAcceleration = a
        }

newtype Game a = Game {unGame :: StateT GameState IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

runGameT :: Game a -> GameState -> IO (a, GameState)
runGameT game = runStateT (unGame game)

addCharacter :: Game EntityID
addCharacter = Game $ do
  gameState <- get
  let EntityID e = gameNextEntityID gameState
      animations = Map.fromList [("idle", Animation 0 8), ("run", Animation 1 8), ("jump", Animation 2 2)]
      newCharacter =
        Character
          { characterPosition = V2 0 0,
            characterVelocity = V2 0 0,
            characterAcceleration = V2 0 0,
            characterDirection = East,
            characterSize = V2 200 200,
            characterTexture = gameCharacterTexture gameState,
            characterAnimations = animations,
            characterAnimation = "idle",
            characterActionAnimation = Nothing,
            characterAnimationStart = gameTick gameState
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
                characterAnimationStart = gameTick gameState
              }
          newCharacters = Map.insert entityID newCharacter characters
          newGameState = gameState {gameCharacters = newCharacters}
       in put newGameState
    Nothing -> liftIO $ putStrLn $ "EntityID " ++ show entityID ++ " not found."

animateAction :: EntityID -> String -> Word32 -> Game ()
animateAction entityID animationName duration = Game $ do
  gameState <- get
  let characters = gameCharacters gameState
  case Map.lookup entityID characters of
    Just character ->
      let newCharacter =
            character
              { characterActionAnimation = Just (animationName, duration),
                characterAnimationStart = gameTick gameState
              }
          newCharacters = Map.insert entityID newCharacter characters
          newGameState = gameState {gameCharacters = newCharacters}
       in put newGameState
    Nothing -> liftIO $ putStrLn $ "EntityID " ++ show entityID ++ " not found."

face :: EntityID -> Direction -> Game ()
face entityID direction = Game $ do
  gameState <- get
  let characters = gameCharacters gameState
  case Map.lookup entityID characters of
    Just character ->
      let newCharacter =
            character
              { characterDirection = direction,
                characterAnimationStart = gameTick gameState
              }
          newCharacters = Map.insert entityID newCharacter characters
          newGameState = gameState {gameCharacters = newCharacters}
       in put newGameState
    Nothing -> liftIO $ putStrLn $ "EntityID " ++ show entityID ++ " not found."

accelerate :: EntityID -> V2 Float -> Game ()
accelerate entityID acceleration = Game $ do
  gameState <- get
  let characters = gameCharacters gameState
  case Map.lookup entityID characters of
    Just character ->
      let newCharacter =
            character
              { characterVelocity = acceleration,
                characterAcceleration = acceleration
              }
          newCharacters = Map.insert entityID newCharacter characters
          newGameState = gameState {gameCharacters = newCharacters}
       in put newGameState
    Nothing -> liftIO $ putStrLn $ "EntityID " ++ show entityID ++ " not found."

jump :: EntityID -> Game ()
jump e = do
  animateAction e "jump" 2
  accelerate e (V2 0 (-0.03))

run :: Config -> Game a -> IO a
run cfg game = do
  gameState <- newGame cfg
  let game' = do
        a <- game
        draw
        return a
  (a, _) <- runGameT game' gameState
  return a
