{-# LANGUAGE OverloadedStrings #-}

module MyLib where

import SDL

run :: IO ()
run = do
  initializeAll
  window <- createWindow "Fisticuffs" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  draw renderer

draw :: Renderer -> IO ()
draw renderer = do
  rendererDrawColor renderer $= V4 255 0 0 255
  fillRect renderer (Just (Rectangle (P (V2 10 10)) (V2 50 50)))
  present renderer
  draw renderer
