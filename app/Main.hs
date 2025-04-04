module Main where

import Fisticuffs

main :: IO ()
main = run defaultConfig $ do
  e <- addCharacter
  animate e "run"
  face e West

  jump e

  return ()
