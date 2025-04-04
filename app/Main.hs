module Main where

import Linear
import MyLib

main :: IO ()
main = run defaultConfig $ do
  e <- addCharacter
  animate e "run"
  face e West

  jump e

  return ()
