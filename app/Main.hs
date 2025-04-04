module Main where

import MyLib

main :: IO ()
main = run defaultConfig $ do
  e <- addCharacter
  animate e "run"
  return ()
