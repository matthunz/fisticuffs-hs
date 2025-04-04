module Main where

import MyLib

main :: IO ()
main = run defaultConfig $ do
  _ <- addCharacter
  return ()
