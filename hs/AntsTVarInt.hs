module Main where

import AntsBase
import BoardTVar
import ThingInt

main = do b <- antsMain :: IO (TBoard AThing)
          return ()
