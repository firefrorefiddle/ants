module Main where

import AntsBase
import BoardMVar
import ThingInt

main = do b <- antsMain :: IO (MBoard AThing)
          return ()
