module Main where

import AntsBase
import BoardMVar
import ThingADT

main = do b <- antsMain :: IO (MBoard AThing)
          return ()
