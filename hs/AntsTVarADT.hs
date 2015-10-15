module Main where

import AntsBase
import BoardTVar
import ThingADT

main = do b <- antsMain :: IO (TBoard AThing)
          return ()
