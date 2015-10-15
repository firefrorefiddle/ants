module Main where

import AntsBase
import BoardTVar
import ThingInt

main = do b <- antsMainPos 40 40 :: IO (TBoard AThing)
          return ()
