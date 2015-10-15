{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BoardClass where

import ThingClass
import Control.Monad

type Coords = (Int, Int)

class BoardImpl b where
  emptyBoard     :: Int -> Int -> IO b
  move           :: b -> Coords -> Coords -> IO Bool
  addAnt         :: b -> Coords -> IO Bool
  collectAnts    :: b -> IO [AntId]
  countMoves     :: b -> IO Int
  width          :: b -> Int
  height         :: b -> Int
  currentValue   :: b -> Coords -> IO (Maybe AntId)
  nextAntId      :: b -> IO Int

currentValues :: (BoardImpl b) => b -> IO [[Maybe AntId]]
currentValues b = do
  forM [1..height b] $ \i ->
    forM [1..width b] $ \j -> currentValue b (j,i)

