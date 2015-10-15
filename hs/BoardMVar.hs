{-# LANGUAGE TypeFamilies #-}

module BoardTVar where

import Control.Concurrent.MVar
import Control.Monad
import Control.Applicative
import Data.Array
import Data.Maybe

import BoardClass
import ThingClass

data MBoard t = MBoard
                { getBoard     :: Array Coords (MVar t)
                , getNextAntId :: MVar AntId
                , getMoveCount :: MVar Int
                }

instance (Thing t) => BoardImpl (MBoard t) where
  emptyBoard h w   = emptyBoardMV h w
  move             = moveAnt
  countMoves b     = readMVar $ getMoveCount b
  addAnt b c       = addAntMV b c
  collectAnts b    = collectAntsMV b
  height           = snd . snd . bounds . getBoard
  width            = fst . snd . bounds . getBoard
  currentValue b c = getAntId <$> (readMVar $ (getBoard b) ! c)
  nextAntId b      = readMVar (getNextAntId b)

getField :: MBoard t -> Coords -> MVar t
getField = (!) . getBoard

emptyBoardMV :: (Thing t) => Int -> Int -> IO (MBoard t)
emptyBoardMV h w = do
  mvars         <- replicateM (h*w) (newMVar emptyThing)
  antIdTVar     <- newMVar 0
  moveCountTVar <- newMVar 0
  return . (\b -> MBoard b antIdTVar moveCountTVar) $
    (array ((1,1), (w,h)) (zip
                           [(i,j) | i <- [1..w], j <- [1..h]]
                           mvars))

moveAnt :: (Thing t) => MBoard t -> Coords -> Coords -> IO Bool
moveAnt b from to = do
  let fromMV = getField b from
  let toMV = getField b to
  toCont <- takeMVar toMV
  if isEmpty toCont
    then do
     fromCont <- takeMVar fromMV
     putMVar toMV fromCont
     putMVar fromMV toCont
     modifyMVar_ (getMoveCount b) (return . succ)
     return True
    else do
     putMVar toMV toCont
     return False

addAntMV :: (Thing t) => MBoard t -> Coords -> IO Bool
addAntMV b i = do
  let mv = getField b i
  val <- takeMVar mv
  if isEmpty val
    then do     
     antId <- modifyMVar (getNextAntId b) (\x -> return (succ x, x))
     putMVar mv (fromAntId antId)
     return True
    else do
     putMVar mv val
     return False

collectAntsMV :: (Thing t) => MBoard t -> IO [AntId]
collectAntsMV b = do
  ress <- forM [(i,j) | i <- [1..width b], j <- [1..height b]] $ \coords ->
    readMVar (getField b coords)
  return . catMaybes . map getAntId $ ress
