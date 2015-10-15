{-# LANGUAGE TypeFamilies #-}

module BoardTVar where

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Monad
import Control.Applicative
import Data.Array
import Data.Maybe

import BoardClass
import ThingClass

data TBoard t = TBoard
                { getBoard     :: Array Coords (TVar t)
                , getNextAntId :: TVar AntId
                , getMoveCount :: TVar Int
                }

instance (Thing t) => BoardImpl (TBoard t) where
  emptyBoard h w   = atomically $ emptyBoardST h w
  move             = moveAntIO
  countMoves b     = atomically (readTVar $ getMoveCount b)
  addAnt b c       = atomically $ addAntST b c
  collectAnts b    = atomically $ collectAntsST b
  height           = snd . snd . bounds . getBoard
  width            = fst . snd . bounds . getBoard
  currentValue b c = getAntId <$>
                     (atomically . readTVar $ (getBoard b) ! c)
  nextAntId b      = atomically $ readTVar (getNextAntId b)

getField :: TBoard t -> Coords -> TVar t
getField = (!) . getBoard

emptyBoardST :: (Thing t) => Int -> Int -> STM (TBoard t)
emptyBoardST h w = do
  tvars <- replicateM (h*w) (newTVar emptyThing)
  antIdTVar <- newTVar 0
  moveCountTVar <- newTVar 0
  return . (\b -> TBoard b antIdTVar moveCountTVar) $
    (array ((1,1), (w,h)) (zip
                           [(i,j) | i <- [1..w], j <- [1..h]]
                           tvars))

moveAnt :: (Thing t) => TBoard t -> Coords -> Coords -> STM Bool
moveAnt b from to = do
  let fromTV = getField b from
  let toTV = getField b to
  fromCont <- readTVar fromTV
  toCont <- readTVar toTV
  if isEmpty toCont && not (isEmpty fromCont)
    then do
     writeTVar toTV fromCont
     writeTVar fromTV toCont
     return True
    else (return False)

moveAntIO :: (Thing t) => TBoard t -> Coords -> Coords -> IO Bool
moveAntIO b from to = do
  res <- atomically $ moveAnt b from to
  case res of
   True -> do atomically $ modifyTVar' (getMoveCount b) succ `orElse` retry
              return True
   False -> return False

addAntST :: (Thing t) => TBoard t -> Coords -> STM Bool
addAntST b i = do
  let tv = getField b i
  val <- readTVar tv
  if isEmpty val
    then do
     antId <- readTVar (getNextAntId b)
     writeTVar tv (fromAntId antId)
     writeTVar (getNextAntId b) (antId + 1)
     return True
    else return False

collectAntsST :: (Thing t) => TBoard t -> STM [AntId]
collectAntsST b = do
  ress <- forM [(i,j) | i <- [1..width b], j <- [1..height b]] $ \coords ->
    readTVar (getField b coords)
  return . catMaybes . map getAntId $ ress
