{-# LANGUAGE TypeFamilies #-}

module BoardTVar where

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Concurrent
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
  emptyBoard h w   = atomically $ emptyBoardSTM h w
  move f b from to = atomically $ moveSTM f b from to
  countMoves b     = atomically (readTVar $ getMoveCount b)
  addAnt b c       = atomically $ addAntSTM b c
  collectAnts b    = atomically $ collectAntsSTM b
  height           = snd . snd . bounds . getBoard
  width            = fst . snd . bounds . getBoard
  currentValue b c = getAntId <$>
                     (atomically . readTVar $ (getBoard b) ! c)
  nextAntId b      = atomically $ readTVar (getNextAntId b)

getField :: TBoard t -> Coords -> TVar t
getField = (!) . getBoard

emptyBoardSTM :: (Thing t) => Int -> Int -> STM (TBoard t)
emptyBoardSTM h w = do
  tvars <- replicateM (h*w) (newTVar emptyThing)
  antIdTVar <- newTVar 0
  moveCountTVar <- newTVar 0
  return . (\b -> TBoard b antIdTVar moveCountTVar) $
    (array ((1,1), (w,h)) (zip
                           [(i,j) | i <- [1..w], j <- [1..h]]
                           tvars))

moveSTM :: (Thing t) => [MoveFlag] -> TBoard t ->
          Coords -> Coords -> STM Bool
moveSTM flags b from to
  | TestOnly `elem` flags =
      isEmpty <$> readTVar (getField b to)
  | otherwise = do
      res <- doMove `orElse` recover
      when res $ updateMoveCounter b
      return res
   where doMove = do let fromTV = getField b from
                     let toTV = getField b to
                     fromCont <- readTVar fromTV
                     toCont <- readTVar toTV
                     check (isEmpty toCont)
                     writeTVar toTV fromCont
                     writeTVar fromTV toCont
                     return True
         recover | NoBlock `elem` flags = return False
                 | otherwise = retry >> return True
  
updateMoveCounter :: TBoard t -> STM ()
updateMoveCounter b = modifyTVar' (getMoveCount b) succ `orElse` retry
  
addAntSTM :: (Thing t) => TBoard t -> Coords -> STM Bool
addAntSTM b i = do
  let tv = getField b i
  val <- readTVar tv
  if isEmpty val
    then do
     antId <- readTVar (getNextAntId b)
     writeTVar tv (fromAntId antId)
     writeTVar (getNextAntId b) (antId + 1)
     return True
    else return False

collectAntsSTM :: (Thing t) => TBoard t -> STM [AntId]
collectAntsSTM b = do
  ress <- forM [(i,j) | i <- [1..width b], j <- [1..height b]] $ \coords ->
    readTVar (getField b coords)
  return . catMaybes . map getAntId $ ress

