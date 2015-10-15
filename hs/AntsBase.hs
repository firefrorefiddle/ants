{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module AntsBase where

import Control.Monad
import Control.Applicative
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Control.Concurrent.MVar
import Data.IORef
import Data.Array
import Data.List
import Data.Maybe
import Text.Printf
import System.Random
import Debug.Trace
import System.Process
import System.Environment
import System.IO.Unsafe
import System.Time
import BoardClass
import ThingClass

import BoardTVar
import ThingADT

neighbours :: (BoardImpl b) => b -> Coords -> [Coords]
neighbours b (i,j) = filter onBoard [(i+1,j), (i-1,j), (i,j+1), (i,j-1)]
  where onBoard (i,j) = i >= 1 && i <= width b &&
                        j >= 1 && j <= height b

neighbours8 :: (BoardImpl b) => b -> Coords -> [Coords]
neighbours8 b (i,j) = filter onBoard [(i',j') | i' <- [i-1,i,i+1], j' <- [j-1,j,j+1]]
  where onBoard (i,j) = i >= 1 && i <= width b &&
                        j >= 1 && j <= height b

randomNeighbour :: [Coords] -> IO Coords
randomNeighbour cands = do
  idx <- (`mod` length cands) <$> randomIO
  return $ cands !! idx

randomNeighbourWeighted :: [Coords] -> (Coords -> Int) -> IO Coords
randomNeighbourWeighted cands wf = do
  let weights = map wf cands
      weights' = if minimum weights <= 0
                 then map (+ (abs (minimum weights) + 1)) weights
                 else weights
  i <- randomRIO (1, sum weights')
  return $ getCand i (zip cands weights')
 where getCand 0 ((c,_):_) = c
       getCand w ((c,wi):cs) | w <= wi = c
                             | otherwise = getCand (w-wi) cs

wfProgress :: Coords -> Coords -> Coords -> Int
wfProgress (xfrom,yfrom) (xto,yto) (xcand,ycand) =
  (absdiff xto xfrom - absdiff xto xcand) +
  (absdiff yto yfrom - absdiff yto ycand)
  where absdiff x y = abs (abs x - abs y)

wfNeutral :: Coords -> Int
wfNeutral = const 1

mmap = map.map

showBoardBase :: (BoardImpl b, PrintfType r) => (r -> r -> r) -> b -> IO r
showBoardBase conc b = do
  len <- length . show . pred <$> nextAntId b
  vals <- currentValues b
  return $ foldr1 conc $ map (showLine len) vals
 where showLine len vals = (foldr1 conc $ map (showThing len) vals) `conc`
                           (printf "\n")
       showThing len Nothing  = foldr1 conc $ replicate len (printf "-")
       showThing len (Just a) = printf ("%0"++show len++"d") a
       
printBoard :: (BoardImpl b) => b -> IO ()
printBoard b = join (showBoardBase (>>) b)

showBoard :: (BoardImpl b) => b -> IO String
showBoard = showBoardBase (++)

randomCoords :: (BoardImpl b) => b -> IO Coords
randomCoords b = do
  i <- (+1) . (`mod` (width b)) <$> randomIO
  j <- (+1) . (`mod` (height b)) <$> randomIO 
  return (i,j)

addRandomAnt :: (BoardImpl b) => b -> IO Coords
addRandomAnt b = do
          coords <- randomCoords b
          res <- addAnt b coords
          case res of
           True -> return coords
           False -> addRandomAnt b

addRandomAnts :: (BoardImpl b) => b -> Int -> IO [Coords]
addRandomAnts b n = replicateM n (addRandomAnt b)
                                             
type Life = (BoardImpl b) => b -> Coords -> IO ()

blockDelay =   0
blockWait  = 150
minDelay   =  50
maxDelay   = 150

moveAroundRandom :: Int -> Life
moveAroundRandom 0   _ _ = return ()
moveAroundRandom tdl b pos = do
  newPos <- randomNeighbour (neighbours b pos)
  res <- tryMove b pos newPos
  case res of
   False -> do threadDelay blockDelay
               moveAroundRandom tdl b pos
   True -> do delay <- (+minDelay) . (`mod` (maxDelay-minDelay)) <$> randomIO 
              threadDelay delay
              moveAroundRandom (tdl-1) b newPos

moveTo :: Coords -> Life
moveTo toPos b pos | toPos == pos = return ()
                   | otherwise = do
                       newPos <- randomNeighbourWeighted (neighbours8 b pos) (wfProgress pos toPos)
                       res <- tryMove b pos newPos
                       case res of
                        False -> do threadDelay blockDelay
                                    moveTo toPos b pos
                        True -> do delay <- (+minDelay) . (`mod` (maxDelay-minDelay)) <$> randomIO 
                                   threadDelay delay
                                   moveTo toPos b newPos

addLiveAnt :: (BoardImpl b) => b -> Life -> IO (MVar ())
addLiveAnt b l = do
  mv <- newEmptyMVar
  coords <- addRandomAnt b
  forkIO $ do l b coords
              putMVar mv ()
  return mv

mps :: Int -> TimeDiff -> Double
mps moves diff =
  fromIntegral (moves * 10^12) /
  fromIntegral (fromIntegral (tdSec diff) * 10^12 + tdPicosec diff)

showUpdates :: (BoardImpl b) => b -> Integer -> IO ()
showUpdates b totalMoves = do
  initTime <- getClockTime
  go 0 initTime initTime
    where go oldMoves initTime oldTime = do
          system "clear"
          moves <- countMoves b
          time <- getClockTime
          let diff = time `diffClockTimes` oldTime          
          printf "  %d/%d moves (%.0f%%)\n" moves totalMoves
            ((fromIntegral moves*100 / fromIntegral totalMoves) :: Double)
          printf "      mps: %7.2f\n" (mps (moves-oldMoves) diff)
          printf "  avg mps: %7.2f\n" (mps moves (time `diffClockTimes` initTime))          
          printBoard b
          threadDelay 100000
          go moves initTime time

validateBoard :: (BoardImpl b) => b -> IO Bool
validateBoard b = do
  ants <- collectAnts b
  nextId <- nextAntId b
  return $ sort ants == [0..nextId-1]

antsMain :: (BoardImpl b) => IO b
antsMain = do
  args <- getArgs
  let contentionFactor =
        case args of
         [] -> 0.2
         [f] -> read f :: Float
  b <- emptyBoard 40 40
  let tdl = 1000
  let w = 40
  let h = 40
  mvs <- replicateM (round $ w*h*contentionFactor)
         (addLiveAnt b (moveAroundRandom tdl))
  forkIO $ showUpdates b (round $ w*h*contentionFactor*fromIntegral tdl)
  mapM_ takeMVar mvs
  threadDelay 200000
  validateBoard b >>= print
  return b

antsMainPos :: Int -> Int -> (BoardImpl b) => IO b
antsMainPos w h = do
  b <- emptyBoard w h
  let poss = [(x,y) | x <- [1,3..w-1], y <- [1,3..w-1]] ++
             [(x,y) | x <- [2,4..w],   y <- [2,4..w]]
  mvs <- mapM (\toPos -> addLiveAnt b (moveTo toPos)) poss
  forkIO $ showUpdates b (100000)
  mapM_ takeMVar mvs
  threadDelay 200000
  validateBoard b >>= print
  return b
  
