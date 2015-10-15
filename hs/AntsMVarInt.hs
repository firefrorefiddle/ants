{-# LANGUAGE ScopedTypeVariables #-}

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

{-# NOINLINE moveCounter #-}
moveCounter = unsafePerformIO $ newIORef (0::Int)

type Thing = Int

data Board = Board
             { getBoard :: Array Coords (MVar Thing)
             , getNextAntId :: MVar Int
             }
                    
type Coords = (Int, Int)

height, width :: Board -> Int
height = snd . snd . bounds . getBoard
width  = fst . snd . bounds . getBoard

neighbours :: Board -> Coords -> [Coords]
neighbours b (i,j) = filter onBoard [(i+1,j), (i-1,j), (i,j+1), (i,j-1)]
  where onBoard (i,j) = i >= 1 && i <= width b &&
                        j >= 1 && j <= height b

randomNeighbour :: Board -> Coords -> IO Coords
randomNeighbour b c = do
  let cands = neighbours b c
  idx <- (`mod` length cands) <$> randomIO
  return $ cands !! idx

getField :: Board -> Coords -> MVar Thing
getField = (!) . getBoard

emptyBoard :: Int -> Int -> IO Board
emptyBoard h w = do
  mvars <- replicateM (h*w) (newMVar (-1))
  antIdTVar <- newMVar 0
  return . flip Board antIdMVar $
    (array ((1,1), (w,h)) (zip
                           [(i,j) | i <- [1..w], j <- [1..h]]
                           mvars))
    

currentValues :: Board -> IO [[Thing]]
currentValues b = do
  forM [1..height b] $ \i ->
    forM [1..width b] $ \j -> readMVar (getBoard b ! (j,i))

mmap = map.map

showBoard :: Board -> IO String
showBoard b = do
  len <- length . show . pred <$> readMVar (getNextAntId b)
  unlines . map unwords . mmap (showThing len) <$>
              currentValues b
  where showThing len (-1) = replicate len '-'
        showThing len a = printf ("%0"++show len++"d") a
        

printBoard :: Board -> IO ()
printBoard b = (atomically $ showBoard b) >>= putStr

addAnt :: Board -> Coords -> STM Bool
addAnt b i = do
  let tv = getField b i
  val <- readTVar tv
  case val of
   (-1) -> do
     antId <- readTVar (getNextAntId b)
     writeTVar tv antId
     writeTVar (getNextAntId b) (antId + 1)
     return True
   n -> return False

randomCoords :: Board -> IO Coords
randomCoords b = do
  i <- (+1) . (`mod` (width b)) <$> randomIO
  j <- (+1) . (`mod` (height b)) <$> randomIO 
  return (i,j)

addRandomAnt :: Board -> IO Coords
addRandomAnt b = do
          coords <- randomCoords b
          res <- atomically $ addAnt b coords
          case res of
           True -> return coords
           False -> (addRandomAnt b)

addRandomAnts :: Board -> Int -> IO [Coords]
addRandomAnts b n = replicateM n (addRandomAnt b)
                                             
moveAnt :: Board -> Coords -> Coords -> STM Bool
moveAnt b from to = do
  let fromTV = getField b from
  let toTV = getField b to
  fromCont <- readTVar fromTV
  toCont <- readTVar toTV
  if fromCont >= 0 && toCont < 0
    then do
     writeTVar toTV fromCont
     writeTVar fromTV (-1)
     return True
    else return False

moveAntIO :: Board -> Coords -> Coords -> IO Bool
moveAntIO b from to = do
  res <- atomically $ moveAnt b from to
  case res of
   True -> do modifyIORef moveCounter succ
              return True
   False -> return False

type Life = Board -> Coords -> IO ()

blockDelay =   0
blockWait  = 150
minDelay   =  50
maxDelay   = 150

moveAroundRandom :: Int -> Life
moveAroundRandom 0   _ _ = return ()
moveAroundRandom tdl b pos = do
  newPos <- randomNeighbour b pos
  res <- moveAntIO b pos newPos
  case res of
   False -> do threadDelay blockDelay
               moveAroundRandom tdl b pos
   True -> do delay <- (+minDelay) . (`mod` (maxDelay-minDelay)) <$> randomIO 
              threadDelay delay
              moveAroundRandom (tdl-1) b newPos

addLiveAnt :: Board -> Life -> IO (MVar ())
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

showUpdates :: Board -> Integer -> IO ()
showUpdates b totalMoves = do
  initTime <- getClockTime
  go 0 initTime initTime
    where go oldMoves initTime oldTime = do
          system "clear"
          moves <- readIORef moveCounter
          time <- getClockTime
          let diff = time `diffClockTimes` oldTime          
          printf "  %d/%d moves (%.0f%%)\n" moves totalMoves
            ((fromIntegral moves*100 / fromIntegral totalMoves) :: Double)
          printf "      mps: %7.2f\n" (mps (moves-oldMoves) diff)
          printf "  avg mps: %7.2f\n" (mps moves (time `diffClockTimes` initTime))          
--          printBoard b
          threadDelay 100000
          go moves initTime time

validateBoard :: Board -> STM Bool
validateBoard b = do
  ants <- collectAnts b
  nextId <- readTVar (getNextAntId b)
  return $ sort ants == [0..nextId-1]

collectAnts b = do
  ress <- forM [(i,j) | i <- [1..width b], j <- [1..height b]] $ \coords -> do
    val <- readTVar (getField b coords)
    if val >= 0
      then return (Just val)
      else return Nothing
  return $ catMaybes ress

main :: IO ()
main = do
  args <- getArgs
  let contentionFactor =
        case args of
         [] -> 0.2
         [f] -> read f :: Float
  b <- atomically $ emptyBoard 40 40
  let tdl = 10000
  let w = 40
  let h = 40
  mvs <- replicateM (round $ w*h*contentionFactor)
         (addLiveAnt b (moveAroundRandom tdl))
  forkIO $ showUpdates b (round $ w*h*contentionFactor*fromIntegral tdl)
  mapM_ takeMVar mvs
  threadDelay 200000
  atomically (validateBoard b) >>= print
  
