{-# LANGUAGE TupleSections #-}

module Lib
    ( solve
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.List as List
import qualified Control.Monad.State.Strict as S
import qualified ListT as ListT
import Data.Maybe (maybeToList)
import Control.Monad.Loops (iterateUntilM)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex = (flip List.zipWith) [0..]

mapWithIndex2d :: (Int -> Int -> a -> b) -> [[a]] -> [[b]]
mapWithIndex2d f = mapWithIndex ((mapWithIndex .) f)

from2dList :: [[a]] -> M.Map (Int, Int) a
from2dList = M.fromList . concat . mapWithIndex2d (\r c -> ((r,c),))

-- Laser's direction is described by how it will be translated
-- on the next step, assuming (row, col) coordinates.
-- For example, moving "north" is (-1, 0).
data LaserDir = LaserDir Int Int deriving (Eq, Show)

stepLaserPos (r,c) (LaserDir dr dc)  = (r+dr, c+dc)

-- Where the laser is and is pointing at the current step.
data LaserState = LaserState
  { pos :: (Int, Int)
  , dir :: LaserDir
  } deriving (Eq, Show)

type Chamber = M.Map (Int, Int) Char
type ChamberState = M.Map (Int, Int) [LaserDir]

-- Provides the next directions for a laser given the tile it will hit.
nextDirs :: Char -> LaserDir -> [LaserDir]
nextDirs '.'  d                 = [d]
nextDirs '/'  (LaserDir dr dc)  = [LaserDir (-dc) (-dr)]
nextDirs '\\' (LaserDir dr dc)  = [LaserDir dc dr]
nextDirs '|'  d@(LaserDir _ dc) = if dc == 0 then [d] else [LaserDir 1 0, LaserDir (-1) 0]
nextDirs '-'  d@(LaserDir dr _) = if dr == 0 then [d] else [LaserDir 0 1, LaserDir 0 (-1)]

-- Performs a single step of every active laser.
step :: Chamber -> [LaserState] -> S.State ChamberState [LaserState]
step chamber lasers = ListT.toReverseList $ do
    LaserState{pos=currPos, dir=currDir} <- ListT.fromFoldable lasers
    chamberState <- S.get
    let visitedDirs = M.findWithDefault [] currPos chamberState

    if List.elem currDir visitedDirs
        then mempty -- Exits early
        else return ()

    tile <- ListT.fromFoldable $ maybeToList $ M.lookup currPos chamber

    S.modify' $ M.insert currPos (currDir:visitedDirs)

    nextDir <- ListT.fromFoldable $ nextDirs tile currDir

    return $ LaserState {pos = stepLaserPos currPos nextDir, dir = nextDir}

-- Iterates `step` until all lasers have settled.
settledLaserPositions :: Chamber -> LaserState -> ChamberState
settledLaserPositions chamber laser = S.execState settledState M.empty
  where settledState = iterateUntilM List.null (step chamber) [laser]

energizedTileCount :: Chamber -> LaserState -> Int
energizedTileCount = (M.size .) . settledLaserPositions

allInitialLasers :: Chamber -> [LaserState]
allInitialLasers chamber = left ++ right ++ top ++ bottom
    where
        (maxRow, maxCol) = List.last $ M.keys chamber
        left   = map (\r -> LaserState (r,     0) (LaserDir 0 1))    [0..maxRow]
        right  = map (\r -> LaserState (r,maxCol) (LaserDir 0 (-1))) [0..maxRow]
        top    = map (\c -> LaserState (0,     c) (LaserDir 1 0))    [0..maxCol]
        bottom = map (\c -> LaserState (maxCol,c) (LaserDir (-1) 0)) [0..maxCol]

parse :: String -> Chamber
parse = from2dList . lines

solve :: IO ()
solve = do
    contents <- readFile "input.txt"
    let chamber = parse contents
    putStrLn $ show $ List.maximum $ map (energizedTileCount chamber) $ allInitialLasers chamber
