-- |
-- Module      :  CrossedWires
-- Description :  Day 3 - Find closest intersection point to the central port
-- Copyright   :  Copyright Alexander DuPree (c) 2019
-- Maintainer  :  Alexander DuPree
-- Stability   :  experimental
-- Portability :  POSIX

module CrossedWires
  ( Point2D
  , Line
  , Movement(..)
  , add
  , minus
  , dot
  , norm
  , rectDist
  , origin
  , update
  , createPath
  , findIntersections
  , minDistance
  , intersectLine
  , intersectSegment
  , calcMinDistance
  , calcMinSteps
  , calcSteps
  , projection
  )
where

import           Control.Monad                  ( foldM )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set


type Point2D = (Float, Float)
type Line    = (Point2D, Point2D)

data Movement = U Float | D Float | L Float | R Float
    deriving (Show, Eq )

origin = (0, 0) :: Point2D

-- Rectilinear Distance often called Manahattan Distance
rectDist :: Point2D -> Point2D -> Float
rectDist (x1, y1) (x2, y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

{- Vector Operations -}
add :: Point2D -> Point2D -> Point2D
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

minus :: Point2D -> Point2D -> Point2D
minus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

dot :: Point2D -> Point2D -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

norm :: Point2D -> Float
norm p1 = sqrt $ dot p1 p1

det :: Num a => [a] -> a
det [a, b, c, d] = a * d - b * c

delta :: (Point2D -> Float) -> Line -> Float
delta f line = f (fst line) - f (snd line)

magnitude :: Line -> Float
magnitude (p1, p2) = norm (p2 `minus` p1)

intersectLine :: Line -> Line -> Maybe Point2D
intersectLine ab pq = case denom of
  0 -> Nothing -- Parallel Lines
  _ -> Just (ix, iy)
 where
    -- Difference Matrix
  [abDx, pqDx, abDy, pqDy] = [delta fst, delta snd] <*> [ab, pq]
    -- Determinant of line AB, Determinant of line PQ
  [abD, pqD] = (\(a, b) -> det ([fst, snd] <*> [a, b])) <$> [ab, pq]
    -- Determinant of Difference Matrix
  denom                    = det [abDx, abDy, pqDx, pqDy]
    -- intersection X value
  ix                       = det [abD, abDx, pqD, pqDx] / denom
    -- intersection Y value
  iy                       = det [abD, abDy, pqD, pqDy] / denom

intersectSegment :: Line -> Line -> Maybe Point2D
intersectSegment ab pq
  | Just point <- intersectLine ab pq
  -- Check if point exists in line segment interval
  , w1 <- projection ab point
  , w2 <- projection pq point
  , w1 >= 0 && w1 <= 1
  , w2 >= 0 && w2 <= 1
  = Just point
  | otherwise
  = Nothing

-- Returns the location relative to p1, p2 of the projection of p3 onto the line
-- I.E. projection (origin, (0,4)) (0, 2) == 0.5  since (0,2) is halfway between the segment
projection :: Line -> Point2D -> Float
projection (p1, p2) p3 =
  (p3 `minus` p1) `dot` (p2 `minus` p1) / (p2 `minus` p1) `dot` (p2 `minus` p1)

update :: Point2D -> Movement -> Point2D
update (x, y) movement = case movement of
  U n -> (x, y + n)
  D n -> (x, y - n)
  L n -> (x - n, y)
  R n -> (x + n, y)

-- Interprets a list of movements into a list of line segments
createPath :: [Movement] -> [Line]
createPath moves = pair $ foldl updatePath [origin] moves
 where
  updatePath path move = update (head path) move : path
  pair points = zip points (tail points)

-- TODO We use filter O(N) to get rid of one element, optimize
findIntersections :: [Line] -> [Line] -> Set Point2D
findIntersections p1 p2 = Set.filter (\x -> x /= origin) $ Set.fromList $ foldr
  intersection'
  []
  paths
 where
  paths = [ (a, b) | a <- p1, b <- p2 ]
  intersection' (l1, l2) points = case intersectSegment l1 l2 of
    Nothing    -> points
    Just point -> point : points

minDistance :: Point2D -> Set Point2D -> Float
minDistance point path = Set.findMin $ Set.map (rectDist point) path

calcMinDistance :: [Movement] -> [Movement] -> Float
calcMinDistance p1 p2 =
  minDistance origin $ (createPath p1) `findIntersections` (createPath p2)

calcMinSteps :: [Movement] -> [Movement] -> Float
calcMinSteps p1 p2 = Set.findMin $ Set.map (addSteps) intersections
 where
  [path1, path2] = [createPath] <*> [p1, p2]
  intersections  = path1 `findIntersections` path2
  addSteps point = (calcSteps point path1) + (calcSteps point path2)

-- TODO: This function is has horrible operation complexity
calcSteps :: Point2D -> [Line] -> Float
calcSteps point path = calcSteps' point (reverse path)
 where
  calcSteps' point ((p1, p2) : lines) = case isBetween (p1, p2) point of
    True  -> (projection (p2, p1) point) * (rectDist p1 p2)
    False -> (rectDist p1 p2) + (calcSteps' point lines)

isBetween :: Line -> Point2D -> Bool
isBetween (a, b) c
  | (magnitude (a, c)) + (magnitude (c, b)) == (magnitude (a, b)) = True
  | otherwise = False
