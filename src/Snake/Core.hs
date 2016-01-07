-- |
-- Module      : Snake.Core
-- Description :
-- Copyright   : (c) Jonatan H Sundqvist, 2016
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental|stable
-- Portability : POSIX (not sure)
--

-- Created January 2 2016

-- TODO | -
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE TupleSections #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Snake.Core where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Maybe (catMaybes)

import Linear.V2
import Linear.V3

import Cartesian.Plane.Types
import Cartesian.Plane (magnitude, intersect, dotmap, to3D)
import Cartesian.Space.Types


--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- | Constructs a snake body from a list of angles (radians) and lengths and a starting point. Each angle is relative to the previous segment,
--   except for the first angle which is relative to the positive X-axis.
--   In other words, it takes some wiggles and some stretches and it makes a serpent.
-- TODO: Use scan_ (?)
-- TODO: Refactor
-- TODO: Better description
fromAngles :: (Floating f, Eq f) => Vector2D f -> [(f, f)] -> [Vector2D f]
fromAngles begin []     = begin : []
fromAngles begin angles = map snd $ scanl curve (0, begin) angles
  where
    curve (α, p) (θ, r) = let next = p + fromPolar (α + θ) r in (argument $ next - p, next)
    makePolar v         = (magnitude v, argument v)
    fromPolar arg mag   = Vector2D (mag*cos arg) (mag*sin arg)


-- |
intersects :: RealFloat f => Line (Vector2D f) -> Line (Vector2D f) -> Bool
intersects a b = maybe False (const True) (intersect a b)


-- |
-- TODO: Refactor
collisions :: RealFloat f => [Line (Vector2D f)] -> [Vector2D f]
collisions segments = catMaybes $ map (uncurry intersect) (pairs segments)
  where
    pairs []     = []
    pairs [a]    = []
    pairs [a, b] = [(a, b)]
    pairs (a:as) = map (a, ) as ++ pairs as

--------------------------------------------------------------------------------------------------------------------------------------------

-- | Connects the dots. So to speak.
connect :: RealFloat f => [Vector2D f] -> [Line (Vector2D f)]
connect = pairwise Line


-- |
pairwise :: (a -> a -> b) -> [a] -> [b]
pairwise f xs = zipWith f xs (drop 1 xs)

-- Vector operations -----------------------------------------------------------------------------------------------------------------------

-- |
toList :: (Vector v, Num f) => v f -> [f]
toList =  reverse . vfold (flip (:)) []


-- | Angle (in radians) between the positive X-axis and the vector
argument :: (Floating f, Eq f) => Vector2D f -> f
argument (Vector2D 0 0) = 0
argument (Vector2D 0 y) = asin $ y/abs y
argument (Vector2D x y) = atan $ y/x


-- |
from2D :: (Real a, Fractional b) => Vector2D a -> V3 b
from2D = toV3 . to3D . dotmap realToFrac


-- |
toV3 :: Vector3D n -> V3 n
toV3 (Vector3D x' y' z') = V3 x' y' z'


-- |
neg :: Num n => V3 n -> V3 n
neg (V3 x' y' z') = V3 (-x') (-y') (-z')


-- |
mult :: Num n => Vector2D n -> Vector2D n -> Vector2D n
mult (Vector2D ax ay) (Vector2D bx by) = Vector2D (ax*bx+ay*by) (ax*by + ay*bx)
