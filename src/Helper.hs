module Helper where

import Control.Lens

import Graphics.Gloss hiding (Point)

import Model

--Helper function to change our Point to the gloss Vector
toVector :: Point -> Vector
toVector p = (p^.x, p^.y)

-- Move a certain amount of pixels to a goal.
moveTo :: Float -> Point -> Point -> Point
moveTo speed goal start = moveDir (pointDirection start goal) speed start

-- Move a certain amount of pixels in a given direction
moveDir :: Float -> Float -> Point -> Point
moveDir dir speed start = Point { _x = start^.x + speed * sin dir, _y = start^.y + speed * cos dir }

-- Get the distance between two points
pointDistance :: Point -> Point -> Float
pointDistance p1 p2 = sqrt $ (p2^.x - p1^.x) ** 2 + (p2^.y - p1^.y) ** 2

-- Get the direction between two points
pointDirection :: Point -> Point -> Float
pointDirection p1 p2 = atan2 (p2^.x - p1^.x) (p2^.y - p1^.y)

-- Scale both the horizontal and vertical resolution of a picture
scaleBoth :: Float -> Picture -> Picture
scaleBoth s = scale s s

-- Add two points
addPoints :: Point -> Point -> Point
addPoints p1 p2 = Point { _x = p1^.x + p2^.x, _y = p1^.y + p2^.y}

