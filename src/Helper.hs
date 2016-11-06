module Helper where

import Control.Lens

import Graphics.Gloss hiding (Point)

import Model

--Class for objects you can collide with
class Collider a where
    --Checks if there is a collision with a Bullet and returns it
    collideWith :: [a] -> Bullet -> Maybe (Bullet, a)
    
--Instance to collide enemies with bullets
--Use enemy size * 1.3 because we use an optimistic hitbox 
--(enemies can be a bit bigger than their size, because of the shape)
instance Collider Enemy where
    collideWith es b | null flist = Nothing
                     | otherwise  = Just (b, head flist)
                     where flist  = filter (\e -> dist e < (e^.enemySize * 1.3)) es
                           dist e = pointDistance (e^.enemyPos) (b^.bulPos)

--Instance to collide bonuses with bullets                          
instance Collider Bonus where
    collideWith bs b | null flist  = Nothing
                     | otherwise   = Just (b, head flist)
                     where flist   = filter (\bo -> dist bo < bonusSize) bs
                           dist bo = pointDistance (bo^.bonusPos) (b^.bulPos)
                           
--Helper function to change our Point to the gloss Vector
toVector :: Point -> Vector
toVector p = (p^.x, p^.y)

--Move a certain amount of pixels to a goal.
moveTo :: Float -> Point -> Point -> Point
moveTo speed goal start = moveDir (pointDirection start goal) speed start

--Move a certain amount of pixels in a given direction
moveDir :: Float -> Float -> Point -> Point
moveDir dir speed start = Point { _x = start^.x + speed * sin dir, _y = start^.y + speed * cos dir }

--Get the distance between two points
pointDistance :: Point -> Point -> Float
pointDistance p1 p2 = sqrt $ (p2^.x - p1^.x) ** 2 + (p2^.y - p1^.y) ** 2

--Get the direction between two points
pointDirection :: Point -> Point -> Float
pointDirection p1 p2 = atan2 (p2^.x - p1^.x) (p2^.y - p1^.y)

--Scale both the horizontal and vertical resolution of a picture
scaleBoth :: Float -> Picture -> Picture
scaleBoth s = scale s s

--Add two points
addPoints :: Point -> Point -> Point
addPoints p1 p2 = Point { _x = p1^.x + p2^.x, _y = p1^.y + p2^.y}

--Map with index
indexedMap :: (a -> Int -> b) -> [a] -> [b]
indexedMap func list = zipWith func list [0 ..]