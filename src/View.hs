{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Geometry.Angle
import Data.Monoid


import Control.Lens

import Helper
import Model

import MenuView

-- | Drawing

--This is where we convert all different elements in the passed world to a Picture
--Important uses: http://hackage.haskell.org/package/gloss-1.8.1.2/docs/Graphics-Gloss-Data-Picture.html#t:Picture
draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution world
     = if world^.gameState == InMenu then
          drawMenu horizontalResolution verticalResolution world
      else
          drawPlayer (world^.player)
          <> drawEnemies world
          <> drawBullets world

--Returns a circle around given point, in given color, with given radius
drawCircle :: Point -> Color -> Float -> Picture
drawCircle p c r = translate (p^.x) (p^.y) (color c (circle r))

--Returns a standard circle around given point, useful for testing
drawStdCircle :: Point -> Picture
drawStdCircle p = drawCircle p white 5

drawEnemies :: World -> Picture
drawEnemies world = pictures $ map drawEnemy (world^.enemies)
                  where drawEnemy enemy = drawCircle (enemy^.enemyPos) red 20

--Returns a picture used to draw the player                  
drawPlayer :: Player -> Picture
drawPlayer player = drawCircle (player^.playerPos) blue 20
                    <> drawCircle (moveDir (player^.playerDir) 7 (player^.playerPos)) green 5
                    <> drawScore (player^.score)
                    
--Draws all bullets as small lines
drawBullets :: World -> Picture
drawBullets world = pictures $ map drawBullet (world^.bullets)
                  where drawBullet b = color green $ line $ path b
                        path b = [toVector $ b^.bulPos, toVector $ moveDir (b^.bulDir) (-8) (b^.bulPos)]

--Draws score on the screen (Temp, must be improved)                        
drawScore :: Int -> Picture
drawScore x = Color white (text $ show x)
                  
                  
                  
                  
                  