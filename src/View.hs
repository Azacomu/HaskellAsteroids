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
draw :: Float -> Float -> World -> IO Picture
draw horizontalResolution verticalResolution world
     = return $ drawStars world <>
       if world^.gameState == InMenu then
          drawMenu horizontalResolution verticalResolution world
       else
          drawParticles  world
          <> drawPlayer  (world^.player)
          <> drawEnemies world
          <> drawBullets world
          <> drawGUI horizontalResolution verticalResolution (world^.player)
          <> drawBonuses world
          <> drawHighscore horizontalResolution verticalResolution (world^.highscore)

--Returns a circle around given point, in given color, with given radius
drawCircle :: Point -> Color -> Float -> Picture
drawCircle p c r = translate (p^.x) (p^.y) (color c (circle r))

drawCircleSolid :: Point -> Color -> Float -> Picture
drawCircleSolid p c r = translate (p^.x) (p^.y) (color c (circleSolid r))

--Returns a standard circle around given point, useful for testing
drawStdCircle :: Point -> Picture
drawStdCircle p = drawCircle p white 5

drawEnemies :: World -> Picture
drawEnemies world = pictures $ map drawEnemy (world^.enemies)
                  where drawEnemy enemy = translate (enemy^.enemyPos.x) (enemy^.enemyPos.y) $ enemy^.enemyPicture--drawCircle (enemy^.enemyPos) red (enemy^.enemySize)

--Returns a picture used to draw the player                  
drawPlayer :: Player -> Picture
drawPlayer player = translate (player^.playerPos^.x) (player^.playerPos^.y) (rotate (radToDeg $ player^.playerDir) modelPlayer)

-- drawCircle (player^.playerPos) blue (player^.playerSize)
-- <> drawCircle (moveDir (player^.playerDir) 7 (player^.playerPos)) green 5
                    
--Draws all bullets as small lines
drawBullets :: World -> Picture
drawBullets world = pictures $ map drawBullet (world^.bullets)
                  where drawBullet b = color green $ line $ path b
                        path b = [toVector $ b^.bulPos, toVector $ moveDir (b^.bulDir) (-8) (b^.bulPos)]

drawBonuses :: World -> Picture
drawBonuses world = pictures $ map drawBonus (world^.bonuses)
                  where drawBonus bonus = drawCircle (bonus^.bonusPos) yellow bonusSize
                  
--Draws the current highscore on the screen
drawHighscore :: Float -> Float -> Int -> Picture
drawHighscore hres vres x = translate (0.5 * hres - 200) (0.5 * vres - 25) (scale 0.2 0.2  (color white (text $ "Highscore: " ++ show x)))

--Draws score on the screen (Temp, must be improved)                        
drawScore :: Float -> Float -> Int -> Picture
drawScore hres vres x = translate (-0.5 * hres) (0.5 * vres - 25) (scale 0.2 0.2  (color white (text $ "Score: " ++ show x)))

--Draws the multiplier on the screen
drawMultiplier :: Float -> Float -> Int -> Picture
drawMultiplier hres vres x = translate (-0.1 * hres) (0.5 * vres - 25) (scale 0.2 0.2  (color blue (text $ "Multiplier: X" ++ show x ++ "!")))

--Draws the life as a number of small playerModels on the screen
drawLives :: Float -> Float -> Int -> Picture
drawLives _ _ 0       = blank
drawLives hres vres x = pictures (drawLives hres vres (x-1) : [translate (fromIntegral $ x * 30) 0 (scale 0.5 0.5 modelPlayer)])

--Draws all in-game GUI elements
drawGUI :: Float -> Float -> Player -> Picture
drawGUI h v player =    drawScore      h v (player^.score)
                     <> drawMultiplier h v (player^.scoreMul)
                     <> translate (-0.5 * h) (0.5 * v - 50) (drawLives h v (player^.lives))

--Constant for the picture/graphic used to represent the player (and the remaining lives)
modelPlayer :: Picture
modelPlayer = pictures [ color white (line [(-16,-20), (0,20), (16,-20)]) 
                       , color white (line [(-12,-10), (12,-10)])
                       , drawCircle Point {_x = 0, _y = 5} green 3 ]
                  
drawStars :: World -> Picture
drawStars world = pictures $ map drawStar (world^.stars)
                where drawStar star = drawCircleSolid (star^.starPos) (makeColor 1 1 1 $ star^.starSpeed / 7) (star^.starSpeed)
                
drawParticles :: World -> Picture
drawParticles world = pictures $ map drawPart (world^.particles)
                    where drawPart part = drawCircleSolid (part^.partPos) yellow (part^.partSize)