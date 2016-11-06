module View (draw) where

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
          <> drawEnemies world
          <> drawBullets world
          <> drawPlayer  (world^.player)
          <> drawGUI horizontalResolution verticalResolution (world^.player)
          <> drawBonuses world
          <> drawHighscore horizontalResolution verticalResolution (world^.highscore)

--Returns a circle around given point, in given color, with given radius
drawCircle :: Point -> Color -> Float -> Picture
drawCircle p c r = translate (p^.x) (p^.y) (color c (circle r))

drawCircleSolid :: Point -> Color -> Float -> Picture
drawCircleSolid p c r = translate (p^.x) (p^.y) (color c (circleSolid r))

drawEnemies :: World -> Picture
drawEnemies world = pictures $ map drawEnemy (world^.enemies)
                  where drawEnemy e = translate (e^.enemyPos.x) (e^.enemyPos.y) $ e^.enemyPicture

--Returns a picture used to draw the player
drawPlayer :: Player -> Picture
drawPlayer plr | plr^.lives > 0 = trans $ rotate (radToDeg $ plr^.playerDir) $ modelPlayer alpha
               | otherwise      = blank
               where trans = translate (plr^.playerPos^.x) (plr^.playerPos^.y)
                     alpha | plr^.invincibleTime > 0 = 0.3
                           | otherwise               = 1
                    
--Draws all bullets as small lines
drawBullets :: World -> Picture
drawBullets world = pictures $ map drawBullet (world^.bullets)
                  where drawBullet b = color green $ line $ path b
                        path b       = [toVector $ b^.bulPos, toVector $ moveDir (b^.bulDir) (-8) (b^.bulPos)]

drawBonuses :: World -> Picture
drawBonuses world = pictures $ map drawBonus (world^.bonuses)
                  where drawBonus bonus = drawCircle (bonus^.bonusPos) yellow bonusSize
                  
--Draws the current highscore on the screen
drawHighscore :: Float -> Float -> Int -> Picture
drawHighscore hres vres n = translate (0.5 * hres - 200) (0.5 * vres - 25) $
                            scaleBoth 0.2 $ color white (text $ "Highscore: " ++ show n)

--Draws score on the screen                        
drawScore :: Float -> Float -> Int -> Picture
drawScore hres vres n = translate (-0.5 * hres) (0.5 * vres - 25) $
                        scaleBoth 0.2 $ color white (text $ "Score: " ++ show n)

--Draws the multiplier on the screen
drawMultiplier :: Float -> Float -> Int -> Picture
drawMultiplier hres vres n = translate (-0.1 * hres) (0.5 * vres - 25) $
                             scaleBoth 0.2 $ color blue (text $ "Multiplier: X" ++ show n ++ "!")

--Draws the life as a number of small playerModels on the screen
drawLives :: Float -> Float -> Int -> Picture
drawLives _ _ 0       = blank
drawLives hres vres n = pictures $ drawLives hres vres (n-1) : [picture]
                        where picture = translate (fromIntegral $ n * 30) 0 (scaleBoth 0.5 $ modelPlayer 1)

--Draws all in-game GUI elements
drawGUI :: Float -> Float -> Player -> Picture
drawGUI h v plr =    drawScore      h v (plr^.score)
                  <> drawMultiplier h v (plr^.scoreMul)
                  <> translate (-0.5 * h) (0.5 * v - 50) (drawLives h v (plr^.lives))

--Constant for the picture/graphic used to represent the player (and the remaining lives) 
modelPlayer :: Float -> Picture
modelPlayer alpha = pictures [ color col1 (line [(-16,-20), (0,20), (16,-20)]) 
                             , color col1 (line [(-12,-10), (12,-10)])
                             , drawCircle Point {_x = 0, _y = 5} col2 3 ]
                  where col1 = withAlpha alpha white
                        col2 = withAlpha alpha green
                  
drawStars :: World -> Picture
drawStars world = pictures $ map drawStar (world^.stars)
                where drawStar star = drawCircleSolid (star^.starPos) (makeColor 1 1 1 $ star^.starSpeed / 7) (star^.starSpeed)
                
drawParticles :: World -> Picture
drawParticles world = pictures $ map drawPart (world^.particles)
                    where drawPart part = drawCircleSolid (part^.partPos) (part^.partCol) (part^.partSize)