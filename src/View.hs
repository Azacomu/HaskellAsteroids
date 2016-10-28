{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Geometry.Angle

import Control.Lens

import Model

-- | Drawing

--This is where we convert all different elements in the passed world to a Picture
--Important uses: http://hackage.haskell.org/package/gloss-1.8.1.2/docs/Graphics-Gloss-Data-Picture.html#t:Picture
draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution world
    = drawCircle (world^.player.playerPos) blue 20
    
--Returns a circle around given point, in given color, with given radius
drawCircle :: Point -> Color -> Float -> Picture
drawCircle p c r = translate (p^.x) (p^.y) (color c (circle r))

--Returns a standard circle around given point, useful for testing
drawStdCircle :: Point -> Picture
drawStdCircle p = drawCircle p white 5




