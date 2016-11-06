module MenuView where

import Graphics.Gloss hiding (Point)
import Data.Monoid

import Control.Lens
import Helper
import Controller.MenuUpdate

import Model

-- Constant for the text base height
-- This is the height at which unscaled text is normally drawn
textBaseH :: Num a => a
textBaseH = 110

-- Draw the menu using the horizontal and vertical resolution
drawMenu :: Float -> Float -> World -> Picture
drawMenu horizontalResolution verticalResolution world
    = -- Make sure everything is drawn at the right position and with the right
      -- color    
      translate left top $ scaleBoth worldScale $ color white $
             -- Draw the title
             translate 10
                       (textBaseH * (-0.5) - 10)
                       (scaleBoth 0.5 $ text getTitle)
             -- Draw the subtitle
          <> translate 10
                       (textBaseH * (-0.7) - 20)
                       (scaleBoth 0.2 $ text getSubTitle)
             -- Draw the options
          <> drawOptions
    where -- The scale of the display
          worldScale  = verticalResolution / 576
          -- The top and left positions on the screen
          top         = verticalResolution / 2
          left        = horizontalResolution / (-2)
          -- Draw the menu options
          drawOptions = foldl (<>) blank $ indexedMap (drawMenuOption world) $
                                                      menuOptions isDieMenu
          -- Get if this is the death menu
          isDieMenu   = world^.menu.hasDiedBefore
          -- Get the title of the menu
          getTitle    | isDieMenu = "You died!"
                      | otherwise = "Hasteroids"
          -- Get the subtitle of the menu
          getSubTitle --Scores 
                      | isDieMenu = "Score: " ++ show (world^.player.score) ++
                                        "; " ++
                                        if world^.isNewHighscore then
                                            "new highscore!"
                                        else
                                            "highscore: " ++
                                                show (world^.highscore)
                      -- Credits
                      | otherwise = "By Martin Boers and Florian van Strien"
                       
-- Draw a single option of the menu 
-- Uses the world, the text of the option and the index
-- Returns a picture with the drawn menu option
drawMenuOption :: World -> String -> Int -> Picture
drawMenuOption world option i
    = translate 10 transY $ scaleBoth 0.3 $ text optionText
    where transY     = -35 - 15 * i' - textBaseH * (1 + 0.3 * i')
          i'         = fromIntegral i
          optionText | i == world^.menu.selectionOption = '>' : option
                     | otherwise                        =       option