{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp, FlexibleContexts                              #-}

module Controller.MenuUpdate where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad
import Control.Monad.State

import Data.List

import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Geometry.Angle

import System.Random

import System.IO.Unsafe
import System.Exit

import Model

-- Menu options for the die menu or the normal one
-- (True if just died)
menuOptions :: Bool -> [String]
menuOptions True  = ["Play Again", "Quit"]
menuOptions False = ["Play", "Quit"]

updateMenu :: MonadState World m => m ()
updateMenu = do hasJustDied <- use $ menu.hasDiedBefore
                selectsPrev <- use doesSelectPrev
                when selectsPrev $
                    menu.selectionOption %= (\o -> if o > 0 then o - 1 else o)
                selectsNext <- use doesSelectNext
                when selectsNext $
                    menu.selectionOption %= (\o -> if o + 1 < (length (menuOptions hasJustDied)) then o + 1 else o)
                confirms    <- use doesConfirm
                nowSelected <- use $ menu.selectionOption
                when confirms $
                    if nowSelected == 0 then do
                        player.score .= 0
                        gameState    .= InGame
                    else when (nowSelected == 1) $
                        unsafePerformIO exitSuccess