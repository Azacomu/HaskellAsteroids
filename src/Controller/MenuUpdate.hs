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

menuOptions :: [String]
menuOptions = ["Play", "Quit"]

updateMenu :: MonadState World m => m ()
updateMenu = do selectsPrev <- use doesSelectPrev
                when selectsPrev $ do
                    menu.selectionOption %= (\o -> if o > 0 then o - 1 else o)
                selectsNext <- use doesSelectNext
                when selectsNext $ do
                    menu.selectionOption %= (\o -> if o + 1 < (length menuOptions) then o + 1 else o)
                confirms    <- use doesConfirm
                nowSelected <- use $ menu.selectionOption
                when confirms $ do
                    if nowSelected == 0 then
                        gameState .= InGame
                    else when (nowSelected == 1) $ do
                        unsafePerformIO exitSuccess