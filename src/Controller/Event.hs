module Controller.Event (
    eventHandler
) where

import Control.Lens
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Model

-- | Event handling


--Returns a world with the rotateAction/movementAction changed to needed value
eventHandler :: Event -> World -> World
eventHandler (EventKey (SpecialKey KeyLeft ) Down _ _)
    = rotateAction .~ RotateLeft
eventHandler (EventKey (SpecialKey KeyLeft ) Up   _ _)
    = rotateAction .~ NoRotation
eventHandler (EventKey (SpecialKey KeyRight) Down _ _)
    = rotateAction .~ RotateRight
eventHandler (EventKey (SpecialKey KeyRight) Up   _ _)
    = rotateAction .~ NoRotation
eventHandler (EventKey (SpecialKey KeyUp   ) Down _ _)
    = movementAction .~ Thrust
eventHandler (EventKey (SpecialKey KeyUp   ) Up   _ _)
    = movementAction .~ NoMovement
eventHandler (EventKey (SpecialKey KeySpace) Down _ _)
    = shootAction .~ Shoot
eventHandler (EventKey (SpecialKey KeySpace) Up   _ _)
    = shootAction .~ DontShoot
eventHandler _
    = id
