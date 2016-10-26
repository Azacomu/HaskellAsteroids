{-# LANGUAGE TemplateHaskell, DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import Control.Lens
import System.Random

-- | Game state

--Basic data of World, add other datatypes used below
data World = World {
        -- Random generator
        _rndGen           :: StdGen,
        -- Event queue
        _rotateAction     :: RotateAction,
        _movementAction   :: MovementAction,
        _shootAction      :: ShootAction
        -- TODO: add more fields here!
    }deriving (Show)
    
data RotateAction   = NoRotation | RotateLeft | RotateRight deriving (Show)
data MovementAction = NoMovement | Thrust                   deriving (Show)
data ShootAction    = Shoot      | DontShoot                deriving (Show)

--TODO: Add more datatypes here (player/enemy/asteroid)
data Player = Player {_position :: Point
                     } deriving (Show)
data Point = Point {_x :: Double,
                    _y :: Double
                    } deriving (Show)

--Add lenses below (must be after defining datatypes)
--(TemplateHaskell can do this automatically with makeLenses,
-- this will make a lens for all _ vars in the datatype )
makeLenses ''World
makeLenses ''Player
makeLenses ''Point

--Returns the starting world of the game based on given seed
initial :: Int -> World
initial seed = World {}
