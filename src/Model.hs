{-# LANGUAGE TemplateHaskell, DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import Control.Lens
import System.Random

-- | Game state

--Basic data of World, add other datatypes used below
data World = World { _rndGen           :: StdGen
                   , _rotateAction     :: RotateAction
                   , _movementAction   :: MovementAction
                   , _shootAction      :: ShootAction
                   , _player           :: Player
                   } deriving (Show)
    
data RotateAction   = NoRotation | RotateLeft | RotateRight deriving (Show)
data MovementAction = NoMovement | Thrust                   deriving (Show)
data ShootAction    = Shoot      | DontShoot                deriving (Show)

--TODO: Add more datatypes here (player/enemy/asteroid)
data Player = Player { _position :: Point
                     , _lives    :: Int
                     , _score    :: Int
                     , _scoreMul :: Int
                     } deriving (Show)
data Point = Point { _x :: Double
                   , _y :: Double
                   } deriving (Show)

--Add lenses below (must be after defining datatypes)
--(TemplateHaskell can do this automatically with makeLenses,
-- this will make a lens for all _ vars in the datatype )
makeLenses ''World
makeLenses ''Player
makeLenses ''Point

--Returns the starting world of the game based on given seed
initial :: Int -> World
initial seed = World { _rndGen = mkStdGen seed
                     , _rotateAction = NoRotation
                     , _movementAction = NoMovement
                     , _shootAction = DontShoot
                     , _player = newPlayer
                     }
                      
--Returns the starting values for a player/enemy/asteroid
newPlayer :: Player
newPlayer = Player { _position = Point {_x = 0, _y = 0}
                   , _lives    = 0
                   , _score    = 0 
                   , _scoreMul = 1
                   }









