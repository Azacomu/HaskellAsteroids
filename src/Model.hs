{-# LANGUAGE TemplateHaskell, DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import Control.Lens
import System.Random

import Control.Monad
import Control.Monad.State

-- | Game state

--Basic data of World, add other datatypes used below
data World = World { _gameState        :: GameState
                   , _menu             :: Menu
                   , _rndGen           :: StdGen
                   , _rotateAction     :: RotateAction
                   , _movementAction   :: MovementAction
                   , _shootAction      :: ShootAction
                   , _doesConfirm      :: Bool
                   , _doesSelectPrev   :: Bool
                   , _doesSelectNext   :: Bool
                   , _player           :: Player
                   , _enemies          :: [Enemy]
                   , _passedTime       :: Float
                   , _enemySpawner     :: Spawner
                   , _bullets          :: [Bullet]
                   , _tickTime         :: Float
                   , _bonuses          :: [Bonus]
                   , _bonusSpawner     :: Spawner
                   } deriving (Show)
    
data RotateAction   = NoRotation | RotateLeft | RotateRight deriving (Show, Eq)
data MovementAction = NoMovement | Thrust                   deriving (Show, Eq)
data ShootAction    = Shoot      | DontShoot                deriving (Show, Eq)

data EnemyMovementType = FixedDirection | FollowPlayer      deriving (Show, Eq)
data GameState         = InMenu | InGame                    deriving (Show, Eq)

--TODO: Add more datatypes here (player/enemy/etc.)
data Player = Player { _playerPos   :: Point
                     , _playerSize  :: Float
                     , _playerSpeed :: Float
                     , _playerDir   :: Float
                     , _lives       :: Int
                     , _score       :: Int
                     , _scoreMul    :: Int
                     , _baseShootTime :: Float
                     , _shootTime   :: Float
                     } deriving (Show, Eq)
data Enemy  = Enemy  { _enemyPos  :: Point
                     , _movementType :: EnemyMovementType 
                     , _enemyDir  :: Float
                     , _enemySize :: Float
                     } deriving (Show, Eq)
data Bullet = Bullet { _bulPos    :: Point
                     , _bulSpeed  :: Float
                     , _bulDir    :: Float
                     } deriving (Show, Eq)
data Point  = Point  { _x         :: Float
                     , _y         :: Float
                     } deriving (Show, Eq)
data Menu   = Menu   { _selectionOption :: Int
                     } deriving (Show, Eq)
data Bonus  = Bonus  { _bonusPos  :: Point
                     } deriving (Show, Eq)

-- Contains data needed for spawning things
-- only the time to next at the moment, but this could include much more
-- (such as the enemy type, patterns, etc.)
data Spawner = Spawner { _timeToNext :: Float
                       , _interval   :: Float } deriving (Show)

--Add lenses below (must be after defining datatypes)
--(TemplateHaskell can do this automatically with makeLenses,
-- this will make a lens for all _ vars in the datatype )
makeLenses ''World
makeLenses ''Player
makeLenses ''Enemy
makeLenses ''Point
makeLenses ''Spawner
makeLenses ''Bullet
makeLenses ''Menu
makeLenses ''Bonus

--Returns the starting world of the game based on given seed
initial :: Int -> World
initial seed = World { _gameState      = InMenu
                     , _menu           = newMenu
                     , _rndGen         = mkStdGen seed
                     , _rotateAction   = NoRotation
                     , _movementAction = NoMovement
                     , _shootAction    = DontShoot
                     , _doesConfirm    = False
                     , _doesSelectPrev = False
                     , _doesSelectNext = False
                     , _player         = newPlayer
                     , _enemies        = []
                     , _passedTime     = 0
                     , _enemySpawner   = newSpawner 60
                     , _bonusSpawner   = newSpawner 240
                     , _bullets        = []
                     , _tickTime       = 0
                     , _bonuses        = []
                     }
                      
--Returns the starting values for a player
newPlayer :: Player
newPlayer = Player { _playerPos     = Point {_x = 0, _y = 0}
                   , _playerSize    = 10
                   , _playerSpeed   = 5
                   , _playerDir     = 0
                   , _lives         = 3
                   , _score         = 0 
                   , _scoreMul      = 1
                   , _baseShootTime = 10
                   , _shootTime     = 0
                   }
                   
--Returns a new enemy at a given (random) point, moving in a given dir
newEnemy :: Point -> Float -> Enemy
newEnemy p d = Enemy { _enemyPos     = p
                     , _enemySize    = 5
                     , _movementType = FixedDirection
                     , _enemyDir     = d
                     }

newFollowingEnemy :: Point -> Enemy
newFollowingEnemy p = set movementType
                          FollowPlayer
                          $ newEnemy p 0

newSpawner :: Float -> Spawner
newSpawner interval = Spawner { _timeToNext = 0
                              , _interval   = interval }

newBonus :: Point -> Bonus
newBonus position = Bonus { _bonusPos = position }

--Returns a new Bullet with given position and direction                               
newBullet :: Point -> Float -> Bullet
newBullet p d = Bullet { _bulPos   = p
                       , _bulSpeed = 10
                       , _bulDir   = d
                       }

newMenu :: Menu
newMenu = Menu { _selectionOption = 0 }