{-# LANGUAGE TemplateHaskell, DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import Control.Lens
import System.Random

import Config
import Graphics.Gloss hiding (Point)

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
                   , _stars            :: [Star]
                   , _particles        :: [Particle]
                   , _highscore        :: Int
                   , _isHighSet        :: Bool
                   , _isNewHighscore   :: Bool
                   , _endTimer         :: Float
                   } deriving (Show)
    
data RotateAction   = NoRotation | RotateLeft | RotateRight  deriving (Show, Eq)
data MovementAction = NoMovement | Thrust     | BackThrust   deriving (Show, Eq)
data ShootAction    = Shoot      | DontShoot                 deriving (Show, Eq)

data EnemyMovementType = FixedDirection | FollowPlayer       deriving (Show, Eq)
data Side              = North  | South | West | East | None deriving (Show, Eq)
data GameState         = InMenu | InGame                     deriving (Show, Eq)
data BonusType         = ExtraMultiplier                     deriving (Show, Eq)

--TODO: Add more datatypes here (player/enemy/etc.)
data Player = Player { _playerPos      :: Point
                     , _playerSize     :: Float
                     , _playerSpeed    :: Float
                     , _playerDir      :: Float
                     , _lives          :: Int
                     , _score          :: Int
                     , _scoreMul       :: Int
                     , _baseShootTime  :: Float
                     , _shootTime      :: Float
                     , _invincibleTime :: Float
                     } deriving (Show, Eq)
data Enemy  = Enemy  { _enemyPos  :: Point
                     , _movementType :: EnemyMovementType 
                     , _enemyDir     :: Float
                     , _enemySize    :: Float
                     , _enemyEdges   :: [Point]
                     , _enemyPicture :: Picture
                     , _enemySpeed   :: Float
                     } deriving (Show, Eq)
data Bullet = Bullet { _bulPos    :: Point
                     , _bulSpeed  :: Float
                     , _bulDir    :: Float
                     , _bulTime   :: Float
                     } deriving (Show, Eq)
data Point  = Point  { _x         :: Float
                     , _y         :: Float
                     } deriving (Show, Eq)
data Menu   = Menu   { _selectionOption :: Int
                     , _hasDiedBefore   :: Bool
                     } deriving (Show, Eq)
data Bonus  = Bonus  { _bonusPos  :: Point
                     } deriving (Show, Eq)
data Star   = Star   { _starPos   :: Point
                     , _starSpeed :: Float
                     } deriving (Show, Eq)
data Particle = Particle { _partPos  :: Point
                         , _partSize :: Float
                         , _partCol  :: Color}
                         deriving (Show, Eq)
                     
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
makeLenses ''Star
makeLenses ''Particle

--Several combined lenses for simplicity
allBullets   :: Traversal' World Bullet
allEnemies   :: Traversal' World Enemy
allParticles :: Traversal' World Particle
allBullets   = bullets.traversed
allEnemies   = enemies.traversed
allParticles = particles.traversed

--Constants for the size of the screen
screenWidth  :: Float
screenHeight :: Float
screenWidth  = defaultHorizontalResolution
screenHeight = defaultVerticalResolution

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
                     , _enemySpawner   = newSpawner 90
                     , _bonusSpawner   = newSpawner 320
                     , _bullets        = []
                     , _tickTime       = 0
                     , _bonuses        = []
                     , _stars          = []
                     , _particles      = []
                     , _highscore      = 0
                     , _isHighSet      = False
                     , _isNewHighscore = False
                     , _endTimer       = 0
                     }
                      
--Returns the starting values for a player
newPlayer :: Player
newPlayer = Player { _playerPos      = newPoint 0 0
                   , _playerSize     = 20
                   , _playerSpeed    = 5
                   , _playerDir      = 0
                   , _lives          = 3
                   , _score          = 0 
                   , _scoreMul       = 1
                   , _baseShootTime  = 10
                   , _shootTime      = 0
                   , _invincibleTime = 0
                   }
                   
--Returns a new point with given x and y values
newPoint :: Float -> Float -> Point
newPoint px py = Point { _x = px, _y = py}

-- How much invincible time you have after colliding
invincibleTimeAfterCollision :: Float
invincibleTimeAfterCollision = 60
            
--Returns a new enemy at a given (random) point, moving in a given dir with given picture and size
--edgePoints are the points that make up the shape of the enemy
newEnemy :: Point -> Float -> [Point] -> Float -> Float -> Enemy
newEnemy p d edgePoints size speed
    = Enemy { _enemyPos     = p
            , _enemySize    = size
            , _movementType = FixedDirection
            , _enemyDir     = d
            , _enemyEdges   = edgePoints 
            , _enemyPicture = getEnemyPic edgePoints
            , _enemySpeed   = speed
            }

getEnemyPic :: [Point] -> Picture
getEnemyPic points = color red $ lineLoop $ map (\p -> (p^.x, p^.y)) points

followingEnemySpeed :: Float
followingEnemySpeed = 3

newFollowingEnemy :: Point -> [Point] -> Float -> Enemy
newFollowingEnemy p pnts size = set movementType FollowPlayer
                                    $ newEnemy p 0 pnts size followingEnemySpeed

newSpawner :: Float -> Spawner
newSpawner intval = Spawner { _timeToNext = intval - 1
                            , _interval   = intval }

newBonus :: Point -> Bonus
newBonus position = Bonus { _bonusPos = position }

--The bonus size is always the same
bonusSize :: Float
bonusSize = 12

--Returns a new Bullet with given position and direction                               
newBullet :: Point -> Float -> Bullet
newBullet p d = Bullet { _bulPos   = p
                       , _bulSpeed = 10
                       , _bulDir   = d
                       , _bulTime  = 60
                       }

newMenu :: Menu
newMenu = Menu { _hasDiedBefore   = False
               , _selectionOption = 0 }

starSpawnChance :: Float
starSpawnChance = 0.3

-- Chance a following enemy is spawned
followingChance :: Float
followingChance = 0.4

newStar :: Point -> Float -> Star
newStar p s = Star { _starPos   = p
                   , _starSpeed = s
                   }

newParticle :: Point -> Float -> Color -> Particle
newParticle position size col = Particle { _partPos  = position
                                         , _partSize = size
                                         , _partCol  = col }




