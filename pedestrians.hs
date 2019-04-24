{-# LANGUAGE TemplateHaskell #-}

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Control.Lens
import Control.Monad.Random
import Control.Applicative
import Data.Monoid

data Pedestrian = Pedestrian
  { _pos :: Point
  , _destination :: Point
  , _individualColor :: Color
  }
  deriving (Show)

makeLenses ''Pedestrian

data Model = Model
  { _pedestrians :: [Pedestrian]
  }
  deriving (Show)

makeLenses ''Model

main :: IO ()
main = do
  model <- evalRandIO startModel
  let
    displayMode = InWindow "pedestrians" (512, 512) (0, 0)
    background = dark . dark . dark $ green
  simulate displayMode background 30 model render (\viewPort dt -> step)

startModel :: (Applicative m, MonadRandom m) => m Model
startModel = groups
  [ groupOfPedestrians (-200, 0) (200, 0) (bright red) 12
  , groupOfPedestrians (200, -100) (-200, 200) (bright yellow) 6
  , groupOfPedestrians (-100, -100) (-100, 150) (bright cyan) 9
  , groupOfPedestrians (-200, 180) (100, -100) (bright green) 3
  ]
  where
    groups gs = Model . concat <$> sequence gs

groupOfPedestrians :: (Applicative m, MonadRandom m) =>
  Point -> Point -> Color -> Int -> m [Pedestrian]
groupOfPedestrians (xc, yc) dest color n =
  sequence . replicate n
  $ fmap (\pos -> Pedestrian pos dest color) boxPosition
  where
    boxPosition = (,)
      <$> (getRandomR (xc-r, xc+r))
      <*> (getRandomR (yc-r, yc+r))
    r = sqrt (fromIntegral n) * 10

render :: Model -> Picture
render m = mconcat
  [ translate' (p ^. pos)
    $ color (p ^. individualColor) $ circleSolid 5
  | p <- m ^. pedestrians ]
  where
    translate' = uncurry translate

step :: Model -> Model
step m = m & pedestrians . each %~ move m

move :: Model -> Pedestrian -> Pedestrian
move m p = p & pos %~ plusV
  (cutoffNormalize $ totalForce m p)

totalForce :: Model -> Pedestrian -> Vector
totalForce m p =
  destinationForce p
  `plusV` totalProximityForce m p

totalProximityForce :: Model -> Pedestrian -> Vector
totalProximityForce m p =
  foldl plusV (0, 0) (map (proximityForce p) (m ^. pedestrians))

proximityForce :: Pedestrian -> Pedestrian -> Vector
proximityForce me you =
  if d < 10**(-3)
  then (0, 0)
  else
    max 0 (f_1 * (1/d - 1/d_0) / (1/d_1 - 1/d_0))
    `mulSV` normaliseV v
  where
    v = (me ^. pos) `minusV` (you ^. pos)
    d = magV v
    d_0 = 20  -- force is 0 at this distance
    d_1 = 10  -- force is f_1 at this distance
    f_1 = 2

destinationForce :: Pedestrian -> Vector
destinationForce p =
  subNormalize 2 $
  0.1 `mulSV` ((p ^. destination) `minusV` (p ^. pos))

cutoff :: Float -> Vector -> Vector
cutoff c v = if magV v < c then (0, 0) else v

plusV :: Vector -> Vector -> Vector
plusV (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

minusV :: Vector -> Vector -> Vector
minusV (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

subNormalize :: Float -> Vector -> Vector
subNormalize r v =
  if m <= r
  then v
  else (r/m) `mulSV` v
  where
    m = magV v

cutoffNormalize :: Vector -> Vector
cutoffNormalize v =
  if m <= 1
  then (0, 0)
  else (1/m) `mulSV` v
  where
    m = magV v
