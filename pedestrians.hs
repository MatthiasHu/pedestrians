{-# LANGUAGE TemplateHaskell #-}

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Control.Lens
import Control.Monad.Random
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
  model <- startModel
  let displayMode = InWindow "pedestrians" (512, 512) (0, 0)
  simulate displayMode blue 30 model render (\viewPort dt -> step)

startModel :: (MonadRandom m) => m Model
startModel = return $ Model
  [ Pedestrian (-200, 0) (200, 0) red
  , Pedestrian (-200, 6) (200, 0) red ]

render :: Model -> Picture
render m = mconcat
  [ translate' (p ^. pos) $ color (p ^. individualColor) $ circleSolid 5
  | p <- m ^. pedestrians ]
  where
    translate' = uncurry translate

step :: Model -> Model
-- step m = m & pedestrians . each . pos . _1 +~ 1
step m = m & pedestrians . each %~ move m

move :: Model -> Pedestrian -> Pedestrian
move m p = p & pos %~ plusV (subNormalize 1 (totalForce m p))

totalForce :: Model -> Pedestrian -> Vector
totalForce m p =
  (100, 0)

plusV :: Vector -> Vector -> Vector
plusV (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subNormalize :: Float -> Vector -> Vector
subNormalize r v =
  if m <= r
  then v
  else (r/m) `mulSV` v
  where
    m = magV v
