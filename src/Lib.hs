{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
  ( goBoids,
  )
where

import Apecs
import Apecs.Gloss
import Apecs.TH
import Control.Monad (forM_)
import System.Exit (exitSuccess)

newtype Position = Position Vec deriving (Show)

newtype Velocity = Velocity Vec deriving (Show)

newtype Force = Force Vec deriving (Show)

type Vec = (Float, Float)

data Boid = Boid deriving (Show)

data Attractor = Attractor deriving (Show)

makeMapComponents [''Position, ''Velocity, ''Boid, ''Attractor, ''Force]
makeWorld "World" [''Camera, ''Position, ''Velocity, ''Boid, ''Attractor, ''Force]

type System' a = System World a

makeBoid :: Float -> (Boid, Position, Velocity, Force)
makeBoid f = (Boid, Position (sin f * 10, cos f * 10), Velocity (0, 0), Force (0, 0))

initialize :: System' ()
initialize = do
  forM_ [0 .. 10] $ newEntity_ . makeBoid

translate' :: Position -> Picture -> Picture
translate' (Position (x, y)) = translate x y

draw :: System' Picture
draw = do
  boids <- foldDraw (\(Boid, pos) -> translate' pos . color white . scale 1 1 $ circleSolid 3)
  attractorPic <- foldDraw (\(Attractor, pos) -> translate' pos . color green . scale 1 1 $ circle 3)
  pure $ boids <> attractorPic

handleEvent :: Event -> System' ()
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess
handleEvent (EventKey (MouseButton LeftButton) Down (Modifiers Down _ _) (x, y)) = do
  cmap $ \(Attractor, Position _) -> (Not @(Attractor, Position))
  newEntity_ (Attractor, Position (x, y))
handleEvent (EventKey (MouseButton RightButton) Down _ _) = cmap $ \(Attractor, Position _) -> (Not @(Attractor, Position))
handleEvent (EventKey (MouseButton LeftButton) Down (Modifiers Up _ _) (x, y)) = newEntity_ (Boid, Position (x, y), Velocity (0, 0), Force (0,0))
handleEvent _ = pure ()

step :: Float -> System' ()
step dt = do
  centreOfMass 50 0.1
  followOthers 50 0.125
  avoidCollisions 1
  attractor 5
  applyForce
  clampSpeed 100
  applyVelocity dt
  connectEdges

attractor :: Float -> System' ()
attractor fac = do
  maybeAttrac <- cfold (\_acc (Attractor, Position p) -> Just p) Nothing
  case maybeAttrac of
    Just attrac ->
      cmap $ \(Boid, Position p) -> Force $ vclamp fac (attrac `vsub` p)
    Nothing -> pure ()

applyVelocity :: Float -> System' ()
applyVelocity dt = cmap $ \(Position p, Velocity v) -> Position $ p `vadd` vscale dt v

applyForce :: System' ()
applyForce = cmap $ \(Velocity v, Force f) -> (Velocity $ v `vadd` f, Force (0,0))

clampSpeed :: Float -> System' ()
clampSpeed mx = cmap $ \(Velocity v) -> Velocity (vclamp mx v)

connectEdges :: System' ()
connectEdges = cmap $ \case
  (Position (x, y)) | x > 250 -> Position (-250, y)
  (Position (x, y)) | x < -250 -> Position (250, y)
  (Position (x, y)) | y > 250 -> Position (x, -250)
  (Position (x, y)) | y < -250 -> Position (x, 250)
  p -> p

centreOfMass :: Float -> Float -> System' ()
centreOfMass centreDist fac = do
  cmapM
    ( \(Boid, Position p, Force f) -> do
        boidCount <- cfold (\acc (Boid, Position p') -> if vdist p p' < centreDist then 1 + acc else acc) 0
        centre <- vscale (1 / boidCount) <$> calcCentre p
        pure (Force $ f `vadd` vscale fac (centre `vsub` p))
    )
  where
    calcCentre :: Vec -> System' Vec
    calcCentre pOwn = cfold (\acc (Boid, Position p) -> if p /= pOwn && vdist pOwn p < centreDist then vadd p acc else acc) (0, 0)

avoidCollisions :: Float -> System' ()
avoidCollisions fac = cmapM $ \(Boid, Position p, Force f) -> do
  avoid <- calcAvoidance p
  pure (Force $ f `vadd` vscale fac avoid)
  where
    calcAvoidance :: Vec -> System' Vec
    calcAvoidance p = flip cfold (0, 0) $ \acc (Boid, Position p') -> case vsub p p' of
      (0, 0) -> acc
      (dx, dy) | abs dx + abs dy < 15 -> vsub acc (vsub p' p)
      _ -> acc

followOthers :: Float -> Float -> System' ()
followOthers flockDist fac = do
  cmapM
    ( \(Boid, Position p, Velocity v, Force f) -> do
        boidCount <- cfold (\acc (Boid, Position p') -> if vdist p p' < flockDist then 1 + acc else acc) 0
        centre <- vscale (1 / boidCount) <$> calcVelocity p v
        pure (Force $ f `vadd` vscale fac centre)
    )
  where
    calcVelocity :: Vec -> Vec -> System' Vec
    calcVelocity pOwn vOwn = cfold (\acc (Boid, Velocity v, Position p) -> if v /= vOwn && vdist pOwn p < flockDist then vadd v acc else acc) (0, 0)

vadd :: Num a => (a, a) -> (a, a) -> (a, a)
vadd (x, y) (x', y') = (x + x', y + y')

vsub :: Num a => (a, a) -> (a, a) -> (a, a)
vsub (x, y) (x', y') = (x - x', y - y')

vscale :: Num a => a -> (a, a) -> (a, a)
vscale s (x, y) = (s * x, s * y)

vclamp :: Float -> Vec -> Vec
vclamp mx (x, y) = let l = abs x + abs y in if l < mx then (x, y) else vscale (mx / l) (x, y)

vdist :: Vec -> Vec -> Float
vdist (x, y) (x', y') = abs (x - x') + abs (y - y')

goBoids :: IO ()
goBoids = do
  world <- initWorld
  runWith world $ do
    initialize
    play (InWindow "Boid" (500, 500) (10, 10)) black 60 draw handleEvent step
