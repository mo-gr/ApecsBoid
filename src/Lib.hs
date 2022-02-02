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

makeBoid :: Vec -> (Boid, Position, Velocity, Force)
makeBoid v = (Boid, Position v, Velocity (0, 0), Force (0, 0))

initialize :: System' ()
initialize = do
  forM_ [0 .. 10] $ newEntity_ . makeBoid . \f -> (sin f * 10, cos f * 10)

translate' :: Position -> Picture -> Picture
translate' (Position (x, y)) = translate x y

radToDeg :: Float -> Float
radToDeg r = r * 180 / pi

draw :: System' Picture
draw = do
  boids <- foldDraw (\(Boid, pos, Velocity (vx, vy)) -> translate' pos . rotate (radToDeg $ atan2 vx vy) . color white . scale 1 1 $ lineLoop [(-2, -2), (2, -2), (0, 4)])
--  boidVelocities <- foldDraw (\(Boid, Position (x, y), Velocity (vx, vy)) -> translate' (Position (x + vx, y + vy)) . color white . scale 1 1 $ circleSolid 2)
  attractorPic <- foldDraw (\(Attractor, pos) -> translate' pos . color green . scale 1 1 $ circle 3)
  pure $ boids <> attractorPic -- <> boidVelocities

handleEvent :: Event -> System' ()
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess
handleEvent (EventKey (MouseButton LeftButton) Down (Modifiers Down _ _) (x, y)) = do
  cmap $ \(Attractor, Position _) -> (Not @(Attractor, Position))
  newEntity_ (Attractor, Position (x, y))
handleEvent (EventKey (MouseButton RightButton) Down _ _) = cmap $ \(Attractor, Position _) -> (Not @(Attractor, Position))
handleEvent (EventKey (MouseButton LeftButton) Down (Modifiers Up _ _) (x, y)) = newEntity_ $ makeBoid (x, y)
handleEvent _ = pure ()

step :: Float -> System' ()
step dt = do
  centerOfMass 50 0.1
  followOthers 50 0.125
  avoidCollisions 1
  attractor 5
  applyForce
  clampSpeed 200
  applyVelocity dt
  connectEdges

attractor :: Float -> System' ()
attractor fac =
  cmapM_ $ \(Attractor, Position attrac) ->
    cmap $ \(Boid, Position p, Force f) -> Force $ f `vadd` vclamp fac (attrac `vsub` p)

applyVelocity :: Float -> System' ()
applyVelocity dt = cmap $ \(Position p, Velocity v) -> Position $ p `vadd` vscale dt v

applyForce :: System' ()
applyForce = cmap $ \(Velocity v, Force f) -> (Velocity $ v `vadd` f, Force (0, 0))

clampSpeed :: Float -> System' ()
clampSpeed mx = cmap $ \(Velocity v) -> Velocity (vclamp mx v)

connectEdges :: System' ()
connectEdges = cmap $ \case
  (Position (x, y)) | x > 250 -> Position (-250, y)
  (Position (x, y)) | x < -250 -> Position (250, y)
  (Position (x, y)) | y > 250 -> Position (x, -250)
  (Position (x, y)) | y < -250 -> Position (x, 250)
  p -> p

centerOfMass :: Float -> Float -> System' ()
centerOfMass radius fac =
  cmapM
    ( \(Boid, Position p, self, Force f) -> do
        countAndCenter <- calcCenter self (Position p)
        let center = case countAndCenter of
              (0, _) -> p
              (n, centerVec) -> vscale (1 / n) centerVec
        pure (Force $ f `vadd` vscale fac (center `vsub` p))
    )
  where
    calcCenter :: Entity -> Position -> System' (Float, Vec)
    calcCenter self pos =
      cfold
        ( \(cnt, acc) (Boid, e, Position p') ->
            if self /= e && isVisible radius pos (Position p')
              then (cnt + 1, vadd p' acc)
              else (cnt, acc)
        )
        (0, (0, 0))

isVisible :: Float -> Position -> Position -> Bool
isVisible radius (Position p) (Position p')
  | vdist p p' < radius = True
  | otherwise = False

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
followOthers radius fac = do
  cmapM
    ( \(Boid, e, p, Force f) -> do
        countAndVelocity <- calcVelocity e p
        case countAndVelocity of
          (0, _) -> pure (Force f)
          (n, velo) -> pure (Force $ f `vadd` vscale (fac / n) velo)
    )
  where
    calcVelocity :: Entity -> Position -> System' (Float, Vec)
    calcVelocity self pos =
      cfold
        ( \(count, acc) (Boid, Velocity v, pos', other) ->
            if self /= other && isVisible radius pos pos'
              then (count + 1, vadd v acc)
              else (count, acc)
        )
        (0, (0, 0))

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
