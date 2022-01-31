{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
  ( goBoids,
  )
where

import Apecs
import Apecs.Gloss
import Apecs.TH
import System.Exit (exitSuccess)
import Control.Monad (forM_)

newtype Position = Position (Float, Float) deriving (Show)

newtype Velocity = Velocity (Float, Float) deriving (Show)

data Boid = Boid deriving (Show)

makeMapComponents [''Position, ''Velocity, ''Boid]
makeWorld "World" [''Camera, ''Position, ''Velocity, ''Boid]

type System' a = System World a

makeBoid :: Float -> (Boid, Position, Velocity)
makeBoid f = (Boid, Position (f, f), Velocity (f, f))

initialize :: System' ()
initialize = do
  forM_ [0..10] $ newEntity_ . makeBoid

translate' :: Position -> Picture -> Picture
translate' (Position (x, y)) = translate x y

draw :: System' Picture
draw = do
  foldDraw (\(Boid, pos) -> translate' pos . color white . scale 1 1 $ circleSolid 3)

handleEvent :: Event -> System' ()
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess
handleEvent _ = pure ()

step :: Float -> System' ()
step dt = do
  cmap $ \(Position (x,y), Velocity (vx,vy)) -> Position (x + (dt * vx), y + (dt * vy))

goBoids :: IO ()
goBoids = do
  world <- initWorld
  runWith world $ do
    initialize
    play (InWindow "Boid" (500, 500) (10, 10)) black 60 draw handleEvent step
