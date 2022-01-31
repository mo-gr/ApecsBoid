{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( goBoids,
  )
where

import Apecs
import Apecs.Gloss
import System.Exit (exitSuccess)

makeWorld "World" [''Camera]

type System' a = System World a

initialize :: System' ()
initialize = pure ()

draw :: System' Picture
draw = pure mempty

handleEvent :: Event -> System' ()
handleEvent (EventKey (SpecialKey KeyEsc) Down   _ _) = liftIO exitSuccess
handleEvent _ = pure ()

step :: Float -> System' ()
step dt = pure ()

goBoids :: IO ()
goBoids = do
  world <- initWorld
  runWith world $ do
    initialize
    play (InWindow "Boid" (320, 240) (10, 10)) black 60 draw handleEvent step
