{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL
import SDL.Cairo (createCairoTexture)
import SDL.Cairo.Canvas
--
import Linear.V2 (V2(..))
--
import Data.Maybe (catMaybes)
import Control.Concurrent (threadDelay)
import Control.Monad (unless,void,mapM)
import Control.Monad.Fix (fix)
--
import qualified SDL.Cairo.Image as I
--
main :: IO ()
main = do
   --
   SDL.initialize [SDL.InitEverything]
   window   <- SDL.createWindow "Hello!" winConfig
   renderer <- SDL.createRenderer window (-1) rdrConfig
   texture  <- createCairoTexture renderer (V2 640 480)
   --
   -- img1 <- L.loadPNGRGBA "/Users/jaiyalas/img/xd.png"
   -- img2 <- L.loadPNGRGBA "/Users/jaiyalas/img/char.png"
   img3 <- I.loadRGBA8 I.PNG "./img/newton.png"
   img4 <- I.loadRGB8 I.PNG "./img/newton.png"
   --
   fix $ \loop -> do
         threadDelay 100000
         withCanvas texture $ do
            background $ gray 102
            I.renderImgCanvas (V2 10  50) img3
            I.renderImgCanvas (V2 10 200) img4
         -- R.drawImg  texture   (V2 50 120) img3
         -- R.drawImgC texture   (V2 50 220) img3
         SDL.copy renderer texture Nothing Nothing
         SDL.present renderer
         qb <- quitPredicate
         unless qb loop
--
-- ################################
--
quitPredicate :: IO Bool
quitPredicate = do
   events <- SDL.pollEvents
   return $ elem SDL.KeycodeEscape $ events2Kcodes events
--
events2Kcodes :: [SDL.Event] -> [SDL.Keycode]
events2Kcodes = catMaybes . map event2Kcode
--
event2Kcode :: SDL.Event -> Maybe SDL.Keycode
event2Kcode e = case SDL.eventPayload e of
   SDL.KeyboardEvent edata -> if isPressed edata
      then return $ getKeycode edata
      else Nothing
   otherwise -> Nothing
--
isPressed :: SDL.KeyboardEventData -> Bool
isPressed edata = SDL.keyboardEventKeyMotion edata == SDL.Pressed
--
getKeycode :: SDL.KeyboardEventData -> SDL.Keycode
getKeycode = SDL.keysymKeycode . SDL.keyboardEventKeysym
--
-- ##################################
--
winConfig = SDL.defaultWindow
   { SDL.windowPosition = SDL.Centered
   , SDL.windowInitialSize = V2 640 480
   }

rdrConfig = SDL.RendererConfig
   { SDL.rendererType = SDL.AcceleratedVSyncRenderer
   , SDL.rendererTargetTexture = True
   }
