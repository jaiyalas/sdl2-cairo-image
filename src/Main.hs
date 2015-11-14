{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL
import SDL.Cairo (createCairoTexture)
import SDL.Cairo.Canvas
--
import qualified Graphics.Rendering.Cairo as Ca
--
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
--
import Data.Maybe (catMaybes)
import Control.Concurrent (threadDelay)
import Control.Monad (unless,void,mapM)
import Control.Monad.Fix (fix)
--
import Codec.Picture
-- import Codec.Picture.Png
--
import Data.Word (Word8(..))
import qualified Data.Vector.Storable as VecS
import qualified Data.Vector as Vec
--
import qualified SDL.Cairo.Image.Loader as L
import qualified SDL.Cairo.Image.Renderer as R
--
main :: IO ()
main = do
   --
   SDL.initialize [SDL.InitEverything]
   window   <- SDL.createWindow "Hello!" winConfig
   renderer <- SDL.createRenderer window (-1) rdrConfig
   texture  <- createCairoTexture renderer (V2 640 480)
   --

   img <- L.loadPNGRGBA "/Users/jaiyalas/img/xdd.png"
   --
   fix $ \loop -> do
         threadDelay 100000
         withCanvas texture $ do
            background $ gray 102
            R.renderImgCanvas (V2 50 50) img
         R.drawImg  texture (V2 150 150) img
         R.drawImgC texture (V2 220 220) img
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
