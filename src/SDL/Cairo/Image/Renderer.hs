{-|
Module      : SDL.Cairo.Image.Renderer
Copyright   : Copyright (c) 2015 Yun-Yan Chi
License     : MIT
Maintainer  : jaiyalas@gmail.com

This module provides functions for directly/indirectly
rendering a JuicyPixel's 'Image' onto a sdl2's 'Texture'.

This module is actually implemented with both of 'Render' and 'Canvas',
therefore it provides functions in both way.
In fact, There are three simple ways to use this module.
Assume we alread have following code,

>import SDL
>import Linear.V2 (V2(..))
>import SDL.Cairo
>import SDL.Cairo.Canvas
>import SDL.Cairo.Image.Renderer
>import Codec.Picture
>
>main :: IO ()
>main = do
>  initialize [InitEverything]
>  window <- createWindow "SDL2 Cairo Canvas" defaultWindow
>  renderer <- createRenderer window (-1) defaultRenderer
>  texture <- createCairoTexture' renderer window
>  withCanvas texture $ do
>    background $ gray 102
>    -- location A
>
>  Right (ImageRGB8 img) <- readPng "pic.png"
>  -- location B
>
>  copy renderer texture Nothing Nothing
>  present renderer
>  delay 5000

[using @renderImgCanvas@]

   The first way to draw image is replacing

   > -- location A

   by

   >renderImgCanvas (V2 50 50) img

[using @drawImg@ or @drawImgC@]

   The other ways are replacing

   > -- location B

   by

   >drawImg texture (V2 50 50) img

   or

   >drawImgC texture (V2 50 50) img


-}
module SDL.Cairo.Image.Renderer
   (
   -- * draw 'Image' in 'IO' ()
     drawImg
   , drawImgC
   -- * draw 'Image' in 'Canvas' ()
   , renderImgCanvas
   -- * draw 'Image' in 'Render' ()
   , renderImgCairo
   , renderOnTexture
   -- * Type class
   , RenderablePixel (drawPx)
   , RenderablePixelC (renderPx)
   ) where
--
import Control.Monad (void)
--
import qualified Data.Convertible as CVT
--
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import SDL (Texture)
import SDL.Cairo.Canvas
   ( withCanvas, renderCairo
   , Dim (..), Canvas
   , rect, noStroke, fill, rgb)
--
import qualified Graphics.Rendering.Cairo as Cairo
import Codec.Picture

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | draw a __JuicyPixel__ 'Image' on the given __SDL__ 'Texture'
-- (at the specified coordinate).
drawImg :: RenderablePixel a =>
             Texture -- ^ a 'Texture'
          -> V2 Int  -- ^ a 'V2' coordinate of the top left corner
          -> Image a -- ^ an 'Image'
          -> IO ()
drawImg st v2 img = withCanvas st $ renderImgCanvas v2 img

-- | draw a __JuicyPixel__ 'Image' on the given __SDL__ 'Texture'
-- (at the specified coordinate).
-- Using Cairo's 'Render' underneath.
drawImgC :: RenderablePixelC a =>
             Texture   -- ^ a 'Texture'
          -> V2 Int    -- ^ a 'V2' coordinate of the top left corner
          -> Image a   -- ^ an 'Image'
          -> IO ()
drawImgC st v2 img = renderOnTexture st $ renderImgCairo v2 img

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | render an __JuicyPixel__ 'Image' through __Cairo__ 'Canvas'
renderImgCanvas :: RenderablePixel a =>
                  V2 Int    -- ^ a 'V2' coordinate of the top left corner
               -> Image a   -- ^ an 'Image'
               -> Canvas () -- ^ a 'Canvas' action
renderImgCanvas (V2 _x _y) img@(Image _w _h _) = do
   let ps = [(V2 (_x+offx) (_y+ offy)
             ,pixelAt img offx offy)
            | offx <- [0..(_w-1)], offy <- [0..(_h-1)]]
   void $ mapM drawPx ps

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | perform a 'Render' action on the given 'Texture'
renderOnTexture :: Texture            -- ^ target 'Texture'
                -> Cairo.Render () -- ^ a 'Render' action
                -> IO ()
renderOnTexture st = withCanvas st . renderCairo

-- | render an 'Image' through __Cairo__ 'Render'
renderImgCairo :: RenderablePixelC a =>
                  V2 Int          -- ^ a 'V2' coordinate of the top left corner
               -> Image a         -- ^ a JuicyPixel 'Image'
               -> Cairo.Render () -- ^ an Cairo 'Render' action
renderImgCairo (V2 _x _y) img@(Image _w _h _) = do
   let ps = [(V2 (_x+offx) (_y+ offy)
             ,pixelAt img offx offy)
            | offx <- [0..(_w-1)], offy <- [0..(_h-1)]]
   void $ mapM renderPx ps

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | A pixel structure is 'RenderablePixel' if
-- how to draw on 'Canvas' is defined.
class Pixel px => RenderablePixel px where
   drawPx :: (V2 Int, px)    -- ^ (coordinate, pixel)
          -> Canvas ()
--
instance RenderablePixel PixelRGB8 where
   drawPx (V2 _x _y, PixelRGB8 _r _g _b) = do
      fill $ rgb _r _g _b
      noStroke
      rect $ D (fromIntegral _x) (fromIntegral _y) 1 1
--
instance RenderablePixel PixelRGB16 where
   drawPx (V2 _x _y, PixelRGB16 _r _g _b) = do
      fill $ rgb (CVT.convert _r) (CVT.convert _g) (CVT.convert _b)
      noStroke
      rect $ D (fromIntegral _x) (fromIntegral _y) 1 1
--
instance RenderablePixel PixelRGBA8 where
   drawPx (V2 _x _y, PixelRGBA8 _r _g _b _a) = do
      fill $ V4 _r _g _b _a
      noStroke
      rect $ D (fromIntegral _x) (fromIntegral _y) 1 1
--
instance RenderablePixel PixelRGBA16 where
   drawPx (V2 _x _y, PixelRGBA16 _r _g _b _a) = do
      fill $ V4 (CVT.convert _r) (CVT.convert _g) (CVT.convert _b) (CVT.convert _a)
      noStroke
      rect $ D (fromIntegral _x) (fromIntegral _y) 1 1

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | A pixel structure is 'RenderablePixelC' if
-- how to render on 'Render' can be provided.
class Pixel px => RenderablePixelC px where
   renderPx :: (V2 Int, px)    -- ^ (coordinate, pixel)
                 -> Cairo.Render ()
--
instance RenderablePixelC PixelRGB8 where
   renderPx (V2 _x _y, PixelRGB8 _r _g _b) = do
      Cairo.rectangle (fromIntegral _x) (fromIntegral _y) 1 1
      Cairo.setSourceRGB
         ((/255) $ fromIntegral _r)
         ((/255) $ fromIntegral _g)
         ((/255) $ fromIntegral _b)
      Cairo.fill
--
instance RenderablePixelC PixelRGB16 where
   renderPx (V2 _x _y, PixelRGB16 _r _g _b) = do
      Cairo.rectangle (fromIntegral _x) (fromIntegral _y) 1 1
      Cairo.setSourceRGB
         ((/255) $ fromIntegral _r)
         ((/255) $ fromIntegral _g)
         ((/255) $ fromIntegral _b)
      Cairo.fill
--
instance RenderablePixelC PixelRGBA8 where
   renderPx (V2 _x _y, PixelRGBA8 _r _g _b _a) = do
      Cairo.rectangle (fromIntegral _x) (fromIntegral _y) 1 1
      Cairo.setSourceRGBA
         ((/255) $ fromIntegral _r)
         ((/255) $ fromIntegral _g)
         ((/255) $ fromIntegral _b)
         ((/255) $ fromIntegral _a)
      Cairo.fill
--
instance RenderablePixelC PixelRGBA16 where
   renderPx (V2 _x _y, PixelRGBA16 _r _g _b _a) = do
      Cairo.rectangle (fromIntegral _x) (fromIntegral _y) 1 1
      Cairo.setSourceRGBA
         ((/255) $ fromIntegral _r)
         ((/255) $ fromIntegral _g)
         ((/255) $ fromIntegral _b)
         ((/255) $ fromIntegral _a)
      Cairo.fill
--
