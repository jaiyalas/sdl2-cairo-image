
--------------------------------------------------------------------
{-|
Module      : SDL.Cairo.Image.Loader
Copyright   : Copyright (c) 2015 Yun-Yan Chi
License     : MIT
Maintainer  : jaiyalas@gmail.com

This module exposes the functions to load image files into memory
by using JuicyPixel (See <https://hackage.haskell.org/package/JuicyPixels>).

So far, supports only format PNG (Portable Network Graphics) and
two kinds of pixel format 'PixelRGB8' and 'PixelRGBA8'.
-}
--------------------------------------------------------------------
module SDL.Cairo.Image.Loader
   (
   -- * Image loading
     loadPNGRGB
   , loadPNGRGBA
   -- * Default image
   , defImageRGB8
   , defImageRGBA8
   ) where
--
import Codec.Picture
import qualified Data.Vector.Storable as VecS (fromList)
import Data.Monoid
--

-- | ...
loadPNGRGB :: FilePath -> IO (Image PixelRGB8)
loadPNGRGB fp = do
   ei <- readPng fp
   case ei of
      Right (ImageRGB8 img) -> do
         return img
      _ -> do
         return defImageRGB8

-- | ...
loadPNGRGBA :: FilePath -> IO (Image PixelRGBA8)
loadPNGRGBA fp = do
   ei <- readPng fp
   case ei of
      Right (ImageRGBA8 img) -> do
         return img
      _ -> do
         return defImageRGBA8
--
defImageRGB8 :: Image PixelRGB8
defImageRGB8 = Image
   { imageWidth = 5
   , imageHeight = 5
   , imageData = VecS.fromList
      [ 255,0,0,  0,0,0,   0,0,0,   0,0,0,   255,0,0
      , 0,0,0,    255,0,0, 0,0,0,   255,0,0, 0,0,0
      , 0,0,0,    0,0,0,   255,0,0, 0,0,0,   0,0,0
      , 0,0,0,    255,0,0, 0,0,0,   255,0,0, 0,0,0
      , 255,0,0,  0,0,0,   0,0,0,   0,0,0,   255,0,0
      ]
   }
defImageRGBA8 :: Image PixelRGBA8
defImageRGBA8 = Image
   { imageWidth = 5
   , imageHeight = 5
   , imageData = VecS.fromList 
      [ 255,0,0,255,  0,0,0,255,   0,0,0,255,   0,0,0,255,   255,0,0,255
      , 0,0,0,255,    255,0,0,255, 0,0,0,255,   255,0,0,255, 0,0,0,255
      , 0,0,0,255,    0,0,0,255,   255,0,0,255, 0,0,0,255,   0,0,0,255
      , 0,0,0,255,    255,0,0,255, 0,0,0,255,   255,0,0,255, 0,0,0,255
      , 255,0,0,255,  0,0,0,255,   0,0,0,255,   0,0,0,255,   255,0,0,255
      ]
   }
