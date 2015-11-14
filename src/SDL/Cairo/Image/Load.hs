
--------------------------------------------------------------------
{-|
Module      : SDL.Cairo.Image.Loader
Copyright   : Copyright (c) 2015 Yun-Yan Chi
License     : MIT
Maintainer  : jaiyalas@gmail.com

This module exposes wrapper functions to load image files into memory
by using JuicyPixel (See <https://hackage.haskell.org/package/JuicyPixels>).

So far, supported file formats are only PNG, JPG, BMP.
Plus, merely four pixel formats
'PixelRGB8', 'PixelRGB16', 'PixelRGBA8' and 'PixelRGBA16'
are supported.
-}
--------------------------------------------------------------------
module SDL.Cairo.Image.Load
   (
   -- * loading 'Image'
     loadRGB8
   , loadRGB16
   , loadRGBA8
   , loadRGBA16
   -- * loadable format
   , ImgType (..)
   -- * Default 5x5 'Image'
   , defImageRGB8
   , defImageRGB16
   , defImageRGBA8
   , defImageRGBA16
   ) where
--
import Codec.Picture
import qualified Data.Vector.Storable as VecS (fromList)
--

-- |
data ImgType = PNG | JPG | BMP deriving (Show,Eq)

loadRGB8   :: ImgType -> FilePath -> IO (Image PixelRGB8)
loadRGB8 PNG fp = do
   ei <- readPng fp
   case ei of
      Right (ImageRGB8 img) -> do
         return img
      _ -> do
         return defImageRGB8
--
loadRGB8 JPG fp = do
   ei <- readJpeg fp
   case ei of
      Right (ImageRGB8 img) -> do
         return img
      _ -> do
         return defImageRGB8
--
loadRGB8 BMP fp = do
   ei <- readBitmap fp
   case ei of
      Right (ImageRGB8 img) -> do
         return img
      _ -> do
         return defImageRGB8
--
loadRGB16  :: ImgType -> FilePath -> IO (Image PixelRGB16)
loadRGB16 PNG fp = do
   ei <- readPng fp
   case ei of
      Right (ImageRGB16 img) -> do
         return img
      _ -> do
         return defImageRGB16
--
loadRGB16 JPG fp = do
   ei <- readJpeg fp
   case ei of
      Right (ImageRGB16 img) -> do
         return img
      _ -> do
         return defImageRGB16
--
loadRGB16 BMP fp = do
   ei <- readBitmap fp
   case ei of
      Right (ImageRGB16 img) -> do
         return img
      _ -> do
         return defImageRGB16
--
loadRGBA8  :: ImgType -> FilePath -> IO (Image PixelRGBA8)
loadRGBA8 PNG fp = do
   ei <- readPng fp
   case ei of
      Right (ImageRGBA8 img) -> do
         return img
      _ -> do
         return defImageRGBA8
--
loadRGBA8 JPG fp = do
   ei <- readJpeg fp
   case ei of
      Right (ImageRGBA8 img) -> do
         return img
      _ -> do
         return defImageRGBA8
--
loadRGBA8 BMP fp = do
   ei <- readBitmap fp
   case ei of
      Right (ImageRGBA8 img) -> do
         return img
      _ -> do
         return defImageRGBA8
--
loadRGBA16 :: ImgType -> FilePath -> IO (Image PixelRGBA16)
loadRGBA16 PNG fp = do
   ei <- readPng fp
   case ei of
      Right (ImageRGBA16 img) -> do
         return img
      _ -> do
         return defImageRGBA16
--
loadRGBA16 JPG fp = do
   ei <- readJpeg fp
   case ei of
      Right (ImageRGBA16 img) -> do
         return img
      _ -> do
         return defImageRGBA16
--
loadRGBA16 BMP fp = do
   ei <- readBitmap fp
   case ei of
      Right (ImageRGBA16 img) -> do
         return img
      _ -> do
         return defImageRGBA16
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
--
defImageRGB16 :: Image PixelRGB16
defImageRGB16 = Image
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
--
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
--
defImageRGBA16 :: Image PixelRGBA16
defImageRGBA16 = Image
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
