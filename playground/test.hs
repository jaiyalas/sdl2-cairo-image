{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test where
--
import qualified SDL
import qualified SDL.Raw.Types as RT
import Foreign.Storable
import Foreign.Ptr
--
import Codec.Picture as JP
import Codec.Picture.Types as JP.T
--
import Data.Vector.Storable as SV
--

lesson03 :: IO ()
lesson03 = do
   -- SDL.initialize [SDL.InitVideo]
   -- window <- SDL.createWindow "Lesson03" SDL.defaultWindow
   -- SDL.showWindow window
   -- gSurface <- SDL.getWindowSurface window
   sur <- SDL.loadBMP "./Broom.bmp"
   -- define the main loop
   -- let
   --    loop = do
   --       -- fetch all events from events pool
   --       events <- SDL.pollEvents
   --       -- check the existence of QuitEvent
   --       let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
   --       SDL.surfaceFillRect gSurface Nothing $
   --          V4 minBound minBound minBound maxBound
   --       SDL.surfaceBlit pictureS Nothing gSurface Nothing
   --       SDL.updateWindowSurface window
   --       threadDelay 20000
   --       unless quit loop
   -- -- exec our main loop
   -- loop
   -- SDL.destroyWindow window
   -- SDL.freeSurface pictureS
   SDL.quit

-- test01 :: FilePath -> IO ()
-- test01 str = do
--    (Right dymg) <- readImage str
--    case dymg of
--       ImageY8     _ -> do print "ImageY8"
--       ImageY16    _ -> do print "ImageY16"
--       ImageYF     _ -> do print "ImageYF"
--       ImageYA8    _ -> do print "ImageYA8"
--       ImageYA16   _ -> do print "ImageYA16"
--       ImageRGB8   _ -> do print "ImageRGB8"
--       ImageRGB16  _ -> do print "ImageRGB16"
--       ImageRGBF   _ -> do print "ImageRGBF"
--       ImageRGBA8  _ -> do print "ImageRGBA8"
--       ImageRGBA16 _ -> do print "ImageRGBA16"
--       ImageYCbCr8 _ -> do print "ImageYCbCr8"
--       ImageCMYK8  _ -> do print "ImageCMYK8"
--       ImageCMYK16 _ -> do print "ImageCMYK16"
--    return ()
--
-- test02 :: forall px. FilePath -> IO (Maybe (Vector px))
-- test02 str = readImage str >>= \case
--    Left msg   -> return Nothing
--    Right dymg -> return $ Just $ vct where
--       vct :: Vector px
--       !vct = case dymg of
--          ImageY8     (Image w h vec) -> return vec
--          ImageY16    (Image w h vec) -> return vec
--          ImageYF     (Image w h vec) -> return vec
--          ImageYA8    (Image w h vec) -> return vec
--          ImageYA16   (Image w h vec) -> return vec
--          ImageRGB8   (Image w h vec) -> return vec
--          ImageRGB16  (Image w h vec) -> return vec
--          ImageRGBF   (Image w h vec) -> return vec
--          ImageRGBA8  (Image w h vec) -> return vec
--          ImageRGBA16 (Image w h vec) -> return vec
--          ImageYCbCr8 (Image w h vec) -> return vec
--          ImageCMYK8  (Image w h vec) -> return vec
--          ImageCMYK16 (Image w h vec) -> return vec
--    otherwise -> return Nothing
-- --
-- -- getVec :: DynamicImage -> SV.Vector Pixel8
-- -- -- getVec (ImageY8     img) = imageData img
-- -- -- getVec (ImageY16    img) = imageData img
-- -- -- getVec (ImageYF     img) = imageData img
-- -- -- getVec (ImageYA8    img) = imageData img
-- -- -- getVec (ImageYA16   img) = imageData img
-- -- -- getVec (ImageRGB8   img) = imageData img
-- -- -- getVec (ImageRGB16  img) = imageData img
-- -- -- getVec (ImageRGBF   img) = imageData img
-- -- getVec (ImageRGBA8  img) = imageData img
-- -- -- getVec (ImageRGBA16 img) = imageData img
-- -- -- getVec (ImageYCbCr8 img) = imageData img
-- -- -- getVec (ImageCMYK8  img) = imageData img
-- -- -- getVec (ImageCMYK16 img) = imageData img
-- -- getVec _ = undefined
