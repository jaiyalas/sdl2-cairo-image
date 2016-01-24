# JuicyPixel

## Codec.Picture

```haskell
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
```

```haskell
data Image a = Image
    { imageWidth  :: {-# UNPACK #-} !Int
    , imageHeight :: {-# UNPACK #-} !Int
    , imageData   :: V.Vector (PixelBaseComponent a)
    }
```

```haskell
data MVector s a = MVector ...
type STVector s = MVector s
type IOVector = MVector RealWorld
```

sdl2 用 IOVector

JuicyPixel 用
