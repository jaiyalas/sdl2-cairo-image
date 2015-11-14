# sdl2-cairo-image

[![MIT](http://b.repl.ca/v1/license-MIT-blue.png)](https://en.wikipedia.org/wiki/MIT_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-green.png)](http://haskell.org)

An image loading and rendering library for sdl2 / sdl2-cairo

## Installation

This module can easily install via **Cabal**.

``` bash
> cabal update
> cabal install sdl2-cairo-image
```

## Usage

### Loading image

Assume one import this module as

``` haskell
import qualified SDL.Cairo.Image as I
```

One can load an image within an `IO a`.
For example,

``` haskell
main = do
   img <- I.loadRGBA8 PNG "wherever/your/image/is/img.png"
```

If this file is loaded correctly then `img` will be a `Image PixelRGBA8`.
If failed, `img` will be a default 5x5 image (i.e. `I.defImageRGBA8`).

### Displaying image

Please read [here](https://hackage.haskell.org/package/sdl2-cairo-image/docs/SDL-Cairo-Image-Render.html)!
