
-- TODO: 
--   * rewrite the file loading so that it we do not depend on ByteString
--   * automatic glyph indexing, texture creation

--
-- Module      : Graphics.Rendering.TrueType.STB
-- Version     : 0.1.1
-- License     : Public Domain
-- Author      : Balazs Komuves
-- Maintainer  : bkomuves (plus) hackage (at) gmail (dot) com
-- Stability   : experimental
-- Portability : portable(?), requires FFI and CPP
-- Tested with : GHC 6.10.1
--

-- | This is a wrapper around Sean Barrett's TrueType font rasterizer code.
-- The original can be found at <http://nothings.org/stb/stb_truetype.h>.
-- The version of @stb-truetype@ used here is @0.2@.

{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# CFILES cbits/wrapper.c #-}  -- for Hugs (?)
module Graphics.Rendering.TrueType.STB 
  ( TrueType
  , Offset
  , FontInfo
  , Glyph
  --
  , loadTTF
  , withTTF
  , enumerateFonts
  , initFont
  , findGlyphIndex
  --
  , Unscaled
  , HorizontalMetrics(..)
  , VerticalMetrics(..)
  , BoundingBox(..)
  , lineAdvance
  , verticalSize
  , scaleForPixelHeight
  , getFontVerticalMetrics
  , getGlyphHorizontalMetrics
  , getGlyphKernAdvance
  , getGlyphBoundingBox
  --
  , Scaling
  , Bitmap(..)
  , newBitmap
  , withBitmap
  , BitmapOfs
  , getGlyphBitmapBox
  , newGlyphBitmap
  , renderGlyphIntoBitmap'
  , renderGlyphIntoBitmap
  , bitmapArray
  , bitmapFloatArray
  --
  
  ) where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Applicative

import Data.Char
import Data.Maybe

import Data.Array
import Data.Array.IO
import Data.Array.Unboxed
#ifdef __GLASGOW_HASKELL__
import qualified Data.Array.Base as Arr 
#endif

import Foreign
import Foreign.C

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI

--------------------------------------------------------------------------------

-- | A TrueType font file (containing maybe multiple font sets) loaded into memory.
newtype TrueType = TrueType ByteString

-- | A font offset inside a TrueType font file.
newtype Offset = Offset Int deriving Show

-- | A glyph inside a font.
newtype Glyph = Glyph Int deriving Show

ccodepoint :: Char -> CCodepoint
ccodepoint = fromIntegral . ord
 
cglyphindex :: Glyph -> CGlyphIndex
cglyphindex (Glyph i) = fromIntegral i

withTrueType :: TrueType -> (Ptr Word8 -> IO a) -> IO a
withTrueType (TrueType bs) action = withByteString bs action 

withByteString :: ByteString -> (Ptr Word8 -> IO a) -> IO a
withByteString bs action = withForeignPtr fptr h where
  (fptr,ofs,len) = BI.toForeignPtr bs
  h p = action (plusPtr p ofs) 

-- we need to refer to the the original font data here, 
-- otherwise it could be garbage collected !!!  
data FontInfo = FontInfo TrueType (ForeignPtr CFontInfo)

withFontInfo :: FontInfo -> (Ptr CFontInfo -> IO a) -> IO a
withFontInfo (FontInfo _ fptr) = withForeignPtr fptr

--------------------------------------------------------------------------------

-- | Enumerates the fonts found in a TrueType file.
enumerateFonts :: TrueType -> IO [Offset] 
enumerateFonts ttf = withTrueType ttf $ \ptr -> worker ptr 0 where
  worker ptr i = do
    o <- fromIntegral <$> stbtt_GetFontOffsetForIndex ptr i
    if o < 0 
      then return []
      else do
        os <- worker ptr (i+1) 
        return (Offset o : os)
    
initFont :: TrueType -> Offset -> IO FontInfo
initFont ttf (Offset ofs) = withTrueType ttf $ \ptr -> do
  fq <- mallocForeignPtr :: IO (ForeignPtr CFontInfo)
  withForeignPtr fq $ \q -> stbtt_InitFont q ptr (fromIntegral ofs)
  return (FontInfo ttf fq)    
    
--------------------------------------------------------------------------------

loadTTF :: FilePath -> IO TrueType
loadTTF fname = do
  bs <- B.readFile fname
  return (TrueType bs)

withTTF :: FilePath -> (TrueType -> IO a) -> IO a
withTTF fname action = do
  bs <- B.readFile fname
  action (TrueType bs)
  
--------------------------------------------------------------------------------

findGlyphIndex :: FontInfo -> Char -> IO (Maybe Glyph)
findGlyphIndex fontinfo char = 
  withFontInfo fontinfo $ \ptr -> do
    let codepoint = ord char
    i <- stbtt_FindGlyphIndex ptr (fromIntegral codepoint)
    return $ if i == 0 
      then Nothing
      else Just $ Glyph (fromIntegral i)

--------------------------------------------------------------------------------
  
type Unscaled = Int  
      
-- | 'ascent' is the coordinate above the baseline the font extends; 'descent'
-- is the coordinate below the baseline the font extends (i.e. it is typically negative)
-- 'lineGap' is the spacing between one row's descent and the next row's ascent...
-- so you should advance the vertical position by @ascent - descent + lineGap@      
data VerticalMetrics = VMetrics
  { ascent  :: Unscaled      
  , descent :: Unscaled
  , lineGap :: Unscaled
  }
  deriving Show
  
-- | As calculated by @(ascent - descent + lineGap)@.
lineAdvance :: VerticalMetrics -> Unscaled
lineAdvance vm = ascent vm - descent vm + lineGap vm  

-- | As calculated by @(ascent - descent)@.
verticalSize :: VerticalMetrics -> Unscaled
verticalSize vm = ascent vm - descent vm

scaleForPixelHeight :: VerticalMetrics -> Float -> Float 
scaleForPixelHeight vm pixels = pixels / fromIntegral (verticalSize vm)

-- 'leftSideBearing' is the offset from the current horizontal position to the left edge of the character;
-- 'advanceWidth' is the offset from the current horizontal position to the next horizontal position.
data HorizontalMetrics = HMetrics
  { advanceWidth     :: Unscaled      
  , leftSideBearing :: Unscaled
  }
  deriving Show
  
-- | The convention is @BBox (x0,y0) (x1,y1)@.
data BoundingBox a = BBox (a,a) (a,a) deriving Show

--------------------------------------------------------------------------------
     
getFontVerticalMetrics :: FontInfo -> IO VerticalMetrics
getFontVerticalMetrics fontinfo = 
  withFontInfo fontinfo $ \ptr -> do
    alloca $ \pasc -> alloca $ \pdesc -> alloca $ \pgap -> do
      stbtt_GetFontVMetrics ptr pasc pdesc pgap
      asc  <- peek pasc  :: IO CInt
      desc <- peek pdesc :: IO CInt
      gap  <- peek pgap  :: IO CInt
      return $ VMetrics
        { ascent  = fromIntegral asc
        , descent = fromIntegral desc
        , lineGap = fromIntegral gap
        }

--------------------------------------------------------------------------------

getGlyphHorizontalMetrics :: FontInfo -> Glyph -> IO HorizontalMetrics
getGlyphHorizontalMetrics fontinfo glyph = 
  withFontInfo fontinfo $ \ptr -> do
    alloca $ \padv -> alloca $ \plsb  -> do
      stbtt_GetGlyphHMetrics ptr (cglyphindex glyph) padv plsb
      adv <- peek padv :: IO CInt
      lsb <- peek plsb :: IO CInt
      return $ HMetrics
        { advanceWidth = fromIntegral adv
        , leftSideBearing = fromIntegral lsb
        }
        
-- | This is not yet implemented in @stb_truetype@; it always returns 0.
getGlyphKernAdvance :: FontInfo -> Glyph -> Glyph -> IO Unscaled
getGlyphKernAdvance fontinfo glyph1 glyph2 = 
  withFontInfo fontinfo $ \ptr -> do
    kern <- stbtt_GetGlyphKernAdvance ptr (cglyphindex glyph1) (cglyphindex glyph1)
    return (fromIntegral kern)
    
getGlyphBoundingBox :: FontInfo -> Glyph -> IO (BoundingBox Unscaled)
getGlyphBoundingBox fontinfo glyph =
  withFontInfo fontinfo $ \ptr -> do
    alloca $ \px0 -> alloca $ \py0 -> alloca $ \px1 -> alloca $ \py1 -> do
      stbtt_GetGlyphBox ptr (cglyphindex glyph) px0 py0 px1 py1
      x0 <- peek px0 :: IO CInt
      y0 <- peek py0 :: IO CInt
      x1 <- peek px1 :: IO CInt
      y1 <- peek py1 :: IO CInt
      return $ BBox 
        (fromIntegral x0, fromIntegral y0)
        (fromIntegral x1, fromIntegral y1)
        
--------------------------------------------------------------------------------

type Scaling = (Float,Float)

-- | A 8-bit grayscale bitmap.
data Bitmap = Bitmap
  { bitmapSize :: (Int,Int)
  , bitmapPtr  :: ForeignPtr Word8
  }

-- | An offset (for example the pivot of the glyph) 
type BitmapOfs = (Int,Int)
  
newBitmap :: (Int,Int) -> IO Bitmap
newBitmap siz@(xsiz,ysiz) = do
  let n = xsiz*ysiz
  fptr <- mallocForeignPtrBytes n
  withForeignPtr fptr $ \ptr -> pokeArray ptr (replicate n 0)
  return (Bitmap siz fptr)
    
withBitmap :: Bitmap -> (Int -> Int -> Ptr Word8 -> IO a) -> IO a
withBitmap bm action = do
  let (xsiz,ysiz) = bitmapSize bm
  withForeignPtr (bitmapPtr bm) $ \ptr -> action xsiz ysiz ptr  

-- | NOTE: because of the way Haskell indexes rectangular arrays,
-- the resulting array is indexed with @(y,x)@, as opposed to what
-- you would expect.
bitmapArray :: Bitmap -> IO (UArray (Int,Int) Word8)
bitmapArray bm = 
  withBitmap bm $ \xsiz ysiz ptr -> do
    ar <- newArray_ ((0,0),(ysiz-1,xsiz-1)) :: IO (IOUArray (Int,Int) Word8)
    forM_ [0..ysiz-1] $ \y -> do
      let k = y*xsiz 
      forM_ [0..xsiz-1] $ \x -> do
        a <- peekElemOff ptr (k+x)
#ifdef __GLASGOW_HASKELL__ 
        Arr.unsafeWrite ar (k+x) a
#else
        writeArray ar (y,x) a
#endif
    unsafeFreeze ar
                             
bitmapFloatArray :: Bitmap -> IO (UArray (Int,Int) Float)
bitmapFloatArray bm = 
  withBitmap bm $ \xsiz ysiz ptr -> do
    let factor = 1.0 / 255 :: Float
    ar <- newArray_ ((0,0),(ysiz-1,xsiz-1)) :: IO (IOUArray (Int,Int) Float)
    forM_ [0..ysiz-1] $ \y -> do
      let k = y*xsiz 
      forM_ [0..xsiz-1] $ \x -> do
        a <- peekElemOff ptr (k+x)
        let z = fromIntegral a * factor
#ifdef __GLASGOW_HASKELL__ 
        Arr.unsafeWrite ar (k+x) z
#else
        writeArray ar (y,x) z
#endif
    unsafeFreeze ar

-- | Returns the size of the bitmap (in pixels) needed to 
-- render the glyph with the given scaling.
--
-- The box is centered around the glyph origin; so the
-- bitmap width is @x1-x0@, height is @y1-y0@, and location to place
-- the bitmap top left is @(leftSideBearing*scale,y0)@.
-- Note that the bitmap uses /y-increases-down/, but the shape uses
-- /y-increases-up/, so the results of 'getGlyphBitmapBox' and 
-- 'getGlyphBoundingBox' are inverted.
getGlyphBitmapBox :: FontInfo -> Glyph -> Scaling -> IO (BoundingBox Int)
getGlyphBitmapBox fontinfo glyph (xscale,yscale) =
  withFontInfo fontinfo $ \ptr -> do
    alloca $ \px0 -> alloca $ \py0 -> alloca $ \px1 -> alloca $ \py1 -> do
      stbtt_GetGlyphBitmapBox ptr (cglyphindex glyph) (realToFrac xscale) (realToFrac yscale) px0 py0 px1 py1
      x0 <- peek px0 :: IO CInt
      y0 <- peek py0 :: IO CInt
      x1 <- peek px1 :: IO CInt
      y1 <- peek py1 :: IO CInt
      return $ BBox 
        (fromIntegral x0, fromIntegral y0)
        (fromIntegral x1, fromIntegral y1)

-- | Creates a new bitmap just enough to fit the glyph with the given scaling,
-- and renders the glyph into it. The offset returned is the offset of the glyph origin
-- within the bitmap.
newGlyphBitmap :: FontInfo -> Glyph -> Scaling -> IO (Bitmap,BitmapOfs)
newGlyphBitmap fontinfo glyph (xscale,yscale) = do
  withFontInfo fontinfo $ \ptr -> do
    alloca $ \pxsiz -> alloca $ \pysiz -> alloca $ \pxofs -> alloca $ \pyofs -> do
      pbm <- stbtt_GetGlyphBitmap ptr 
        (realToFrac xscale) (realToFrac yscale) 
        (cglyphindex glyph) 
        pxsiz pysiz pxofs pyofs
      xsiz <- peek pxsiz :: IO CInt
      ysiz <- peek pysiz :: IO CInt
      xofs <- peek pxofs :: IO CInt
      yofs <- peek pyofs :: IO CInt
      fpbm <- newForeignPtr bitmapFinalizer pbm
      let bm  = Bitmap (fromIntegral xsiz, fromIntegral ysiz) fpbm
          ofs = (fromIntegral xofs, fromIntegral yofs)
      return $ (bm,ofs)

-- | The offset is the /top-left corner/ of the bounding box of the glyph,
-- and must be nonnegative (otherwise nothing will happen).
renderGlyphIntoBitmap' :: FontInfo -> Glyph -> Scaling -> Bitmap -> BitmapOfs -> IO ()
renderGlyphIntoBitmap' fontinfo glyph (xscale,yscale) bm (xofs,yofs) = do
  let (xsiz,ysiz) = bitmapSize bm 
  when ( xofs < xsiz && yofs < ysiz && xofs >= 0 && yofs >= 0 ) $ do
    withFontInfo fontinfo $ \ptr -> do
      withBitmap bm $ \width height pbm -> do
        let pbm' = pbm `plusPtr` (width*yofs+xofs)
        stbtt_MakeGlyphBitmap ptr pbm'
          (fromIntegral $ width - xofs) (fromIntegral $ height - yofs)
          (fromIntegral width)  -- stride
          (realToFrac xscale) (realToFrac yscale) 
          (cglyphindex glyph)        

-- | The offset is the /origin/ of the glyph. If the glyph extends from the
-- bitmap in the positive direction, it is clipped; however, if it extends
-- in the negative direction, no drawing will happen!
renderGlyphIntoBitmap ::  FontInfo -> Glyph -> Scaling -> Bitmap -> BitmapOfs -> IO ()
renderGlyphIntoBitmap fontinfo glyph scaling@(xscale,yscale) bm ofs@(xofs,yofs) = do
  BBox (x0,y0) _ <- getGlyphBitmapBox fontinfo glyph scaling
  renderGlyphIntoBitmap' fontinfo glyph scaling bm (xofs+x0,yofs+y0) 
         
--------------------------------------------------------------------------------

{-
typedef struct
{
   unsigned char  *data;         // pointer to .ttf file
   int             fontstart;    // offset of start of font

   int numGlyphs;                // number of glyphs, needed for range checking

   int loca,head,glyf,hhea,hmtx; // table locations as offset from start of .ttf
   int index_map;                // a cmap mapping for our chosen character encoding
   int indexToLocFormat;         // format needed to map from glyph index to glyph
} stbtt_fontinfo;
-}

data TableLoc = TableLoc 
  { _loca :: Int
  , _head :: Int
  , _glyf :: Int
  , _hhea :: Int
  , _hmtx :: Int
  }

instance Storable TableLoc where 
  alignment _ = alignment (undefined :: CInt)
  sizeOf _    = 5 * sizeOf (undefined :: CInt)
  peek = error "TableLoc/peek: not implemented"
  poke = error "TableLoc/poke: not implemented"

data CFontInfo = CFontInfo
  { _data      :: Ptr Word8
  , _fontstart :: Int 
  , _numGlyphs :: Int
  , _tableloc  :: TableLoc
  , _indexMap  :: Int
  , _indexToLocFmt :: Int
  }
  
instance Storable CFontInfo where 
  alignment _ = alignment (undefined :: CInt)
  sizeOf _    = sizeOf (undefined :: Ptr Word8)
              + sizeOf (undefined :: TableLoc)
              + 4 * sizeOf (undefined :: CInt)
  peek = error "CFontInfo/peek: not implemented"            
  poke = error "CFontInfo/poke: not implemented"       
              
--------------------------------------------------------------------------------

type CCodepoint  = CInt
type CGlyphIndex = CInt

foreign import ccall unsafe "stb_truetype.h stbtt_GetFontOffsetForIndex" 
  stbtt_GetFontOffsetForIndex :: Ptr Word8 -> CInt -> IO CInt
  
foreign import ccall unsafe "stb_truetype.h stbtt_InitFont"
  stbtt_InitFont :: Ptr CFontInfo -> Ptr Word8 -> CInt -> IO CInt

-------

foreign import ccall unsafe "stb_truetype.h stbtt_FindGlyphIndex"
  stbtt_FindGlyphIndex :: Ptr CFontInfo -> CCodepoint -> IO CGlyphIndex

-------

-- foreign import ccall unsafe "stb_truetype.h stbtt_ScaleForPixelHeight"
--   stbtt_ScaleForPixelHeight :: Ptr CFontInfo -> CFloat -> IO CFloat

foreign import ccall unsafe "stb_truetype.h stbtt_GetFontVMetrics"
  stbtt_GetFontVMetrics :: Ptr CFontInfo -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO () 

{-
foreign import ccall unsafe "stb_truetype.h stbtt_GetCodepointHMetrics"
  stbtt_GetCodepointHMetrics :: Ptr CFontInfo -> CCodepoint -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe "stb_truetype.h stbtt_GetCodepointKernAdvance"
  stbtt_GetCodepointKernAdvance :: Ptr CFontInfo -> CCodepoint -> CCodepoint -> IO CInt

foreign import ccall unsafe "stb_truetype.h stbtt_GetCodepointBox"
  stbtt_GetCodepointBox 
    :: Ptr CFontInfo -> CCodepoint -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt
-}

foreign import ccall unsafe "stb_truetype.h stbtt_GetGlyphHMetrics"
  stbtt_GetGlyphHMetrics :: Ptr CFontInfo -> CGlyphIndex -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe "stb_truetype.h stbtt_GetGlyphKernAdvance"
  stbtt_GetGlyphKernAdvance :: Ptr CFontInfo -> CGlyphIndex -> CGlyphIndex -> IO CInt

foreign import ccall unsafe "stb_truetype.h stbtt_GetGlyphBox"
  stbtt_GetGlyphBox 
    :: Ptr CFontInfo -> CGlyphIndex -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-------

foreign import ccall unsafe "stb_truetype.h stbtt_FreeBitmap"
  stbtt_FreeBitmap :: Ptr Word8 -> IO () 

foreign import ccall unsafe "stb_truetype.h &stbtt_FreeBitmap"
  bitmapFinalizer :: FunPtr (Ptr Word8 -> IO ())
   
{- 
foreign import ccall unsafe "stb_truetype.h stbtt_GetCodepointBitmap"
  stbtt_GetCodepointBitmap 
    :: Ptr CFontInfo -> CFloat -> CFloat -> CCodepoint 
    -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe "stb_truetype.h stbtt_MakeCodepointBitmap"
  stbtt_MakeCodepointBitmap 
    :: Ptr CFontInfo -> Ptr Word8 -> CInt -> CInt -> CInt
    -> CFloat -> CFloat -> CCodepoint -> IO ()

-- (Note that the bitmap uses y-increases-down, but the shape uses
-- y-increases-up, so CodepointBitmapBox and CodepointBox are inverted.)
foreign import ccall unsafe "stb_truetype.h stbtt_GetCodepointBitmapBox"
  stbtt_GetCodepointBitmapBox 
    :: Ptr CFontInfo -> CCodepoint -> CFloat -> CFloat
    -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
-}

foreign import ccall unsafe "stb_truetype.h stbtt_GetGlyphBitmap"
  stbtt_GetGlyphBitmap 
    :: Ptr CFontInfo -> CFloat -> CFloat -> CGlyphIndex 
    -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall unsafe "stb_truetype.h stbtt_MakeGlyphBitmap"
  stbtt_MakeGlyphBitmap 
    :: Ptr CFontInfo -> Ptr Word8 -> CInt -> CInt -> CInt
    -> CFloat -> CFloat -> CGlyphIndex -> IO ()

-- (Note that the bitmap uses y-increases-down, but the shape uses
-- y-increases-up, so GlyphBitmapBox and GlyphBox are inverted.)
foreign import ccall unsafe "stb_truetype.h stbtt_GetGlyphBitmapBox"
  stbtt_GetGlyphBitmapBox 
    :: Ptr CFontInfo -> CGlyphIndex -> CFloat -> CFloat
    -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

--------------------------------------------------------------------------------

  