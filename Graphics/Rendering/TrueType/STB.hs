
-- TODO: 
--   * rewrite the file loading so that it we do not depend on ByteString
--   * automatic glyph indexing, texture creation
--
-- BUG in stb_truetype? 
--   * compound contours not implemented yet... 
--

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
--
-- Note: it seems that compound glyphs are not implemented yet!

{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# CFILES cbits/wrapper.c #-}  -- for Hugs (?)
module Graphics.Rendering.TrueType.STB 
  ( TrueType
  , Offset
  , Font
  , Glyph
  -- * Initialization
  , loadTTF
  , withTTF
  , enumerateFonts
  , initFont
  , findGlyph
  -- * Font metrics
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
  -- * Bitmaps
  , Scaling
  , Bitmap(..)
  , newBitmap
  , withBitmap
  , flipBitmap
  , BitmapOfs
  , getGlyphBitmapBox
  , newGlyphBitmap
  , renderGlyphIntoBitmap'
  , renderGlyphIntoBitmap
  , bitmapArray
  , bitmapFloatArray
  -- * Cached glyph storage
  , CachedBitmap(..)
  , BitmapCache
  , bmcVerticalMetrics
  , bmcScaling
  , newBitmapCache
  , getCachedBitmap
  
  ) where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Applicative
import Control.Concurrent.MVar

import Data.Char
import Data.Maybe

import Data.Array
import Data.Array.IO
import Data.Array.Unboxed
#ifdef __GLASGOW_HASKELL__
import qualified Data.Array.Base as Arr 
#endif

--import Data.Map (Map)
--import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Foreign hiding (newArray)
import Foreign.C

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI

--------------------------------------------------------------------------------

-- | A TrueType font file (containing maybe multiple font sets) loaded into memory.
newtype TrueType = TrueType ByteString

-- | A font offset inside a TrueType font file.
newtype Offset = Offset Int deriving (Eq,Ord,Show)

-- | A glyph inside a font.
newtype Glyph = Glyph Int deriving (Eq,Ord,Show)

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
data Font = Font 
  { _fontData :: TrueType 
  , _fontInfo :: ForeignPtr CFontInfo
  , _glyphMap :: UnicodeCache (Maybe Glyph) 
  } 

withFontInfo :: Font -> (Ptr CFontInfo -> IO a) -> IO a
withFontInfo (Font _ fptr _) = withForeignPtr fptr

--------------------------------------------------------------------------------

-- Organized into small continous blocks (say 128 characters)
-- so lookup should pretty very fast
type UnicodeCache a = MVar (IntMap (IOArray Char (Maybe a)))

unicodeCacheGranularity = 128 :: Int

newUnicodeCache :: IO (UnicodeCache a)
newUnicodeCache = newMVar (IntMap.empty)

lookupUnicodeCache :: Char -> (Char -> IO a) -> UnicodeCache a -> IO a
lookupUnicodeCache char calculate cache = do
  themap <- takeMVar cache  
  let k = ord char `div` unicodeCacheGranularity
  case IntMap.lookup k themap of
    Just arr -> do
      putMVar cache themap
      mvalue <- readArray arr char
      case mvalue of
        Just value -> do
          return value
        Nothing -> do
          new <- calculate char
          writeArray arr char (Just new)
          return new         
    Nothing -> do
      let u = k*unicodeCacheGranularity
      let v = u + unicodeCacheGranularity - 1
      arr <- newArray (chr u, chr v) Nothing
      new <- calculate char
      writeArray arr char (Just new)
      putMVar cache (IntMap.insert k arr themap)
      return new
      
--------------------------------------------------------------------------------

-- | Enumerates the fonts found in a TrueType file. Often there is only one,
-- but there may be more.
enumerateFonts :: TrueType -> IO [Offset] 
enumerateFonts ttf = withTrueType ttf $ \ptr -> worker ptr 0 where
  worker ptr i = do
    o <- fromIntegral <$> stbtt_GetFontOffsetForIndex ptr i
    if o < 0 
      then return []
      else do
        os <- worker ptr (i+1) 
        return (Offset o : os)
    
initFont :: TrueType -> Offset -> IO Font
initFont ttf (Offset ofs) = withTrueType ttf $ \ptr -> do
  fq <- mallocForeignPtr :: IO (ForeignPtr CFontInfo)
  withForeignPtr fq $ \q -> stbtt_InitFont q ptr (fromIntegral ofs)
  mglyphmap <- newUnicodeCache -- newMVar Map.empty
  return (Font ttf fq mglyphmap)    
    
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

-- | Note: this is cached.
findGlyph :: Font -> Char -> IO (Maybe Glyph)
findGlyph fontinfo@(Font _ _ glyphmap) char =
  lookupUnicodeCache char (findGlyphNotCached fontinfo) glyphmap 

-- this is not cached
findGlyphNotCached :: Font -> Char -> IO (Maybe Glyph)
findGlyphNotCached fontinfo char =
  withFontInfo fontinfo $ \ptr -> do
    let codepoint = ord char
    i <- stbtt_FindGlyphIndex ptr (fromIntegral codepoint)
    if i == 0 
      then return Nothing
      else do
        let glyph = Glyph (fromIntegral i)
        return (Just glyph)

--------------------------------------------------------------------------------

-- | Note: the metrics are scaled!
data CachedBitmap = CBM Bitmap BitmapOfs (HorizontalMetrics Float)

-- | A \"bitmap cache\".
data BitmapCache = BMCache
  { bmc_fontinfo :: Font
  , bmc_scaling  :: (Float,Float)
  , bmc_cache    :: UnicodeCache (Maybe CachedBitmap)
  , bmc_vmetrics :: VerticalMetrics Float
  , bmc_flipped  :: Bool
  }

-- | Note: these metrics are scaled!
bmcVerticalMetrics :: BitmapCache -> VerticalMetrics Float
bmcVerticalMetrics = bmc_vmetrics

bmcScaling :: BitmapCache -> Scaling
bmcScaling = bmc_scaling

-- | Creates a new cache where glyph bitmaps with the given scaling
-- will be stored. The second argument is whether the resulting bitmaps
-- should be flipped vertically or not (this is useful with OpenGL).
newBitmapCache :: Font -> Bool -> (Float,Float) -> IO BitmapCache 
newBitmapCache fontinfo flipped scaling@(xscale,yscale) = do 
  cache <- newUnicodeCache
  vmetu <- getFontVerticalMetrics fontinfo
  let vmets = fmap (\y -> yscale * fromIntegral y) vmetu  
  return $ BMCache fontinfo scaling cache vmets flipped
  
-- | Fetches a rendered glyph bitmap from the cache (rendering it first if
-- it was not present in the cache before).
getCachedBitmap :: BitmapCache -> Char -> IO (Maybe CachedBitmap)
getCachedBitmap (BMCache font scaling@(xscale,yscale) cache vmet flipped) char = 
  lookupUnicodeCache char createBitmap cache
  where
    createBitmap char = do
      mglyph <- findGlyph font char
      case mglyph of
        Just glyph -> do
          (bm',ofs) <- newGlyphBitmap font glyph scaling
          bm <- if flipped then flipBitmap bm' else return bm'
          hmetu <- getGlyphHorizontalMetrics font glyph
          let hmets = fmap (\x -> xscale * fromIntegral x) hmetu 
          return $ Just (CBM bm ofs hmets)
        Nothing -> do
          return Nothing
        
--------------------------------------------------------------------------------
  
type Unscaled = Int  
      
-- | 'ascent' is the coordinate above the baseline the font extends; 'descent'
-- is the coordinate below the baseline the font extends (i.e. it is typically negative)
-- 'lineGap' is the spacing between one row's descent and the next row's ascent...
-- so you should advance the vertical position by @ascent - descent + lineGap@      
data VerticalMetrics a = VMetrics
  { ascent  :: a   
  , descent :: a
  , lineGap :: a
  }
  deriving Show
  
instance Functor VerticalMetrics where
  fmap f (VMetrics a d l) = VMetrics (f a) (f d) (f l)  
  
-- | As calculated by @(ascent - descent + lineGap)@.
lineAdvance :: Num a => VerticalMetrics a -> a
lineAdvance vm = ascent vm - descent vm + lineGap vm  

-- | As calculated by @(ascent - descent)@.
verticalSize :: Num a => VerticalMetrics a -> a 
verticalSize vm = ascent vm - descent vm

scaleForPixelHeight :: VerticalMetrics Unscaled -> Float -> Float 
scaleForPixelHeight vm pixels = pixels / fromIntegral (verticalSize vm)

-- 'leftSideBearing' is the offset from the current horizontal position to the left edge of the character;
-- 'advanceWidth' is the offset from the current horizontal position to the next horizontal position.
data HorizontalMetrics a = HMetrics
  { advanceWidth    :: a      
  , leftSideBearing :: a
  }
  deriving Show

instance Functor HorizontalMetrics where
  fmap f (HMetrics a l) = HMetrics (f a) (f l)  
  
-- | The convention is @BBox (x0,y0) (x1,y1)@.
data BoundingBox a = BBox (a,a) (a,a) deriving Show

--------------------------------------------------------------------------------
     
getFontVerticalMetrics :: Font -> IO (VerticalMetrics Unscaled)
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

getGlyphHorizontalMetrics :: Font -> Glyph -> IO (HorizontalMetrics Unscaled)
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
getGlyphKernAdvance :: Font -> Glyph -> Glyph -> IO Unscaled
getGlyphKernAdvance fontinfo glyph1 glyph2 = 
  withFontInfo fontinfo $ \ptr -> do
    kern <- stbtt_GetGlyphKernAdvance ptr (cglyphindex glyph1) (cglyphindex glyph1)
    return (fromIntegral kern)
    
getGlyphBoundingBox :: Font -> Glyph -> IO (BoundingBox Unscaled)
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

-- | Flips the bitmap vertically (leaving the original unchanged)
flipBitmap :: Bitmap -> IO Bitmap
flipBitmap (Bitmap siz@(xsiz,ysiz) fptr1) = withForeignPtr fptr1 $ \ptr1 -> do
  let n = xsiz*ysiz
  fptr2 <- mallocForeignPtrBytes n
  withForeignPtr fptr2 $ \ptr2 -> do
    forM_ [0..ysiz-1] $ \y1 -> do
      let y2 = ysiz-1-y1
      copyBytes (ptr2 `plusPtr` (y2*xsiz)) (ptr1 `plusPtr` (y1*xsiz)) xsiz
  return (Bitmap siz fptr2)


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
getGlyphBitmapBox :: Font -> Glyph -> Scaling -> IO (BoundingBox Int)
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
-- and renders the glyph into it. The offset returned is the offset 
-- in pixel space /from/ the glyph origin /to/ the top-left of the bitmap
-- (so it's almost always negative).
newGlyphBitmap :: Font -> Glyph -> Scaling -> IO (Bitmap,BitmapOfs)
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
renderGlyphIntoBitmap' :: Font -> Glyph -> Scaling -> Bitmap -> BitmapOfs -> IO ()
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
renderGlyphIntoBitmap ::  Font -> Glyph -> Scaling -> Bitmap -> BitmapOfs -> IO ()
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

  