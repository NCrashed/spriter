module Spriter.Voxel(
    VoxelGrid(..)
  , emptyGrid
  , setVoxel
  , getVoxel
  ) where 

import GHC.Generics 
import Control.DeepSeq
import Data.Word 
import Codec.Picture 
import Data.Bits 

import qualified Data.Vector.Unboxed as VU 

data VoxelGrid = VoxelGrid { 
  gridData :: !(VU.Vector Word32)
, gridXSize :: !Int 
, gridYSize :: !Int 
, gridZSize :: !Int
} deriving Generic 

instance NFData VoxelGrid

-- | To pos in data array
toIndex :: Int -> Int -> Int -> VoxelGrid -> Int 
toIndex x y z VoxelGrid{..} = x + gridXSize * y + gridXSize * gridYSize * z

-- | From pos in data array
fromIndex :: Int -> VoxelGrid -> (Int, Int, Int)
fromIndex i VoxelGrid{..} = (i' `mod` gridXSize, i' `div` gridXSize, i `div` gridXSize * gridYSize)
  where i' = i `mod` gridXSize * gridYSize

-- | Check if given index inbounds of array
inRange :: Int -> VoxelGrid -> Bool
inRange i VoxelGrid{..} = i >= 0 && i < gridXSize * gridYSize * gridZSize

-- | Encode color
toItem :: PixelRGBA8 -> Word32 
toItem (PixelRGBA8 r g b a) = r' + shiftL g' 8 + shiftL b' 16 + shiftL a' 24
  where 
  r' = fromIntegral r 
  g' = fromIntegral g 
  b' = fromIntegral b 
  a' = fromIntegral a 

-- | Decode color
fromItem :: Word32 -> PixelRGBA8
fromItem w = PixelRGBA8 r g b a
  where 
  r = fromIntegral $  w .&. 0x000000ff
  g = fromIntegral $ (w .&. 0x0000ff00) `shiftR` 8
  b = fromIntegral $ (w .&. 0x00ff0000) `shiftR` 16
  a = fromIntegral $ (w .&. 0xff000000) `shiftR` 24

-- | Generate empty grid with given shape
emptyGrid :: Int -> Int -> Int -> VoxelGrid
emptyGrid xsize ysize zsize = VoxelGrid {
    gridData = VU.replicate (xsize*ysize*zsize) 0 
  , gridXSize = xsize
  , gridYSize = ysize
  , gridZSize = zsize
  }

-- | Setting single voxel at given pos
setVoxel :: Int -> Int -> Int -> PixelRGBA8 -> VoxelGrid -> VoxelGrid
setVoxel x y z c g = if inRange i g 
  then gd' `seq` g {
      gridData = gd'
    } 
  else g 
  where 
    gd' = gridData g `VU.unsafeUpd` [(i, toItem c)]
    i = toIndex x y z g 

-- | Setting single voxel at given pos, returns 0 if out of bounds
getVoxel :: Int -> Int -> Int -> VoxelGrid -> PixelRGBA8
getVoxel x y z g = if inRange i g 
  then fromItem $ gridData g `VU.unsafeIndex` i
  else PixelRGBA8 0 0 0 0 
  where
    i = toIndex x y z g 

-- traverseGrid :: Ray -> VoxelGrid -> PixelRGBA8