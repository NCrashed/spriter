module Spriter.Voxel(
    VoxelGrid(..)
  , emptyGrid
  , setVoxel
  , getVoxel
  , VoxelModel(..)
  , traverseModel
  , gridFromLayersXZ
  , gridFromImagesXZ
  ) where 

import GHC.Generics 
import Control.DeepSeq
import Data.Word 
import Codec.Picture 
import Data.Bits 
import Data.List 
import Control.Lens 

import qualified Data.Vector.Unboxed as VU 

import Spriter.Math 


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

-- | Test if coords in box range
inGridRange :: Int -> Int -> Int -> VoxelGrid -> Bool 
inGridRange x y z g = x >= 0 && x < gridXSize g && y >= 0 && y < gridYSize g && z >= 0 && z < gridZSize g

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
getVoxel x y z g = if inGridRange x y z g
  then fromItem $ gridData g `VU.unsafeIndex` i
  else PixelRGBA8 0 0 0 0 
  where
    i = toIndex x y z g 

-- | Go withing 3d grid of voxels and return first the ray hit
traverseGrid :: Ray -> VoxelGrid -> Maybe PixelRGBA8
traverseGrid Ray{..} grid@VoxelGrid{..} = if not (inGridRange x0 y0 z0 grid) || c0 == ec 
    then go x0 y0 z0 mx0 my0 mz0 
    else Just c0
  where 
  (Vec3 x0' y0' z0') = rayOrigin
  (Vec3 dx' dy' dz') = rayDirection
    
  x0 = floor (x0' / wx)
  y0 = floor (y0' / wy)
  z0 = floor (z0' / wz)
  c0 = getVoxel x0 y0 z0 grid

  stepx, stepy, stepz :: Int
  stepx = floor $ signum dx'
  stepy = floor $ signum dy'
  stepz = floor $ signum dz'

  mx0 = if stepx < 0 then (fromIntegral x0 * wx - x0') / dx' 
                  else (fromIntegral (x0 + 1) * wx - x0') / dx'
  my0 = if stepy < 0 then (fromIntegral y0 * wy - y0') / dy' 
                  else (fromIntegral (y0 + 1) * wy - y0') / dy'
  mz0 = if stepz < 0 then (fromIntegral z0 * wz - z0') / dz' 
                  else (fromIntegral (z0 + 1) * wz - z0') / dz'

  outx = if stepx > 0 then gridXSize+1 else (-1)
  outy = if stepy > 0 then gridYSize+1 else (-1)
  outz = if stepz > 0 then gridZSize+1 else (-1)

  wx = 1 / fromIntegral gridXSize
  wy = 1 / fromIntegral gridYSize
  wz = 1 / fromIntegral gridZSize

  dx, dy, dz :: Float
  dx = abs $ fromIntegral stepx * wx / dx'
  dy = abs $ fromIntegral stepy * wy / dy'
  dz = abs $ fromIntegral stepz * wz / dz'

  ec = PixelRGBA8 0 0 0 0 

  go x y z mx my mz = if mx < my 
    then if mx < mz then xcase else zcase
    else if my < mz then ycase else zcase
    where 
    xcase = let
      x' = x + stepx
      c = getVoxel x' y z grid
      in if x' == outx then Nothing 
         else if c == ec then go x' y z (mx + dx) my mz 
              else Just c
    ycase = let 
      y' = y + stepy 
      c = getVoxel x y' z grid
      in if y' == outy then Nothing 
         else if c == ec then go x y' z mx (my + dy) mz
              else Just c
    zcase = let 
      z' = z + stepz
      c = getVoxel x y z' grid 
      in if z' == outz then Nothing 
         else if c == ec then go x y z' mx my (mz + dz)
              else Just c

data VoxelModel = VoxelModel {
  voxModelGrid :: !VoxelGrid
, voxModelPos :: !Vec3 
, voxModelScale :: !Vec3 
} deriving Generic 

instance NFData VoxelModel

-- | Get bounding box of the model
voxelModelBox :: VoxelModel -> Box 
voxelModelBox VoxelModel{..} = Box {
    boxMin = voxModelPos
  , boxMax = voxModelPos + voxModelScale
  }
  where 
  Vec3 sx sy sz = voxModelScale

-- | Get color at intersection of ray and model
traverseModel :: Ray -> VoxelModel -> Maybe PixelRGBA8
traverseModel r m@VoxelModel{..} = case r `rayBoxIntersect` voxelModelBox m of 
  Nothing -> Nothing
  Just d -> traverseGrid r' voxModelGrid
    where 
    VoxelGrid{..} = voxModelGrid
    r' = rescaleRay voxModelScale 
      $ rebaseRay voxModelPos 
      $ restartRay (d + 0.0000001) r

-- | Fill voxel layer from XZ layers
gridFromLayersXZ :: [[[PixelRGBA8]]] -> VoxelGrid
gridFromLayersXZ layers = foldl' addLayer (emptyGrid maxx maxy maxx) $ layers `zip` [0..]
  where 
  maxz = length layers
  maxy = maximum $ length <$> layers
  maxx = maximum . concat $ fmap length <$> layers

  addLayer acc (layer, y) = foldl' (addRow y) acc $ layer `zip` [0..]
  addRow y acc (row, z) = foldl' (addVoxel y z) acc $ row `zip` [0..]
  addVoxel y z acc (c, x) = setVoxel x y z c acc

-- | Fill voxel layer from XZ layers
gridFromImagesXZ :: [Image PixelRGBA8] -> VoxelGrid
gridFromImagesXZ layers = foldl' addLayer (emptyGrid maxx maxy maxx) $ layers `zip` [0..]
  where 
  maxz = length layers
  maxy = maximum $ imageHeight <$> layers
  maxx = maximum $ imageWidth <$> layers

  addLayer acc (layer, y) = fst $ mapAccumLOf imageIPixels (addVoxel y) acc layer 
  addVoxel y acc (x, z, c) = (setVoxel x y z c acc, c)