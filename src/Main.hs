module Main where

import Codec.Picture

import Spriter.Camera 
import Spriter.Math 
import Spriter.Voxel

testCamera :: Camera 
testCamera = Camera {
  cameraEye = Vec3 0 0.7 0
, cameraForward = normalize $ Vec3 1 0 1
, cameraUp = Vec3 0 1 0
}

testScene :: VoxelModel 
testScene = VoxelModel {
    voxModelPos = Vec3 0 0 0
  , voxModelScale = Vec3 1 1 1
  , voxModelGrid = grid 
  }
  where
  grid = gridFromLayersXZ [
      [ [z, z]
      , [b, g] ]
    , [ [z, r] 
      , [z, y] ]
    ]
  z = PixelRGBA8 0 0 0 0
  r = PixelRGBA8 255 0 0 0
  g = PixelRGBA8 0 255 0 0
  b = PixelRGBA8 0 0 255 0
  y = PixelRGBA8 200 200 0 0

renderSpriteToFile :: FilePath -> IO ()
renderSpriteToFile path = writePng path $ generateImage renderSprite width height
  where 
  width = 255
  height = 255
  fov = pi / 3
  renderSprite x y = case r `traverseModel` testScene of 
    Nothing -> PixelRGB8 0 0 0
    Just (PixelRGBA8 r g b _) -> PixelRGB8 r g b
    where
    r = screenRayPersp testCamera width height x y fov

main :: IO ()
main = renderSpriteToFile "sprite.png"

