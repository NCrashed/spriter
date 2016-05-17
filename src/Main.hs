module Main where

import Codec.Picture
import Data.Either (rights)
import Data.Monoid ((<>))

import Spriter.Camera 
import Spriter.Math 
import Spriter.Voxel

testCamera :: Camera 
testCamera = Camera {
  cameraEye = Vec3 0.5 (-0.2) 0.5
, cameraForward = normalize $ Vec3 0.5 (-1) 0
, cameraUp = Vec3 0 0 1
}

testScene :: Int -> IO VoxelModel 
testScene layersCount = do
  layers <- mapM readImage $ (\i -> show i <> ".png") <$> [0 .. layersCount-1]
  return VoxelModel {
    voxModelPos = Vec3 0 0 0
  , voxModelScale = Vec3 1 0.3 1
  , voxModelGrid = gridFromImagesXZ $ convertRGBA8 <$> rights layers
  }

renderSpriteToFile :: FilePath -> IO ()
renderSpriteToFile path = do 
  scene <- testScene 7
  writePng path $ generateImage (renderSprite scene) width height
  where 
  width = 255
  height = 255
  fov = pi / 3
  renderSprite scene x y = case r `traverseModel` scene of 
    Nothing -> PixelRGB8 0 0 0
    Just (PixelRGBA8 r g b _) -> PixelRGB8 r g b
    where
    r = screenRayPersp testCamera width height x y fov

main :: IO ()
main = renderSpriteToFile "sprite.png"

