module Main where

import Codec.Picture

import Spriter.Camera 
import Spriter.Math 
import Spriter.Voxel

testCamera :: Camera 
testCamera = Camera {
  cameraEye = Vec3 (-3) 3 (-3)
, cameraForward = normalize $ Vec3 1 (-1) 1
, cameraUp = Vec3 0 1 0
}

testScene :: Box 
testScene = Box {
  boxMin = Vec3 (-1) (-1) (-1)
, boxMax = Vec3 1 1 1
}

renderSpriteToFile :: FilePath -> IO ()
renderSpriteToFile path = writePng path $ generateImage renderSprite width height
  where 
  width = 285
  height = 255
  fov = pi / 6
  renderSprite x y = PixelRGB8 c c 0
    where 
    mis = screenRayPersp testCamera width height x y fov `rayBoxIntersect` testScene
    c = case mis of 
      Nothing -> 0
      Just d -> round $ min 255 $ max 0 $ lerp 0 255 (1 - 0.5*(d-3))


main :: IO ()
main = renderSpriteToFile "sprite.png"

