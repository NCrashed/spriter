module Spriter.Camera(
    Camera(..)
  , cameraLeft 
  , screenRayOrtho
  , screenRayPersp
  ) where 

import GHC.Generics 
import Control.DeepSeq

import Spriter.Math 

data Camera = Camera {
  cameraEye :: !Vec3
, cameraForward :: !Vec3 
, cameraUp :: !Vec3 
} deriving (Show, Generic)

instance NFData Camera 

cameraLeft :: Camera -> Vec3 
cameraLeft Camera{..} = cameraForward `cross` cameraUp

-- | Transforms point of screen into ray
screenRayOrtho :: Camera 
  -> Int -- ^ Width of screen
  -> Int -- ^ Height of screen
  -> Int -- ^ X of screen
  -> Int -- ^ Y of screen
  -> Ray -- ^ Ray from point of screen 
screenRayOrtho c w h x y = Ray{..}
  where 
  aspect = fromIntegral w / fromIntegral h
  x' = (fromIntegral (2*x) / fromIntegral w - 1) * aspect
  y' = negate $ fromIntegral (2*y) / fromIntegral h - 1
  offsetx = cameraUp c `scale` y'
  offsety = cameraLeft c `scale` x' 
  rayOrigin = cameraEye c + offsetx + offsety
  rayDirection = cameraForward c

-- | Transforms point of screen into ray with perspective
screenRayPersp :: Camera 
  -> Int -- ^ Width of screen
  -> Int -- ^ Height of screen
  -> Int -- ^ X of screen
  -> Int -- ^ Y of screen
  -> Float -- ^ Field of view in radians
  -> Ray -- ^ Ray from point of screen 
screenRayPersp c w h x y fov = Ray{..}
  where 
  aspect = fromIntegral w / fromIntegral h
  x' = (fromIntegral (2*x) / fromIntegral w - 1) * aspect
  y' = negate $ fromIntegral (2*y) / fromIntegral h - 1
  offsetx = cameraUp c `scale` y'
  offsety = cameraLeft c `scale` x' 
  fovx = cameraLeft c `scale` sin (x' * fov)
  fovy = cameraUp c `scale` sin (y' * fov)
  rayOrigin = cameraEye c + offsetx + offsety
  rayDirection = normalize $ cameraForward c + fovx + fovy