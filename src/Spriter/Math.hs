module Spriter.Math(
    Vec3(..)
  , cross
  , scale 
  , norm 
  , normalize
  , Plane(..)
  , Ray(..)
  , restartRay
  , rebaseRay
  , rescaleRay
  , Box(..)
  , infinity
  , rayBoxIntersect
  , lerp
  ) where 

import GHC.Generics 
import Control.DeepSeq

data Vec3 = Vec3 !Float !Float !Float 
  deriving (Show, Eq, Generic)

instance NFData Vec3 

cross :: Vec3 -> Vec3 -> Vec3 
cross (Vec3 u1 u2 u3) (Vec3 v1 v2 v3) = Vec3 x y z 
  where 
  x = u2*v3 - u3*v2
  y = u3*v1 - u1*v3
  z = u1*v2 - u2*v1

scale :: Vec3 -> Float -> Vec3 
scale (Vec3 x y z) a = Vec3 (x*a) (y*a) (z*a)

norm :: Vec3 -> Float 
norm (Vec3 x y z) = sqrt $ x*x + y*y + z*z

normalize :: Vec3 -> Vec3 
normalize v = v `scale` (1 / norm v)

instance Num Vec3 where 
  (Vec3 x1 y1 z1) + (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
  (Vec3 x1 y1 z1) - (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
  (Vec3 x1 y1 z1) * (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)
  abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
  signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
  fromInteger v = Vec3 v' v' v'
    where v' = fromIntegral v
  negate (Vec3 x y z) = Vec3 (negate x) (negate y) (negate z)

data Plane = Plane {
  planeNormal :: !Vec3 
, planeDistance :: !Float 
} deriving (Show, Generic)

instance NFData Plane 

data Ray = Ray {
  rayOrigin :: !Vec3
, rayDirection :: !Vec3
} deriving (Show, Generic)

instance NFData Ray 

data Box = Box {
  boxMin :: !Vec3
, boxMax :: !Vec3
}

infinity :: Float 
infinity = 1/0

-- | Intersect box and ray, return distance from ray to intersection
rayBoxIntersect :: Ray -> Box -> Maybe Float 
rayBoxIntersect Ray{..} Box{..} = if tmaxz >= tminz then Just tminz else Nothing
  where 
  Vec3 bminx bminy bminz = boxMin 
  Vec3 bmaxx bmaxy bmaxz = boxMax
  Vec3 rox roy roz = rayOrigin
  Vec3 rdx rdy rdz = rayDirection

  tx1 = (bminx - rox) / rdx 
  tx2 = (bmaxx - rox) / rdx 
  tminx = if rdx == 0 then (-infinity) else max (-infinity) $ min tx1 tx2 
  tmaxx = if rdx == 0 then infinity else min infinity $ max tx1 tx2 

  ty1 = (bminy - roy) / rdy 
  ty2 = (bmaxy - roy) / rdy 
  tminy = if rdy == 0 then tminx else max tminx $ min ty1 ty2 
  tmaxy = if rdy == 0 then tmaxx else min tmaxx $ max ty1 ty2 

  tz1 = (bminz - roz) / rdz 
  tz2 = (bmaxz - roz) / rdz 
  tminz = if rdz == 0 then tminy else max tminy $ min tz1 tz2 
  tmaxz = if rdz == 0 then tmaxy else min tmaxy $ max tz1 tz2 

lerp :: Float -- ^ Start y
 -> Float -- ^ End y
 -> Float -- ^ Current x
 -> Float -- ^ Current y
lerp y0 y1 x = y0 + (y1-y0)*x

-- | Moves origin along ray direction by given value
restartRay :: Float -> Ray -> Ray 
restartRay d r = r {
    rayOrigin = rayOrigin r + rayDirection r `scale` d 
  }

-- | Rebase ray origin relative to given point
rebaseRay :: Vec3 -> Ray -> Ray 
rebaseRay v r = r {
    rayOrigin = rayOrigin r - v
  }

-- | Changes scale of ray origin
rescaleRay :: Vec3 -> Ray -> Ray 
rescaleRay s r = r {
    rayOrigin = rayOrigin r * s
  }