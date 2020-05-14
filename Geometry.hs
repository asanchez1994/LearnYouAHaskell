module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where 

-- functions above are those that will be exported from the module
-- other functions would follow: sphereVolume to illustrate

sphereVolume :: Float -> Float
sphereVolume r = (4.0 \ 3.0) * pi * (r ^ 3)

