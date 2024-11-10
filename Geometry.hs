{-
At the beginning of a module, we specify the module name. If we have a file called Geometry.hs, we should name our module Geometry
Then, we specify the functions that it exports, and after that we can start writing the functions. 
-}

module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side 

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectangleArea a b * c 

cuboidArea :: Float -> Float -> Float -> Float 
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b

{-
Because cubes are a special case of cuboids, we define its area and volume by treating it as a cuboid whose sides are all of the same length
We also defined a helper function rectangleArea, which calculates a rectangle's area based on the lengths of its sides. 
Notice that we used it in some of our functions but we didn't export it. 

When making a module, we usually export only those functions that act as an interface to our module so that the implementation is hidden. 
If someone is using this Geometry module, they don't have to concern themselves with functions that we don't export. 
-}