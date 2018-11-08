{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Object where
import qualified Data.Text.IO as T
import qualified Csg
import qualified Csg.STL
import Data.List
import Data.Semigroup
import Data.Vec3 as V3

type XY = (Double, Double)
data Rectangle = Rectangle {
    size :: XY, 
    position :: XY, 
    center :: XY, 
    rotation :: Double
} 

rectangles :: [Rectangle]
rectangles = [
    Rectangle (27.0, 66.0) (57.0, 48.0) (-30.0, 21.0) (-12.0),
    Rectangle (65.0, 34.0) (69.0, 65.0) (28.0, -24.0) (13.0),
    Rectangle (54.0, 31.0) (21.0, 87.0) (-23.0, -21.0) (-12),
    Rectangle (27.0, 66.0) (104, 13) (11.0, -34.0) (5), 
    Rectangle (39.0, 39.0) (111, 32.5) (10.0, -27.0) (24)
    ]

reifyRectangle :: Rectangle -> Csg.BspTree
reifyRectangle r = Csg.rotate a o $ Csg.translate t $ Csg.scale s Csg.unitCube
    where 
        s = ((fst $ size r) , (snd $ size r), 1.0) 
        t = ((fst $ position r) + (fst $ size r)/2, (snd $ position r) + (snd $ size r)/2, 0.0)
        a = (0.0, 0.0, 1.0)
        o = pi * (rotation r)/180

logo :: Csg.BspTree
logo = Csg.scale (scale, scale, 0.4) $ Csg.translate offset mispositioned
    where 
        body = foldl1 Csg.union $ map reifyRectangle rectangles
        oneEye = Csg.uniformScale 5 $ Csg.unitCylinder 12    
        eye1 = Csg.translate (79.0, 86.0, 0.0) $ oneEye
        eye2 = Csg.translate (98.0, 89.0, 0.0) $ oneEye
        eyes = eye1 `Csg.union` eye2
        mispositioned =  body `Csg.subtract` eyes
        ((lowx, lowy, lowzz), (highx, highy, highz)) = Csg.aabb mispositioned
        offset = ((-lowx - highx)/2.0, (-lowy - highy)/2.0, 0.0)
        scale = 2.5 / (max (highx-lowx) (highy - lowy))
dieFaces :: [[[Int]]]
dieFaces = [
   [[1, 0, 0], 
    [0, 0, 0], 
    [0, 0, 1]], 

   [[1, 0, 0], 
    [0, 1, 0], 
    [0, 0, 1]], 

   [[1, 0, 1], 
    [0, 0, 0], 
    [1, 0, 1]], 

   [[1, 0, 1], 
    [0, 1, 0], 
    [1, 0, 1]], 

   [[1, 0, 1], 
    [1, 0, 1], 
    [1, 0, 1]]
    ]

reifyFace :: Csg.BspTree -> [[Int]] -> Csg.BspTree
reifyFace shape f = foldl1 Csg.union shapes
    where 
         inds = concatMap (\(i, js) -> map (\j->(fromIntegral i, fromIntegral j)) js) $ zip [0..] $ map (elemIndices 1) f
         shapes = map (\(i, j)-> Csg.translate (fromIntegral i - 1.0, fromIntegral j - 1.0, 0.0) shape) inds

axisX = (1.0, 0.0, 0.0)
axisY = (0.0, 1.0, 0.0)
axisZ = (0.0, 0.0, 1.0)

rotations :: [Csg.BspTree -> Csg.BspTree]
rotations = [
    Csg.rotate axisX 0.0,
    Csg.rotate axisY (pi/2), 
    Csg.rotate axisX (pi/2), 
    Csg.rotate axisX (-pi/2), 
    Csg.rotate axisY (-pi/2),
    Csg.rotate axisX pi
    ]

object :: Csg.BspTree
object = combinedFaces
    where 
        holeShape = Csg.uniformScale 0.50 $ Csg.unitCone 8
        facePatterns = logo : (map (reifyFace holeShape) dieFaces)
        translateFaceIntoPlace = Csg.translate (0.0, 0.0, -0.75) . Csg.uniformScale 0.35
        positionedPatterns = map (\(r, f) -> r f) $ zip rotations $ map translateFaceIntoPlace facePatterns
        sphere = Csg.unitSphere 24 12
        cube = Csg.uniformScale 1.5 Csg.unitCube
        diceShape = cube `Csg.intersection` sphere 
        combinedFaces = foldl Csg.subtract diceShape positionedPatterns

