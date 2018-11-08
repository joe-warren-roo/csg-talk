{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Object where
import qualified Data.Text.IO as T
import qualified Csg
import qualified Csg.STL
import Data.List
import Data.Semigroup
import Data.Vec3 as V3

center :: Csg.BspTree -> Csg.BspTree
center o = Csg.translate t o
    where 
        aabb = Csg.aabb o
        (x1, y1, z1) = fst aabb
        (x2, y2, z2) = snd aabb
        t = ((-x1-x2)/2, (-y1-y2)/2, (-z1-z2)/2 )

logo :: Csg.BspTree
logo =csnter $ Csg.uniformScale 0.3 $ bb `Csg.intersection` (back_chevron `Csg.union` lambda `Csg.union` equals_shifted)
    where 
        height = 6
        bar_height = height *2
        bar = Csg.rotate (0.0, 0.0, 1.0) (pi/6.0) $ Csg.translate (-0.50, bar_height/2.0, 0.0) $ Csg.scale (1.0, bar_height, 1.0) $ Csg.unitCube
        mirrored = Csg.scale (1.0, -1.0, -1.0) bar
        chevron = Csg.union bar mirrored
        back_chevron = Csg.translate (-2.0, 0.0, 0.0) chevron
        under = Csg.rotate (0.0, 0.0, 1.0) (pi+pi/6.0+0.0) $ Csg.translate (0.5001, bar_height/2.0, 0.0) $ Csg.scale (1.0, bar_height, 1.0) $ Csg.unitCube
        bb = Csg.scale (10, height, 2.0) Csg.unitCube
        equalsBar1 = Csg.translate (0.500, 1.25, 0.0) $ Csg.scale (3.0, 1.0, 0.999) $ Csg.unitCube
        equalsBar2 = Csg.translate (2/2 + 0.0001, -1.25, 0.0) $ Csg.scale (2, 1.0, 0.999) $ Csg.unitCube
        equals = equalsBar1 `Csg.union` equalsBar2
        equals_erased = equals `Csg.subtract` lambda
        equals_shifted = Csg.translate (1.0, 0.0, 0.0) equals_erased
        lambda = Csg.union under chevron


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


