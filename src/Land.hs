
module Land(land) where

import Graphics.OpenSCAD
-- import Numeric.Noise.Perlin
import Data.List (sortOn)

type V3 = Vector3d

voxelSize :: Double
voxelSize = 10.0

nozzelRes :: Double
nozzelRes = 0.3

sqrt2 :: Double
sqrt2 = 1.414213562

wallThickness :: Double
wallThickness = 2.0 * nozzelRes

supportR :: Double
supportR = 4 * nozzelRes


zPos :: V3 -> Double
zPos (_, _, z) = z

peak :: V3 -> [V3] -> V3
peak (row, col, _z) xs = last $ sortOn zPos zNeighbors
    where
        zNeighbors = filter (\(x, y, _) -> (row == x) && (col == y)) xs

isTop :: V3 -> [V3] -> Bool
isTop v xs = zPos v == zPos (peak v xs)

isSupport :: V3 -> [V3] -> Bool
isSupport v xs = zPos v < zPos (peak v xs)

voxel :: V3 -> [V3] -> Model V3
voxel v xs = translate (v3Scale v voxelSize) $ union [support, faces]
    where
        support
                | isTop v xs     = topSupport
                | isSupport v xs = insideSupport
                | otherwise      = undefined
        faces = voxelFaces v (neighbors v xs)

insideSupport :: Model V3
insideSupport = translate (voxelSize/2.0, voxelSize/2.0, 0) $
        cylinder supportR voxelSize (fn 33)

topSupport :: Model V3
topSupport = union [
    translate (voxelSize/2.0, voxelSize/2.0, hOffset) $
        rotate (0, 0, 45) $
        obCylinder 0 h (sqrt2 * hollowSize * 0.5) (fn 4),
    translate (voxelSize/2.0, voxelSize/2.0, 0) $
        cylinder supportR poleHeight (fn 33)
    ]
    where
        hollowSize = voxelSize - 2 * wallThickness
            -- (0.0, 0.0,  1.0) -> (0.0, 0.0, voxelSize - wallThickness)
            -- (0.0, 0.0, -1.0) -> (0.0, 0.0, wallThickness)
            -- (0.0,  1.0, 0.0) -> (0.0, voxelSize - wallThickness, 0.0)
            -- (0.0, -1.0, 0.0) -> (0.0, wallThickness, 0.0)
            -- (1.0,  0.0, 0.0) -> (voxelSize - wallThickness, 0.0, 0.0)
            -- (-1.0, 0.0, 0.0) -> (wallThickness, 0.0, 0.0)
            -- (0.0, 0.0, 0.0)  -> (0.1, 0.2, 0.3)
            -- otherwise -> undefined
        h = hollowSize / 2.0 -- tan 45 / 2 = 0.5
        hOffset = voxelSize - (wallThickness + h)
        poleHeight = hOffset + 2 * wallThickness

voxelFaces :: V3 -> [V3] -> Model V3
voxelFaces v xs = difference defaultVoxel faces
    where
        faces = union $ pullIn v xs

pullIn :: V3 -> [V3] -> [Model V3]
pullIn _ [] = []
pullIn v ((x, y, z):rest) = translate pos (cube hollowSize) : pullIn v rest
    where
        delta = v3Sub v (x, y, z)
        hollowSize = voxelSize - 2 * wallThickness
        pos = case delta of
            (0.0, 0.0,  1.0) -> (wallThickness, wallThickness, wallThickness - voxelSize) -- above
            (0.0, 0.0, -1.0) -> (wallThickness, wallThickness, hollowSize - wallThickness) -- 
            (0.0,  1.0, 0.0) -> (wallThickness, wallThickness - hollowSize, wallThickness) -- 
            (0.0, -1.0, 0.0) -> (wallThickness, hollowSize - wallThickness, wallThickness) -- 
            (1.0,  0.0, 0.0) -> (wallThickness - hollowSize, wallThickness, wallThickness) -- 
            (-1.0, 0.0, 0.0) -> (hollowSize - wallThickness, wallThickness, wallThickness) -- 
            (0.0, 0.0, 0.0)  -> (0.1, 0.2, 0.3)
            otherwise -> undefined

defaultVoxel :: Model V3
defaultVoxel = difference
    (cube voxelSize)
    (translate (wallThickness, wallThickness, -wallThickness) $ box hollowSize hollowSize voxelSize)
    where
        hollowSize = voxelSize - 2 * wallThickness

neighbors :: V3 -> [V3] -> [V3]
neighbors v xs = neighbors' xs []
    where
        neighbors' [] acc = acc
        neighbors' ((x, y, z) : rest) acc =
            if isNeighbor v (x, y, z)
                then neighbors' rest ((x, y, z) : acc)
                else neighbors' rest acc

v3Add :: V3 -> V3 -> V3
v3Add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

v3Sub :: V3 -> V3 -> V3
v3Sub (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

v3Abs :: V3 -> V3
v3Abs (x, y, z) = (abs(x), abs(y), abs(z))

v3Scale :: V3 -> Double -> V3
v3Scale (x1, y1, z1) s = (x1 * s, y1 * s, z1 * s)

voxels :: [V3]
voxels = [
    (0.0, 0.0, 0.0),
    (0.0, 0.0, 1.0),
    (1.0, 0.0, 0.0),
    (1.0, 1.0, 0.0),
    (0.0, 1.0, 0.0),
    (0.0, 2.0, 0.0)
    ]

isNeighbor :: V3 -> V3 -> Bool
isNeighbor a b = (> 0) $ length $ filter (\x -> b == x) possible
    where
        possible = map (v3Add a) neighborOffsets

neighborOffsets :: [V3]
neighborOffsets = [
    (0.0, 0.0,  1.0),
    (0.0, 0.0, -1.0),
    (0.0,  1.0, 0.0),
    (0.0, -1.0, 0.0),
    (1.0,  0.0, 0.0),
    (-1.0, 0.0, 0.0)
    ]

land :: Model V3
land = union $ map (`voxel` voxels) voxels
