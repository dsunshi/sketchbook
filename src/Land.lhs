
# Exports
```haskell
module Land(land) where
```
# Imports

```haskell
import Graphics.OpenSCAD
import Numeric.Noise.Perlin
```

# Land

```haskell
land :: Model Vector3d
land = union $ map (\(x, y, z) -> translate (x, y, 0) $ box voxel voxel z) points
    where
        size = 20.0
        voxel = 10.0
        seed = 1
        octaves = 5
        scale = 0.05
        persistance = 0.5
        coords = [ 1.0 .. size ]
        perlinNoise = perlin seed octaves scale persistance
        -- xs = [noiseValue perlinNoise (x, y, 0) | y <- coords, x <- coords]
        -- zs = map (fromInteger . noiseToHeight) xs
        xs = [(x, y, fromIntegral $ noiseToHeight (noiseValue perlinNoise (x, y, 0))) | y <- coords, x <- coords]
        -- zOffset = minHeight xs
        zOffset = 50 -- TODO
        points = map (\(x, y, z) -> (x * voxel - voxel, y * voxel - voxel, z * voxel - zOffset)) xs

-- minHeight :: Ord a => 
minHeight xs = minimum (map zPos xs)

zPos :: (a, b, c) -> c
zPos (_, _, z) = z

noiseToHeight :: Double -> Integer
noiseToHeight n = round $ rescaleRange n (-1.0) 1.0 0.0 limit :: Integer
    where
        limit = 20.0
```

# Formula
https://stats.stackexchange.com/questions/281162/scale-a-number-between-a-range
rmin
denote the minimum of the range of your measurement
rmax
denote the maximum of the range of your measurement
tmin
denote the minimum of the range of your desired target scaling
tmax
denote the maximum of the range of your desired target scaling
m∈[rmin,rmax]

    denote your measurement to be scaled

Then

m↦m−rminrmax−rmin×(tmax−tmin)+tmin

will scale m
linearly into [tmin,tmax] as desired.

```haskell
rescaleRange :: Double -> Double -> Double -> Double -> Double -> Double
rescaleRange m rmin rmax tmin tmax = ((m - rmin) / (rmax - rmin)) * (tmax - tmin) + tmin
```
