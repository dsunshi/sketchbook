module Tree(evergreen) where

import Graphics.Polydraw
import System.Environment

-- zUp :: (Vector (a, b, c), Fractional a, Fractional b, Eq a, Eq b, Eq c) =>
--      c -> Model (a, b, c) -> Model (a, b, c)
-- zUp z = translate (0.0, 0.0, z)

data ConePosition = Top | Bottom | Middle deriving (Enum, Eq)

pinInsert :: Double -> Model3d
pinInsert d = translate (-d/2, -d/2, 0) $ union [
        cube d,
        cap
    ]
    where
        r   = d * 0.70710678118 -- sqrt(2) / 2
        h   = r * 0.80988759527 -- tan(45) / 2
        cap = translate (d/2, d/2, d) $
              rotate (0, 0, 45) $
              obCylinder r h 0 (fn 4)

octogon :: Double -> Double -> Model3d
octogon r h = cylinder r h (fn 8)

obOctogon :: Double -> Double -> Double -> Model3d
obOctogon r1 h r2 = obCylinder r1 h r2 (fn 8)

asRadius :: Fractional a => a -> a
asRadius d = d / 2.0

main :: IO ()
main = do
    args <- getArgs
    writeFile (head args) (render
        $ evergreen 32.0 35.0)
    explode

explode :: IO ()
explode = mapM_ (\(fname, model) -> saveFile fname model) files
    where
        filenames = ["stump", "cone1", "cone2", "cone3"]
        files = zip filenames $ evergreenExploded 32.0 35.0
        saveFile :: String -> Model3d -> IO ()
        saveFile path model = writeFile (path ++ ".scad") $ render model


evergreen :: Double -> Double -> Model3d
evergreen w h = union [
        stump,
        cones
    ]
    where
        stump  = octogon stumpR stumpH
        stumpR = asRadius 0.15625 * w
        stumpH = 0.14229 * h
        cones  = up stumpH $ growCones (asRadius w) coneH
        coneH  = (h - stumpH) / 3.0

growCones :: Double -> Double -> Model3d
growCones w h = union [
    cone w (0.675 * w),
    up h
        $ cone (0.860 * w) (0.469 * w),
    up (2.0 * h)
        $ cone (0.645 * w) 0.0 -- TODO: 0.2?
    ]
    where
        cone r1 = obOctogon r1 h

evergreenExploded :: Double -> Double -> [Model3d]
evergreenExploded w h = stump : cones
    where
        stump  = union [
            octogon stumpR stumpH,
            translate (-stumpR/2, -stumpR/2, stumpH) $ cube (stumpR - 0.2)
            ]
        stumpR = asRadius 0.15625 * w
        stumpH = 0.14229 * h
        cones  = buildCones (asRadius w) coneH stumpR
        coneH  = (h - stumpH) / 3.0

buildCone :: Double -> Double -> Double -> Double -> ConePosition ->  Model3d 
buildCone w h r sr t
    | t == Bottom = union [
        difference (cone (1.000 * w) (0.675 * w)) (pinInsert sr),
        translate (-r/2, -r/2, h) $ cube (r - 0.2) -- TODO
        ]
    | t == Top    = difference (cone (0.645 * w) (0.000 * w)) (pinInsert r)
    | otherwise   = union [
        difference (cone (0.860 * w) (0.469 * w)) (pinInsert r),
        translate (-r/2, -r/2, h) $ cube (r - 0.2) -- TODO 
        ]
    where
        cone r1 = obOctogon r1 h

buildCones :: Double -> Double -> Double -> [Model3d]
buildCones w h r = [
        buildCone w h pinR r Bottom,
        buildCone w h pinR r Middle,
        buildCone w h pinR r Top
    ]
    where
        pinR = 4.2
