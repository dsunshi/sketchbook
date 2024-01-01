module Tree(evergreen) where

import Graphics.OpenSCAD
import System.Environment

zUp :: (Vector (a, b, c), Fractional a, Fractional b, Eq a, Eq b, Eq c) =>
     c -> Model (a, b, c) -> Model (a, b, c)
zUp z = translate (0.0, 0.0, z)

octogon :: Double -> Double -> Model3d
octogon r h = cylinder r h (fn 8)

obOctogon :: Double -> Double -> Double -> Model3d
obOctogon r1 h r2 = obCylinder r1 h r2 (fn 8)

asRadius :: Fractional a => a -> a
asRadius d = d / 2.0

main :: IO()
main = do
    args <- getArgs
    writeFile (head args) (render
        $ evergreen 32.0 35.0)

evergreen :: Double -> Double -> Model3d
evergreen w h = union [
        stump,
        cones
    ]
    where
        stump  = octogon stumpR stumpH
        stumpR = asRadius 0.15625 * w
        stumpH = 0.14229 * h
        cones  = zUp stumpH $ growCones (asRadius w) coneH
        coneH  = (h - stumpH) / 3.0

growCones :: Double -> Double -> Model3d
growCones w h = union [
    cone w (0.675 * w),
    zUp h
        $ cone (0.860 * w) (0.469 * w),
    zUp (2.0 * h)
        $ cone (0.645 * w) 0.0 -- TODO: 0.2?
    ]
    where
        cone r1 = obOctogon r1 h
