
import Graphics.OpenSCAD
import Tree
import Land

-- type V2 = Vector2d
-- 
-- centroid :: [V2] -> V2
-- centroid xs = (x' / n, y' / n)
--     where
--     n = fromIntegral $ length xs
--     (x', y') = foldl (#+) (0.0, 0.0) xs
--
importScale :: Double
importScale = 0.2;

main :: IO ()
main = draw $ union [
    land,
    translate (5, 5, 20) $
        scale (importScale, importScale, importScale) $
        importFile "./resources/grass-4-blades.stl",
    translate (15, 15, 10) $
        scale (importScale, importScale, importScale) $
        importFile "./resources/grass-4-blades-alt.stl",
    translate (5, 15, 10) $
        scale (importScale, importScale, importScale) $
        importFile "./resources/grass-2-blades.stl",
    translate (15, 5, 10) $
        scale (importScale, importScale, importScale) $
        importFile "./resources/grass-3-blades.stl"
    ]
