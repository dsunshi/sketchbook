
import Graphics.Polydraw
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
main :: IO ()
main = draw $ union [ land,
    translate(55, 15, 40) $ evergreen 32.0 35.0,
    translate(85,  5, 40) $ evergreen 42.0 45.0,
    translate(65, 35, 30) $ evergreen 27.0 30.0,
    translate(75, 65, 20) $ evergreen 32.0 35.0,
    translate(15, 35, 20) $ evergreen 22.0 25.0
    ]
