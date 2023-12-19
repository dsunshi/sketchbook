
# Imports

```haskell
import Graphics.OpenSCAD
import Tree

-- type V2 = Vector2d
-- 
-- centroid :: [V2] -> V2
-- centroid xs = (x' / n, y' / n)
--     where
--     n = fromIntegral $ length xs
--     (x', y') = foldl (#+) (0.0, 0.0) xs
```
# Main

```haskell
main :: IO ()
main = draw $ evergreen 32.0 35.0
```
