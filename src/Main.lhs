
```haskell
import Graphics.OpenSCAD

type V2 = Vector2d

centroid :: [V2] -> V2
centroid xs = (x' / n, y' / n)
    where
    n = fromIntegral $ length xs
    (x', y') = foldl (#+) (0.0, 0.0) xs
```

```haskell
main :: IO ()
main = putStrLn "Hello World"
```
