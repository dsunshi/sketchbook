
# Centroid
The algorithm comes from: https://math.stackexchange.com/questions/1801867/finding-the-centre-of-an-abritary-set-of-points-in-two-dimensions

```haskell
import Graphics.OpenSCAD

type V2 = Vector2d

centroid :: [V2] -> V2
centroid xs = (x' / n, y' / n)
    where
    n = fromIntegral $ length xs
    (x', y') = foldl (#+) (0.0, 0.0) xs
 
```

# Main entry point of the program

```haskell
main :: IO ()
main = putStrLn "Hello, Haskell!"
```
