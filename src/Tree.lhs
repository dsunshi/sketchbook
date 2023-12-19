# Exports
```haskell
module Tree(evergreen) where
```

# Imports

```haskell
import Graphics.OpenSCAD
```

# Evergreen

```haskell

evergreen w h = union [ stump, translate (0.0, 0.0, stumpH) cones ]
    where
        stump = cylinder stumpR stumpH (fn 8)
        cones = conus (w / 2.0) coneH
        stumpR = (0.15625 * w) / 2.0
        stumpH = 0.14229 * h
        coneH  = (h - stumpH) / 3.0

conus w h = union [
    obCylinder w h (0.675 * w) (fn 8),
    translate (0.0, 0.0, h) $ obCylinder (0.860 * w) h (0.469 * w) (fn 8),
    translate (0.0, 0.0, 2.0 * h) $ obCylinder (0.645 * w) h (0.0) (fn 8)
    ]
```
