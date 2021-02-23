# Interface documentation for Symbol

Add a bounding box record in helper.function. 
```
type BoundingBox = {minX : float; 
                    maxX : float; 
                    minY : float;
                    maxY : float;
                    }
```

Add a bBox (BoundingBox) field into 
```
type Symbol = {
        Pos: XYPos
        LastDragPos : XYPos
        IsDragging : bool
        Id : CommonTypes.ComponentId
        bBox : BoundingBox  
    }
```

