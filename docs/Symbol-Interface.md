# Interface documentation for Symbol

Add a bounding box record in helper.function. 
```
type BoundingBox = {
    TopLeft : XYPos
    BottomRight : XYPos
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

