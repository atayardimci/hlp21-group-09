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

Interface function: 
Returns true if pos is within the bounds of the bounding box of the given symbol; else returns false.
```
let isSymClicked (pos : XYPos) (sym : Symbol) : bool =
    
```
Updated new Symbol model to include PortNumber and PortType 27/02/2021
```
//Add this to Helpers.fs so that both Sheet & Symbol can use it.
type PortInfo = {
    Pos : XYPos
    PortNumber : int option
    PortType : CommonTypes.PortType
}


type Symbol =
    {
        Id : CommonTypes.ComponentId
        Type : CommonTypes.ComponentType 
        Label : string
        PortInfoList : PortInfo list //New
        Pos: XYPos
        LastDragPos : XYPos
        IsDragging : bool

        H : float // New Height ~~~~ Unsure if this is required
        W : float // New Width
        BBox : BoundingBox
    }
```
