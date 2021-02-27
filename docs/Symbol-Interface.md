# Interface documentation for Symbol

Interface function: 
Returns true if pos is within the bounds of the bounding box of the given symbol; else returns false.
```
let isSymClicked (pos : XYPos) (sym : Symbol) : bool =

    
```
Updated new Symbol model to include PortNumber PortType and HostId 27/02/2021
```
//Add this to CommonTypes.fs so that both Sheet & Symbol can use it.
type PortInfo = {
    Pos : XYPos
    PortNumber : int option
    PortType : CommonTypes.PortType
    HostId : CommonTypes.ComponentId 
}

// Symbol updated
type Symbol =
    {
        Id : CommonTypes.ComponentId
        Type : CommonTypes.ComponentType 
        Label : string
        PortInfoList : PortInfo list //New
        Pos: XYPos
        LastDragPos : XYPos
        IsDragging : bool
        BBox : BoundingBox
    }
```
