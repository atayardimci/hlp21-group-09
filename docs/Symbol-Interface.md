# Interface documentation for Symbol

Notes: 
- we need messages to select and deselect components
- we need a message for displaying the error highlight of the symbol
- we need an interface function for buswidth inference

Interface functions: 

```
// Returns true if pos is within the bounds of the bounding box of the given symbol; else returns false.
let isSymClicked (pos : XYPos) (sym : Symbol) : bool =

// Returns all PortInfo of all symbols in the model
let getAllPortInfo (symModel:Model) : CommonTypes.PortInfo List =

// Returns the bounding box of the symbol with the given Id
let getBoundingBoxOf (symModel: Model) (sId: CommonTypes.ComponentId) : BoundingBox =

// Returns the portInfo of all ports of the symbol with the given Id
let getPortInfoOf (symModel: Model) (sId: CommonTypes.ComponentId) : PortInfo list =

    
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
