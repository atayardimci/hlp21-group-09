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

```
//Add this to Symbol.fs
type PortInfo = {
    Pos : XYPos
    PortNumber : int option
    PortType : CommonTypes.PortType
    HostId : CommonTypes.ComponentId 
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
        BBox : BoundingBox
    }
//Updated Dragging and Deletion Msgs to act on a list of symbols
type Msg =   
    | MouseMsg of MouseT 
    /// coords not adjusted for top-level zoom
    | StartDragging of sId : CommonTypes.ComponentId list * pagePos: XYPos list
    | Dragging of sId : CommonTypes.ComponentId list * pagePos: XYPos list 
    | EndDragging of sId : CommonTypes.ComponentId list
    | AddSymbol of sType:CommonTypes.ComponentType * pos:XYPos // used by demo code to add a circle
    | DeleteSymbol of sId:CommonTypes.ComponentId list    
    | UpdateSymbolModelWithComponent of CommonTypes.Component // Issie interface
```
