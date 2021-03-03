# Interface documentation for Sheet
```
type Model = {
    Wire: BusWire.Model
    Canvas: CanvasProps
    SymIdList : CommonTypes.ComponentId list

    Ports : Symbol.Port list
    PortsToBeRendered : (float* Symbol.Port list) list
    IsPortDragging : bool
    IdOfPortBeingDragged : string

    IsDrawingRegion : bool

    IsWireSelected : bool
    SelectedWire : CommonTypes.ConnectionId option

    RegionToBeRendered  : Helpers.BoundingBox
    StartDrawingPosition : XYPos option
    }

type PortDU =
    | In
    | Out
    | All

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL | AltQ
    | AltW | AltA | AltS | AltD
    | AltShiftW | AltShiftA | AltShiftS | AltShiftD

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | Zoom of CanvasProps
    | MouseMsg of MouseT
    | Msg of string

    | StartDragging of sId : CommonTypes.ComponentId list * pagePos: XYPos
    | NewDragging of sId : CommonTypes.ComponentId list * pagePos: XYPos 
    | EndDragging of sId : CommonTypes.ComponentId list

    | StartPortDragging of Symbol.Port
    | DraggingPort of pagePos : XYPos
    | EndPortDragging of XYPos
    | DrawFromPortToCursor of XYPos*XYPos

    | RenderPorts of sId : (float * Symbol.Port list) list
    | UpdatePorts
    | UpdatedPortsToBusWire of sId : Symbol.Port list

    | StartDrawingRegion of XYPos
    | DrawingRegion of pagePos : XYPos
    | EndDrawingRegion of pagePos : XYPos

    | SelectComponentsWithinRegion of Helpers.BoundingBox
    | DeselectAllSymbols
    | DeselectWire 
    | RemoveDrawnLine 


    | AddWire of Symbol.Port*Symbol.Port
    | HighlightWire of CommonTypes.ConnectionId * XYPos    
    | DraggingWire of CommonTypes.ConnectionId option *XYPos
```
## Interface Functions from Symbol
```
let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 

/// Returns the symbol with given Id
let getSymbolWithId (symModel: Model) (sId: CommonTypes.ComponentId) : Symbol =

// Returns all Ports of all symbols in the model
let getAllPorts (symModel: Model) : Port List =


// Returns the bounding box of the symbol with the given Id
let getBoundingBoxOf (symModel: Model) (sId: CommonTypes.ComponentId) : BoundingBox =

// Returns all ports of the symbol with the given Id
let getPortOf (symModel: Model) (sId: CommonTypes.ComponentId) : Port list =
```

## Interface Functions from BusWire
```
let wireToSelectOpt (wModel: Model) (pos: XYPos) : CommonTypes.ConnectionId option 
    \\ A position is provided by Sheet.
    \\ The function then searches for a Wire in wModel whose BoundingBox encloses the specified point

let selectBoundedWires (wModel: Model) (boundary: BoundingBox)
    \\ Allows for sheet to provide a BoundingBox and all Wires that are completely enclosed by the BoundingBox are selected

\\\Messages
type Msg =
    | Symbol of Symbol.Msg 
    | SetColor of CommonTypes.HighLightColor // to change the color of all Wires drawn 
    | MouseMsg of MouseT
    | Select of wId: CommonTypes.ComponentId // To select the Wire with the provided Id
    | DrawFromPortToCursor of XYPos*XYPos // 
    | RemoveDrawnLine
    | AddWire of CommonTypes.Port*CommonTypes.Port // To creates a Wire to connect the two specified Ports
    | HighlightWire of CommonTypes.ConnectionId*XYPos // To select the Wire with the specified Id at the specified position
    | DeleteWire of CommonTypes.ConnectionId // To delete all selected Wires
    | UpdatedPortsToBusWire of sId : CommonTypes.Port list // To make Wires move with the ports they are connected to
    | DeselectWire // To deselect all selected Wires
    | DraggingWire of CommonTypes.ConnectionId*XYPos // To allow individual Wire segments to be moved so reouting can be corrected

```


## Helper Functions :
```
let createPorts (portInfoLst : CommonTypes.PortInfo list) : CommonTypes.Port list

let isPortClicked (pos : XYPos) (port: CommonTypes.Port) : bool

let sortDistToSymbol(pos : XYPos) (symList : Symbol.Symbol list) : (float * CommonTypes.ComponentId) list

let tryFindPortByPortId (id : string) (ports : Symbol.Port list) : Symbol.Port option = 

let findPortsMatchingHostId (portList: Symbol.Port list) (portDU : Helpers.PortDU) 
                            (dist : float , hostId : CommonTypes.ComponentId) : (float * Symbol.Port list)=


let getPortsWithinMinRange (mousePos : XYPos ) (model : Model) (minDist : float) (portDU : PortDU)  = 

let renderPorts (portList: CommonTypes.Port list) = 

let private renderRegion (bBox : BoundingBox) =

let displaySvgWithZoom (model: Model) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) =

```
