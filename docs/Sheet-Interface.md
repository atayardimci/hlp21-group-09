# Interface documentation for Sheet
24/02/2021
- Added CID to identify selected symbols that will be dragged around or deleted and to prevent reevaluation when dragging across other symbols.
- CID will be updated to be a List of ComponentId in the future to handle moving multiple components altogether

```
  type Model = {
    Wire: BusWire.Model
    Canvas: CanvasProps
    CID : CommonTypes.ComponentId
    }
type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | Zoom of CanvasProps
    | MouseMsg of MouseT
    | StartDragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    | Dragging of sId : CommonTypes.ComponentId * pagePos: XYPos
    | EndDragging of sId : CommonTypes.ComponentId
    | Msg of string

Helper Functions : 
let createPorts (portInfoLst : CommonTypes.PortInfo list) : CommonTypes.Port list
let isPortClicked (pos : XYPos) (port: CommonTypes.Port) : bool
let sortDistToSymbol(pos : XYPos) (symList : Symbol.Symbol list) : (float * CommonTypes.ComponentId) list
let findPortsMatchingHostID (portList: CommonTypes.Port list) (hostId : CommonTypes.ComponentId) : CommonTypes.Port list = 
let renderPorts (portList: CommonTypes.Port list) = 


 ```
28/02/2021
- Added Ports and PortsToBeRendered to Model 

