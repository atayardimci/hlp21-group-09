# Interface documentation for Sheet
24/02/2021
- Added CID to identify selected symbols that will be dragged around or deleted and to prevent reevaluation when dragging across other symbols.
- Ported clicking and dragging of symbols to Sheet.
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
