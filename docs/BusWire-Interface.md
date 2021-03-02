# Interface documentation for BusWire


```
\\\To Sheet 
let wireToSelectOpt (wModel: Model) (pos: XYPos) : CommonTypes.ConnectionId option 

\\\Messages
type Msg =
    | Symbol of Symbol.Msg
    | SetColor of CommonTypes.HighLightColor
    | MouseMsg of MouseT
    | Select of wId: CommonTypes.ComponentId
    | DrawFromPortToCursor of XYPos*XYPos
    | RemoveDrawnLine
    | AddWire of CommonTypes.Port*CommonTypes.Port
    | HighlightWire of CommonTypes.ConnectionId*XYPos
    | DeleteWire of CommonTypes.ConnectionId
    | UpdatedPortsToBusWire of sId : CommonTypes.Port list
    | DeselectWire
    | DraggingWire of CommonTypes.ConnectionId*XYPos
```
