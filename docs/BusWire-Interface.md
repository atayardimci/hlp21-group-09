# Interface documentation for BusWire

There are no Interfaces between BusWire and Symbol as Buswire receives all information on Ports from Sheet.

```
\\\To Sheet 
let wireToSelectOpt (wModel: Model) (pos: XYPos) : CommonTypes.ConnectionId option 
    \\ A position is provided by Sheet.
    \\ The function then searches for a Wire in wModel whose BoundingBox encloses the specified point

let selectBoundedWires (wModel: Model) (boundary: BoundingBox)
    \\ Allows for sheet to provide a BoundingBox and all Wires that are completely enclosed by the BoundingBox are selected
    
let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire
    \\ If a WireId is provided, this function will search for the Wire having this Id within the Wire Model.

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
