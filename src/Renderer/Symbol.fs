module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers

//------------------------------------------------------------------------//
//-------------------------------Symbol Types-----------------------------//
//------------------------------------------------------------------------//
type Port = 
    {
        Id : string
        PortNumber : int option
        PortType : CommonTypes.PortType
        HostId : CommonTypes.ComponentId
        Pos : XYPos
        BBox : BoundingBox
        IsDragging : bool
        BusWidth : int Option
    }

type Symbol =
    {
        Id : CommonTypes.ComponentId
        Type : CommonTypes.ComponentType 
        Label : string

        InputPorts : Port list
        OutputPorts : Port list

        InputOrientation: PortOrientation
        OutputOrientation: PortOrientation

        Pos: XYPos
        LastDragPos : XYPos
        IsDragging : bool
        IsSelected : bool
        HasError : bool
        
        H : float
        W : float
        BBox : BoundingBox
    }

type Model = Symbol list

//----------------------------Message Type-----------------------------------//

type Msg =
    | MouseMsg of MouseT
    //| StartDragging of sIdList: CommonTypes.ComponentId list * pagePos: XYPos
    //| Dragging of sIdList : CommonTypes.ComponentId list * pagePos: XYPos
    //| EndDragging of sIdList : CommonTypes.ComponentId list
    | AddSymbol of sType:CommonTypes.ComponentType * pos:XYPos // used by demo code to add a circle
    | DeleteSymbols
    | DeselectAllSymbols
    | UpdateInputOrientation of inputOrientation: PortOrientation
    | UpdateOutputOrientation of outputOrientation: PortOrientation
    | ToggleError
    | SelectSymbolsWithinRegion of box: BoundingBox

    // | UpdateSymbolModelWithComponent of CommonTypes.Component 

//---------------------------------some interface functions----------------//

/// Returns true if pos is within the bounds of the bounding box of the given symbol; else returns false.
let isSymClicked (pos : XYPos) (sym : Symbol) : bool =
    match pos with 
    | p when (p.X >= sym.BBox.TopLeft.X) && (p.X <= sym.BBox.BottomRight.X) &&
             (p.Y >= sym.BBox.TopLeft.Y) && (p.Y <= sym.BBox.BottomRight.Y)  
        -> true
    | _ -> false


/// Returns the Ids of the selected symbols
let getSelectedSymbolIds (symModel: Model) : CommonTypes.ComponentId list = 
    ([], symModel)
    ||> List.fold (fun acc sym -> if sym.IsSelected then acc @ [sym.Id] else acc)

let getSelectedSymbols (symModel : Model) : Symbol list = 
    ([], symModel)
    ||> List.fold (fun acc sym -> if sym.IsSelected then acc @ [sym] else acc)

/// Returns true if two bounding boxes collide, else false
let boxesCollide (boxOne: BoundingBox) (boxTwo: BoundingBox) = 
    let oneTL, oneBR, twoTL, twoBR = boxOne.TopLeft, boxOne.BottomRight, boxTwo.TopLeft, boxTwo.BottomRight
    not (oneBR.X < twoTL.X || oneBR.Y < twoTL.Y || oneTL.X > twoBR.X || oneTL.Y > twoBR.Y)

/// Selects all symbols which have their bounding box collide with the given box and returns the updated model
let selectSymbolsInRegion (symModel: Model) (box: BoundingBox) : Model =
    let doesCollide = boxesCollide box
    symModel
    |> List.map (fun sym -> if doesCollide sym.BBox then {sym with IsSelected = true} else sym)

/// Render a ruler to assist in assigning the symbols
// fun (symbollist ) () go throughs the symbol list to find top left X = current topLeft = x 
//let renderRuler (symList : Symbol list) (BBox : BoundingBox) 
//    //

//---------------------------------helper types and functions----------------//

/// Returns the height and the width of a symbol according to its type in a tuple
let getHeightWidthOf (sType:CommonTypes.ComponentType) = 
    match sType with
    | CommonTypes.ComponentType.Input _ | CommonTypes.ComponentType.Output _ | CommonTypes.ComponentType.IOLabel -> 20., 30. 
    | CommonTypes.ComponentType.Constant _ -> 20., 39.6
    | CommonTypes.ComponentType.BusSelection _ | CommonTypes.ComponentType.BusCompare _ -> 24., 45.
    | CommonTypes.ComponentType.Not | CommonTypes.ComponentType.Nand | CommonTypes.ComponentType.Nor | CommonTypes.ComponentType.Xnor -> 35., 49.
    | CommonTypes.ComponentType.And | CommonTypes.ComponentType.Or | CommonTypes.ComponentType.Xor -> 35., 40.
    | CommonTypes.ComponentType.Decode4 -> 110., 90. 
    | CommonTypes.ComponentType.Mux2 | CommonTypes.ComponentType.Demux2 -> 50., 30. 
    | CommonTypes.ComponentType.NbitsAdder _ 
    | CommonTypes.ComponentType.NbitsXor _ -> 110., 90. 
    | CommonTypes.ComponentType.MergeWires | CommonTypes.ComponentType.SplitWire _ -> 30., 40. 
    | CommonTypes.ComponentType.DFF | CommonTypes.ComponentType.DFFE -> 70., 80.
    | CommonTypes.ComponentType.Register _ | CommonTypes.ComponentType.RegisterE _ -> 70., 120.
    | CommonTypes.ComponentType.AsyncROM _ | CommonTypes.ComponentType.ROM _ -> 100., 80.
    | CommonTypes.ComponentType.RAM _ -> 100., 130.
    | CommonTypes.ComponentType.Custom spec -> 
        let n = float (max spec.InputLabels.Length spec.OutputLabels.Length)
        n * 35., n * 35.
    // | _ -> failwithf "Shouldn't happen"


/// This function won't work! Implement later
let countOfComponentType (model : Model) (sType:CommonTypes.ComponentType) = 
    (0, model)
    ||> List.fold (fun count sym -> if sym.Type = sType then count + 1 else count)

/// Returns the initial name of a component given to it on its creation
let initialNameOfComponent (model : Model) (sType:CommonTypes.ComponentType) = 
    let count = countOfComponentType model sType
    match sType with
    | CommonTypes.ComponentType.Input _ -> sprintf $"I{count}"
    | CommonTypes.ComponentType.Output _ -> sprintf $"O{count}"
    | CommonTypes.ComponentType.IOLabel -> sprintf $"IO{count}"
    | CommonTypes.ComponentType.Constant _ -> sprintf $"C{count}"
    | CommonTypes.ComponentType.BusSelection _ -> sprintf $"B{count}"
    | CommonTypes.ComponentType.BusCompare _ -> sprintf $"EQ{count}"
    | CommonTypes.ComponentType.Not | CommonTypes.ComponentType.Nand | CommonTypes.ComponentType.Nor | CommonTypes.ComponentType.Xnor
    | CommonTypes.ComponentType.And | CommonTypes.ComponentType.Or | CommonTypes.ComponentType.Xor -> sprintf $"G{count}"
    | CommonTypes.ComponentType.Mux2 -> sprintf $"MUX{count}"
    | CommonTypes.ComponentType.Demux2 -> sprintf $"DM{count}"
    | CommonTypes.ComponentType.NbitsAdder _ -> sprintf $"A{count}"
    | CommonTypes.ComponentType.NbitsXor _ -> sprintf $"XOR{count}"
    | CommonTypes.ComponentType.DFF -> sprintf $"FF{count}"
    | CommonTypes.ComponentType.DFFE -> sprintf $"FFE{count}"
    | CommonTypes.ComponentType.Register _ -> sprintf $"REG{count}"
    | CommonTypes.ComponentType.RegisterE _ -> sprintf $"REG{count}"
    | CommonTypes.ComponentType.AsyncROM _ -> sprintf $"AROM{count}"
    | CommonTypes.ComponentType.ROM _ -> sprintf $"ROM{count}"
    | CommonTypes.ComponentType.RAM _ -> sprintf $"RAM{count}"
    | CommonTypes.ComponentType.Custom _ -> sprintf $"CUST{count}"
    | _ -> sprintf ""


/// Returns the port positions of a symbol according to its type and position
let getPortPositions sType pos = 
    let h, w = getHeightWidthOf sType
    match sType with
    | CommonTypes.ComponentType.Input _ 
    | CommonTypes.ComponentType.Constant _ -> getRegularPortPositions (Left, Right) 0 1 h w pos
    | CommonTypes.ComponentType.Output _ -> getRegularPortPositions (Left, Right) 1 0 h w pos
    | CommonTypes.ComponentType.IOLabel
    | CommonTypes.ComponentType.BusSelection _ 
    | CommonTypes.ComponentType.BusCompare _ -> getRegularPortPositions (Left, Right) 1 1 h w pos
    | CommonTypes.ComponentType.Not -> getRegularPortPositions (Left, Right) 1 1 h w pos
    | CommonTypes.ComponentType.Nand | CommonTypes.ComponentType.Nor | CommonTypes.ComponentType.Xnor -> getGatePortPositions Left true h w pos
    | CommonTypes.ComponentType.And | CommonTypes.ComponentType.Or   | CommonTypes.ComponentType.Xor -> getGatePortPositions Left false h w pos
    | CommonTypes.ComponentType.Decode4 -> getRegularPortPositions (Left, Right) 2 4 h w pos
    | CommonTypes.ComponentType.Mux2 -> getMux2PortPositions h w pos
    | CommonTypes.ComponentType.Demux2 -> getDemux2PortPositions h w pos
    | CommonTypes.ComponentType.NbitsAdder _ -> getRegularPortPositions (Left, Right) 3 2 h w pos
    | CommonTypes.ComponentType.NbitsXor _ -> getRegularPortPositions (Left, Right) 2 1 h w pos
    | CommonTypes.ComponentType.MergeWires _ -> getMergeWiresPortPositions h w pos
    | CommonTypes.ComponentType.SplitWire _ -> getSplitWirePortPositions h w pos
    | CommonTypes.ComponentType.DFF -> getRegularPortPositions (Left, Right) 1 1 h w pos
    | CommonTypes.ComponentType.DFFE -> getRegularPortPositions (Left, Right) 2 1 h w pos
    | CommonTypes.ComponentType.Register _ -> getRegularPortPositions (Left, Right) 1 1 h w pos
    | CommonTypes.ComponentType.RegisterE _ -> getRegularPortPositions (Left, Right) 2 1 h w pos
    | CommonTypes.ComponentType.AsyncROM _ -> getRegularPortPositions (Left, Right) 1 1 h w pos
    | CommonTypes.ComponentType.ROM _ -> getRegularPortPositions (Left, Right) 1 1 h w pos
    | CommonTypes.ComponentType.RAM _ -> getRegularPortPositions (Left, Right) 3 1 h w pos
    | CommonTypes.ComponentType.Custom spec -> getRegularPortPositions (Left, Right) spec.InputLabels.Length spec.OutputLabels.Length h w pos
    // | _ -> failwithf "Shouldn't happen"




let getBusWidthOfPort (sType:CommonTypes.ComponentType) (portType:CommonTypes.PortType) (portNumber:int) : int Option = 
    match sType with
    | CommonTypes.ComponentType.Input w -> Some w
    | CommonTypes.ComponentType.Output w -> Some w
    | CommonTypes.ComponentType.IOLabel -> None
    | CommonTypes.ComponentType.Constant (w, v) -> Some w
    | CommonTypes.ComponentType.BusSelection (w, lsb) -> if portType = CommonTypes.PortType.Input then None else Some w
    | CommonTypes.ComponentType.BusCompare (w, v) -> if portType = CommonTypes.PortType.Input then Some w else Some 1
    | CommonTypes.ComponentType.Not | CommonTypes.ComponentType.Nand 
    | CommonTypes.ComponentType.Nor | CommonTypes.ComponentType.Xnor 
    | CommonTypes.ComponentType.And | CommonTypes.ComponentType.Or   
    | CommonTypes.ComponentType.Xor -> Some 1
    | CommonTypes.ComponentType.Decode4 -> if portType = CommonTypes.PortType.Input && portNumber = 0 then Some 2 else Some 1
    | CommonTypes.ComponentType.Mux2 -> if portType = CommonTypes.PortType.Input && portNumber = 2 then Some 1 else None
    | CommonTypes.ComponentType.Demux2 -> if portType = CommonTypes.PortType.Input && portNumber = 1 then Some 1 else None
    | CommonTypes.ComponentType.NbitsAdder w -> 
        match portType with 
        | CommonTypes.PortType.Input when portNumber = 0 -> Some 1
        | CommonTypes.PortType.Output when portNumber = 1 -> Some 1
        | _ -> Some w
    | CommonTypes.ComponentType.NbitsXor w -> Some w
    | CommonTypes.ComponentType.MergeWires -> None
    | CommonTypes.ComponentType.SplitWire w -> if portType = CommonTypes.PortType.Output && portNumber = 0 then Some w else None
    | CommonTypes.ComponentType.DFF | CommonTypes.ComponentType.DFFE -> Some 1
    | CommonTypes.ComponentType.Register w -> Some w
    | CommonTypes.ComponentType.RegisterE w -> if portType = CommonTypes.PortType.Input && portNumber = 1 then Some 1 else Some w
    | CommonTypes.ComponentType.AsyncROM memo | CommonTypes.ComponentType.ROM memo 
        -> if portType = CommonTypes.PortType.Input then Some memo.AddressWidth else Some memo.WordWidth
    | CommonTypes.ComponentType.RAM memo -> 
        match portType with 
        | CommonTypes.PortType.Input when portNumber = 2 -> Some 1
        | CommonTypes.PortType.Input when portNumber = 0 -> Some memo.AddressWidth
        | _ -> Some memo.WordWidth
    | CommonTypes.ComponentType.Custom spec -> 
        match portType with 
        | CommonTypes.PortType.Input -> Some (snd spec.InputLabels.[portNumber])
        | _                          -> Some (snd spec.OutputLabels.[portNumber])
    // | _ -> failwithf "Shouldn't happen"





/// Returns the input and output ports using the hostId of the symbol 
/// together with the input and output ports position lists
let getPorts (sType:CommonTypes.ComponentType) hostId inputPortsPosList outputPortsPosList = 
    let inputPorts = 
        inputPortsPosList
        |> List.mapi (fun idx pos -> 
            {
                Id = uuid()
                PortNumber = Some idx
                PortType = CommonTypes.PortType.Input
                HostId = hostId
                Pos = pos
                BBox = calcBBoxWithRadius 6.0 pos
                IsDragging = false
                BusWidth = getBusWidthOfPort sType CommonTypes.PortType.Input idx
            }
        )
    let outputPorts = 
        outputPortsPosList
        |> List.mapi (fun idx pos -> 
            {
                Id = uuid()
                PortNumber = Some idx
                PortType = CommonTypes.PortType.Output
                HostId = hostId
                Pos = pos
                BBox = calcBBoxWithRadius 6.0 pos
                IsDragging = false
                BusWidth = getBusWidthOfPort sType CommonTypes.PortType.Output idx
            }
        )
    inputPorts, outputPorts

/// Returns a symbol with updated port positions according to 
/// input symbol's port orientations
let createSymbolWithPortOrientation (sym:Symbol) = 
    let newInputPortsPosList, newOutputPortsPosList = 
        match sym.Type with
            | CommonTypes.ComponentType.Nand | CommonTypes.ComponentType.Nor | CommonTypes.ComponentType.Xnor ->
                getGatePortPositions sym.InputOrientation true sym.H sym.W sym.Pos
            | CommonTypes.ComponentType.And | CommonTypes.ComponentType.Or | CommonTypes.ComponentType.Xor -> 
                getGatePortPositions sym.InputOrientation false sym.H sym.W sym.Pos
            | CommonTypes.ComponentType.Not ->
                getRegularPortPositions (sym.InputOrientation, sym.OutputOrientation) 1 1 sym.H (sym.W-9.) sym.Pos
            | CommonTypes.ComponentType.Decode4 | CommonTypes.ComponentType.NbitsAdder _ | CommonTypes.ComponentType.NbitsXor _ 
            | CommonTypes.ComponentType.DFF | CommonTypes.ComponentType.DFFE | CommonTypes.ComponentType.Register _ 
            | CommonTypes.ComponentType.RegisterE _ | CommonTypes.ComponentType.AsyncROM _ | CommonTypes.ComponentType.ROM _ 
            | CommonTypes.ComponentType.RAM _ | CommonTypes.ComponentType.Custom _ ->
                getRegularPortPositions (sym.InputOrientation, sym.OutputOrientation) sym.InputPorts.Length sym.OutputPorts.Length sym.H sym.W sym.Pos
            | _ -> failwithf "This symbol's port orientation cannot be changed"

    let newInputPorts = 
        (sym.InputPorts, newInputPortsPosList)
        ||> List.map2 (fun port newPos -> {port with Pos = newPos})
    let newOutputPorts = 
        match sym.Type with
        | CommonTypes.ComponentType.Not -> sym.OutputPorts
        | _ ->
            (sym.OutputPorts, newOutputPortsPosList)
            ||> List.map2 (fun port newPos -> {port with Pos = newPos})
    {sym with InputPorts = newInputPorts; OutputPorts = newOutputPorts}




//-----------------------Symbol Creation----------------------//

/// Returns a symbol of given type with given name at the given position
let createNewSymbol (sType:CommonTypes.ComponentType) (name:string) (pos:XYPos) (createDU: CreateDU) =
    let h, w = (getHeightWidthOf sType)
    let hostId = CommonTypes.ComponentId (uuid())
    let inputPortsPosList, outputPortsPosList = getPortPositions sType pos
    let inputPorts, outputPorts = getPorts sType hostId inputPortsPosList outputPortsPosList
    {
        Id = hostId // create a unique id for this symbol
        Type = sType
        Label = name

        InputPorts = inputPorts
        OutputPorts = outputPorts

        InputOrientation = Left
        OutputOrientation = Right

        Pos = pos
        LastDragPos = {X=0. ; Y=0.} 
        IsDragging = false
        IsSelected = if(createDU = Duplicate) then true else false
        HasError = false
        
        H = h
        W = w
        BBox = calculateBoundingBox h w pos
    }

let duplicateSymbol (symList : Symbol list) : XYPos*Symbol list = 
    let minY = 
        symList
        |>List.minBy (fun sym -> sym.BBox.TopLeft.Y)
        |>(fun sym -> sym.BBox.TopLeft.Y)

    let maxY = 
        symList
        |>List.minBy (fun sym -> (-1.0)*(sym.BBox.TopLeft.Y))
        |>(fun sym -> sym.BBox.BottomRight.Y)

    let posDisplacement = {X = 0.0; Y = maxY- minY}
    
    let dupList = 
        symList
        |>List.map (fun sym -> 
                            createNewSymbol sym.Type sym.Label (posAdd sym.Pos posDisplacement) Duplicate)
    (posDisplacement,dupList)
                           


let init () =
    let fakeMemo: CommonTypes.Memory = {
        AddressWidth = 4 
        WordWidth = 8
        Data = Map.empty
    }
    let fakeCustomParams :CommonTypes.CustomComponentType = {
        Name = "Custom"
        InputLabels = ["Sel", 2; "In", 8; "X", 1; "Z", 1]
        OutputLabels = ["Err", 4; "Out", 8; "W", 1]
    }
    [
        createNewSymbol (CommonTypes.ComponentType.Input 2)                 "I1"     {X = float (1*64+30); Y=float (1*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.Output 4)                "O1"     {X = float (2*64+30); Y=float (1*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.IOLabel)                 "IO1"    {X = float (3*64+30); Y=float (1*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.Constant (4, 15))        "C1"     {X = float (4*64+30); Y=float (1*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.BusSelection (4, 2))     "B1"     {X = float (5*64+30); Y=float (1*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.BusCompare (4, 10))      "EQ1"    {X = float (6*64+30); Y=float (1*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.Not)                     "G1"     {X = float (1*64+30); Y=float (2*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.And)                     "G2"     {X = float (2*64+30); Y=float (2*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.Or)                      "G3"     {X = float (3*64+30); Y=float (2*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.Xor)                     "G4"     {X = float (4*64+30); Y=float (2*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.Nand)                    "G5"     {X = float (5*64+30); Y=float (2*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.Nor)                     "G6"     {X = float (6*64+30); Y=float (2*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.Xnor)                    "G7"     {X = float (7*64+30); Y=float (2*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.Decode4)                 "DECO4"  {X = float (1*64+30); Y=float (3*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.Mux2)                    "MUX2"   {X = float (3*64+30); Y=float (3*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.Demux2)                  "DEMUX2" {X = float (4*64+30); Y=float (3*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.NbitsAdder 4)            "A1"     {X = float (5*64+30); Y=float (3*64+30)} Init 
        createNewSymbol (CommonTypes.ComponentType.NbitsXor   7)            "XOR1"   {X = float (7*64+30); Y=float (3*64+30)} Init 
        createNewSymbol (CommonTypes.ComponentType.MergeWires)              "MERGE"  {X = float (1*64+30); Y=float (6*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.SplitWire 3)             "SPLIT"  {X = float (2*64+30); Y=float (6*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.DFF)                     "FF1"    {X = float (3*64+30); Y=float (6*64+30)} Init 
        createNewSymbol (CommonTypes.ComponentType.DFFE)                    "FFE1"   {X = float (5*64+30); Y=float (6*64+30)} Init 
        createNewSymbol (CommonTypes.ComponentType.Register 5)              "REG1"   {X = float (1*64+30); Y=float (8*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.RegisterE 3)             "REG2"   {X = float (3*64+30); Y=float (8*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.AsyncROM fakeMemo)       "AROM1"  {X = float (1*64+30); Y=float (10*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.ROM fakeMemo)            "ROM1"   {X = float (3*64+30); Y=float (10*64+30)} Init 
        createNewSymbol (CommonTypes.ComponentType.RAM fakeMemo)            "RAM1"   {X = float (5*64+30); Y=float (10*64+30)} Init
        createNewSymbol (CommonTypes.ComponentType.Custom fakeCustomParams) "CUST1"  {X = float (8*64+30); Y=float (10*64+30)} Init
    ]
    , Cmd.none








//let startDraggingSymbol pagePos model sId  =
//    model
//    |> List.map (fun sym ->
//            if sId <> sym.Id then
//                sym
//            else
//                { sym with
//                    LastDragPos = pagePos
//                    IsDragging = true
//                }
//        )
//let draggingSymbol pagePos model sId  = 
//    model
//    |> List.map (fun sym ->
//        if sId <> sym.Id then
//            sym
//        else
//            let diff = posDiff pagePos sym.LastDragPos
                     
//            { sym with
//                Pos = posAdd sym.Pos diff
//                InputPorts = sym.InputPorts |> List.map (fun port -> {port with Pos = posAdd port.Pos diff ; BBox = calcBBoxWithRadius 5. (posAdd port.Pos diff)}) 
//                OutputPorts = sym.OutputPorts |> List.map (fun port -> {port with Pos = posAdd port.Pos diff ; BBox = calcBBoxWithRadius 5. (posAdd port.Pos diff)})
//                LastDragPos = pagePos
//                BBox = {
//                    TopLeft = (posAdd sym.BBox.TopLeft diff) 
//                    BottomRight = (posAdd sym.BBox.BottomRight diff)
//                }
//            }
//              // fun (symbollist ) () go throughs the symbol list to find top left X = current topLeft = x 

//    )
//let endDraggingSymbol model sId =
//    model
//    |> List.map (fun sym ->
//        if sId <> sym.Id then 
//            sym
//        else
//            { sym with
//                IsDragging = false 
//            }
//    )



let update (msg : Msg) (model : Model): Model*Cmd<'a> =
    match msg with
    | AddSymbol (sType, pos) -> 
        let name = initialNameOfComponent model sType
        (createNewSymbol sType name pos Init) :: model, Cmd.none

    | DeleteSymbols -> 
        let selectedIds = getSelectedSymbolIds model
        model |> List.filter (fun sym -> not (List.contains sym.Id selectedIds))
        , Cmd.none

    | DeselectAllSymbols ->
        model
        |> List.map (fun sym -> {sym with IsSelected = false})
        , Cmd.none
        
    | UpdateInputOrientation inputOrientation ->
        let selectedIds = getSelectedSymbolIds model
        model
        |> List.map (fun sym ->
            if List.contains sym.Id selectedIds then 
                if inputOrientation = sym.OutputOrientation then sym else createSymbolWithPortOrientation {sym with InputOrientation = inputOrientation} 
            else 
                sym
        )
        , Cmd.none

    | UpdateOutputOrientation outputOrientation ->
        let selectedIds = getSelectedSymbolIds model
        model
        |> List.map (fun sym ->
            match List.contains sym.Id selectedIds with //change here
            | true when sym.Type <> CommonTypes.ComponentType.Not && 
                sym.Type <> CommonTypes.ComponentType.And &&
                sym.Type <> CommonTypes.ComponentType.Or &&
                sym.Type <> CommonTypes.ComponentType.Xor &&
                sym.Type <> CommonTypes.ComponentType.Nand &&
                sym.Type <> CommonTypes.ComponentType.Nor &&
                sym.Type <> CommonTypes.ComponentType.Xnor &&
                outputOrientation <> sym.InputOrientation ->
                createSymbolWithPortOrientation {sym with OutputOrientation = outputOrientation}
            | _ -> sym
        )
        , Cmd.none

    | ToggleError ->
        let selectedIds = getSelectedSymbolIds model
        model
        |> List.map (fun sym ->
            if List.contains sym.Id selectedIds then 
                {sym with HasError = if sym.HasError then false else true} 
            else 
                sym
        )
        , Cmd.none

    | SelectSymbolsWithinRegion box ->
        selectSymbolsInRegion model box, Cmd.none



    | MouseMsg _ -> model, Cmd.none // allow unused mouse messags
    // | _ -> failwithf "Not implemented"










//----------------------------View Function for Symbols----------------------------//

type RenderSymbolProps =
    {
        Symbol : Symbol 
        Dispatch : Dispatch<Msg>
        key: string 
    }


//---------------------Helper functions for creating ReactElements----------------------//

let symbolShapeStyle (props : RenderSymbolProps) =
    let color =
        match props.Symbol.IsSelected, props.Symbol.IsDragging, props.Symbol.HasError with
        | true, _, _ 
        | _, true, _ -> dragColor
        | _, _, true -> errorColor
        | _ -> constColor
    Style [
        StrokeWidth 1
        Stroke "black"
        Fill color
    ]

let createInSymbolText (fontSize:float) (hAlignment:string) (rotation:int) (pos:XYPos) (textStr:string) = 
    text [ 
        X pos.X 
        Y pos.Y
        Style [
            TextAnchor hAlignment 
            DominantBaseline "middle" 
            FontSize fontSize
            FontFamily "monospace"
            Fill "Black"
            PointerEvents "none"
            UserSelect UserSelectOptions.None
            TransformOrigin (sprintf $"{pos.X}px {pos.Y}px")
            Transform (sprintf $"rotate({rotation}deg)")
        ]
    ][str textStr]


// will be changed
let getBusWidthInfo (sym:Symbol) =
    let w = 
        match sym.Type with
        | CommonTypes.ComponentType.Input w 
        | CommonTypes.ComponentType.Output w 
        | CommonTypes.ComponentType.Constant (w, _)
        | CommonTypes.ComponentType.NbitsAdder w 
        | CommonTypes.ComponentType.NbitsXor w 
        | CommonTypes.ComponentType.Register w 
        | CommonTypes.ComponentType.RegisterE w -> w
        | _ -> 0
    if w > 1 then
        sprintf $"({w-1}:{0})" 
    else
        ""

let createComponentTitle (props:RenderSymbolProps) = 
    let busWidthInfo = getBusWidthInfo props.Symbol
    text [ 
        X (props.Symbol.Pos.X + props.Symbol.W / 2.); 
        Y (props.Symbol.Pos.Y - VerticalAdjustment)
        Style [
            TextAnchor "middle" 
            DominantBaseline "text-top" 
            FontSize "12px"
            Fill "Black"
            PointerEvents "none"
            UserSelect UserSelectOptions.None
        ]
    ] [str (props.Symbol.Label + busWidthInfo)]
    


let createLogicGateShape props =
    let fX, fY = props.Symbol.Pos.X, props.Symbol.Pos.Y
    polygon [ 
        Points $"{fX},{fY} {fX+40.},{fY} {fX+40.},{fY+35.} {fX},{fY+35.}"
        symbolShapeStyle props
    ] [ ]

let getLogicGateNameStyle props = 
    text [ 
        X (props.Symbol.Pos.X + 20.) 
        Y (props.Symbol.Pos.Y + 18.)
        Style [
            TextAnchor "middle" // horizontal algnment vs (X,Y)
            DominantBaseline "text-top" // vertical alignment vs (X,Y)
            FontSize "14px"
            Fill "Black"
            PointerEvents "none"
            UserSelect UserSelectOptions.None
        ]
    ]

let createInvertElement props =
    let fX, fY = props.Symbol.Pos.X, props.Symbol.Pos.Y
    let w, h = props.Symbol.W, props.Symbol.H
    polygon [ 
        Points $"{fX+w-9.},{fY+h/2.} {fX+w-9.},{fY+11.} {fX+w},{fY+h/2.} "
        symbolShapeStyle props
        SVGAttr.StrokeWidth 0.7
    ] [ ]
        


let busWireStyle opacity = 
    Style [
        Stroke busColor
        StrokeWidth 3
        StrokeOpacity opacity
    ]
let wireStyle opacity = 
    Style [
        Stroke "Black"
        StrokeWidth 1.1
        StrokeOpacity opacity
    ]


let createLogicGate (strOnGate:string) (isInverted:bool) (props:RenderSymbolProps) =
    g []([
        createLogicGateShape props
        createComponentTitle props
        getLogicGateNameStyle props [str strOnGate]

        if isInverted then createInvertElement props
    ] )

let createClock fontSize fX fY heigth =
    g[] [
        polygon [ 
            Points $"{fX},{fY+heigth-12.} {fX+6.},{fY+heigth-7.} {fX},{fY+heigth-2.}"
            SVGAttr.Stroke "black"
            SVGAttr.StrokeWidth 1.
            SVGAttr.Fill "none"
        ] [ ]
        createInSymbolText fontSize "start" 0 {X=fX + 8.; Y=fY + heigth - 7.} "clk"
    ]

let createRectangularSymbol (symName:string) (inputPortNames:string list) (outputPortNames:string list) (includeClk:bool) props = 
    let fX, fY = props.Symbol.Pos.X, props.Symbol.Pos.Y
    let w, h = props.Symbol.W, props.Symbol.H

    let rotationOfTopAndBottomPorts = 
        match props.Symbol.Type with
        | CommonTypes.ComponentType.RAM _ -> -15
        | _ -> 0
    
    let portLabels =
        ((props.Symbol.InputPorts @ props.Symbol.OutputPorts), (inputPortNames @ outputPortNames))
        ||> List.map2 (fun port portName -> 
            match port.Pos.X, port.Pos.Y with
            | x, _ when x = fX -> createInSymbolText 10. "start" 0 {port.Pos with X = port.Pos.X + 8.} portName
            | x, _ when x = fX + w -> createInSymbolText 10. "end" 0 {port.Pos with X = port.Pos.X - 8.} portName
            | _, y when y = fY -> createInSymbolText 10. "middle" rotationOfTopAndBottomPorts {port.Pos with Y = port.Pos.Y + 10.} portName
            | _, y when y = fY + h -> createInSymbolText 10. "middle" rotationOfTopAndBottomPorts {port.Pos with Y = port.Pos.Y - 10.} portName
            | _ -> failwithf "Port position is not at the edge of the symbol!"
    
        )
    g   [ ] 
        ([
            polygon [ 
                Points $"{fX},{fY} {fX+w},{fY} {fX+w},{fY+h} {fX},{fY+h}"
                symbolShapeStyle props
            ] [ ]
            createComponentTitle props
            
            match props.Symbol.InputOrientation, props.Symbol.OutputOrientation with
            | Top, Right | Left, Top -> createInSymbolText 10. "middle" 0 {X=fX + w/2. ; Y=fY + h - 9.} symName
            | Top, Bottom | Bottom, Top -> createInSymbolText 10. "middle" 0 {X=fX + w/2. ; Y=fY + h/2.} symName
            | _ -> createInSymbolText 10. "middle" 0 {X=fX + w/2. ; Y=fY + 9.} symName
            
            let clkH = if props.Symbol.InputOrientation = Bottom then h/2. else h
            if includeClk then createClock 10. fX fY clkH
        ] @ portLabels
        )











// --------------------------render functions-------------------------------

let private renderInput =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let fX, fY = props.Symbol.Pos.X, props.Symbol.Pos.Y
            g   [ ] 
                [
                    polygon [ 
                        Points $"{fX},{fY} {fX+20.},{fY} {fX+30.},{fY+10.} {fX+20.},{fY+20.} {fX},{fY+20.}"
                        symbolShapeStyle props
                    ] [ ]
                    createComponentTitle props 
                ]
    , "Input"
    , equalsButFunctions
    )

let private renderOutput =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let fX, fY = props.Symbol.Pos.X, props.Symbol.Pos.Y
            g   [ ] 
                [
                    polygon [ 
                        Points $"{fX},{fY+10.} {fX+10.},{fY} {fX+30.},{fY} {fX+30.},{fY+20.} {fX+10.},{fY+20.}"
                        symbolShapeStyle props
                    ] [ ]
                    createComponentTitle props
                ]
    , "Output"
    , equalsButFunctions
    )

let private renderIOLabel =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let fX, fY = props.Symbol.Pos.X, props.Symbol.Pos.Y
            g   [ ] 
                [
                    polygon [ 
                        Points $"{fX},{fY+10.} {fX+10.},{fY} {fX+20.},{fY} {fX+30.},{fY+10.} {fX+20.},{fY+20.} {fX+10.},{fY+20.}"
                        symbolShapeStyle props
                    ] [ ]
                    createComponentTitle props
                ]
    , "IOLabel"
    , equalsButFunctions
    )

let private renderConstant =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let fX, fY = props.Symbol.Pos.X, props.Symbol.Pos.Y
            let valueInfo = 
                match props.Symbol.Type with
                | CommonTypes.ComponentType.Constant (w, v) ->
                    if w > 1 then sprintf "0x%0x"v else $"{v}"
                | _ -> failwithf "Shouldn't happen"
            
            g   [] 
                [
                    polygon [ 
                        Points $"{fX},{fY} {fX+10.},{fY+10.} {fX+39.6},{fY+10.} {fX+10.},{fY+10.} {fX},{fY+20.}"
                        symbolShapeStyle props
                    ] [ ]
                    text [ 
                        X (fX + 10.); 
                        Y (fY + 19.)
                        Style [
                            TextAnchor "left" 
                            DominantBaseline "text-top" 
                            FontSize "7.5px" // font size should adjust if the string is too long. To be implemented
                            Fill "Black"
                            PointerEvents "none"
                            UserSelect UserSelectOptions.None
                        ]
                    ] [str valueInfo]
                    createComponentTitle props
                ]
    , "Constant"
    , equalsButFunctions
    )

let private renderBusSelection =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let fX, fY = props.Symbol.Pos.X, props.Symbol.Pos.Y
            let busInfo = 
                match props.Symbol.Type with
                | CommonTypes.ComponentType.BusSelection (w, n) ->
                    if w > 1 then $"[{n+w-1}:{n}]" else $"{n}"
                | _ -> failwithf "Shouldn't happen"
            g   [ ] 
                [
                    polygon [ 
                        Points $"{fX},{fY+24.} {fX},{fY} {fX+25.},{fY} {fX+35.},{fY+6.} {fX+45.},{fY+6.} {fX+45.},{fY+18.} {fX+35.},{fY+18.} {fX+25.},{fY+24.}"
                        symbolShapeStyle props
                    ] [ ]
                    createComponentTitle props
                    createInSymbolText 10. "middle" 0 {X=fX + 17.5; Y=fY + 12.} busInfo
                ]
    , "BusSelection"
    , equalsButFunctions
    )

let private renderBusCompare =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let fX, fY = props.Symbol.Pos.X, props.Symbol.Pos.Y
            let valInfo = 
                match props.Symbol.Type with
                | CommonTypes.ComponentType.BusCompare (w, n) -> $"={n}"
                | _ -> failwithf "Failed in getting the decimal value to compare in renderBusCompare!"
            g   [ ] 
                [
                    polygon [ 
                        Points $"{fX},{fY+24.} {fX},{fY} {fX+25.},{fY} {fX+35.},{fY+6.} {fX+45.},{fY+6.} {fX+45.},{fY+18.} {fX+35.},{fY+18.} {fX+25.},{fY+24.}"
                        symbolShapeStyle props
                    ] [ ]
                    createComponentTitle props
                    createInSymbolText 10. "middle" 0 {X=fX + 17.5; Y=fY + 12.} valInfo
                ]
    , "BusCompare"
    , equalsButFunctions
    )

let private renderMux2 =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let fX, fY = props.Symbol.Pos.X, props.Symbol.Pos.Y
            g   [ ] 
                [
                    polygon [ 
                        Points $"{fX},{fY} {fX+30.},{fY+13.} {fX+30.},{fY+37.} {fX},{fY+50.}"
                        symbolShapeStyle props
                    ] [ ]
                    createComponentTitle props
                    createInSymbolText 10.5 "left" 0 {props.Symbol.InputPorts.[0].Pos with X = props.Symbol.InputPorts.[0].Pos.X + 4.} "0"
                    createInSymbolText 10.5 "left" 0 {props.Symbol.InputPorts.[1].Pos with X = props.Symbol.InputPorts.[1].Pos.X + 4.} "1"
                ]
    , "Mux2"
    , equalsButFunctions
    )
let private renderDemux2 =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let fX, fY = props.Symbol.Pos.X, props.Symbol.Pos.Y
            g   [ ] 
                [
                    polygon [ 
                        Points $"{fX},{fY+13.} {fX+30.},{fY} {fX+30.},{fY+50.} {fX},{fY+37.}"
                        symbolShapeStyle props
                    ] [ ]
                    createComponentTitle props
                    createInSymbolText 10.5 "end" 0 {props.Symbol.OutputPorts.[0].Pos with X = props.Symbol.OutputPorts.[0].Pos.X - 4.} "0"
                    createInSymbolText 10.5 "end" 0 {props.Symbol.OutputPorts.[1].Pos with X = props.Symbol.OutputPorts.[1].Pos.X - 4.} "1"
                ] 
    , "Demux2"
    , equalsButFunctions
    )

let private renderMergeWires =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let fX, fY = props.Symbol.Pos.X, props.Symbol.Pos.Y
            let h, w = props.Symbol.H, props.Symbol.W
            
            let topWidth, bottomWidth = 1, 1 // will be infered from ports

            let opacity = if props.Symbol.IsDragging then 0.4 else 1.
            let topStyle = if topWidth > 1 then busWireStyle opacity else wireStyle opacity
            let bottomStyle = if bottomWidth > 1 then busWireStyle opacity else wireStyle opacity

            g   [ ] 
                [
                    line [ X1 fX; Y1 fY; X2 (fX+w/2.); Y2 fY; topStyle] []
                    line [ X1 (fX+w/2.); Y1 fY; X2 (fX+w/2.); Y2 (fY+h/2.); topStyle] []

                    line [ X1 (fX+w/2.); Y1 (fY+h/2.); X2 (fX+w/2.); Y2 (fY+h); bottomStyle] []
                    line [ X1 (fX+w/2.); Y1 (fY+h); X2 (fX); Y2 (fY+h); bottomStyle] []

                    line [ X1 (fX+w/2.); Y1 (fY+h/2.); X2 (fX+w); Y2 (fY+h/2.); busWireStyle opacity] []

                    createComponentTitle props
                ]
    , "MergeWires"
    , equalsButFunctions
    )

let private renderSplitWire =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let fX, fY = props.Symbol.Pos.X, props.Symbol.Pos.Y
            let h, w = props.Symbol.H, props.Symbol.W
            
            let topWidth, bottomWidth = // will be infered from ports
                match props.Symbol.Type with 
                | CommonTypes.ComponentType.SplitWire w -> w, 1
                | _ -> failwithf "Shouldn't happen"

            let opacity = if props.Symbol.IsDragging then 0.4 else 1.
            let topStyle = if topWidth > 1 then busWireStyle opacity else wireStyle opacity
            let bottomStyle = if bottomWidth > 1 then busWireStyle opacity else wireStyle opacity

            g   [ ] 
                [
                    line [ X1 (fX); Y1 (fY+h/2.); X2 (fX+w/2.); Y2 (fY+h/2.); busWireStyle opacity] []

                    line [ X1 (fX+w/2.); Y1 fY; X2 (fX+w/2.); Y2 (fY+h/2.); topStyle] []
                    line [ X1 (fX+w/2.); Y1 fY; X2 (fX+w); Y2 fY; topStyle] []

                    line [ X1 (fX+w/2.); Y1 (fY+h/2.); X2 (fX+w/2.); Y2 (fY+h); bottomStyle] []
                    line [ X1 (fX+w/2.); Y1 (fY+h); X2 (fX+w); Y2 (fY+h); bottomStyle] []

                    createComponentTitle props
                ]
    , "SplitWire"
    , equalsButFunctions
    )


let private renderGate =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            match props.Symbol.Type with
            | CommonTypes.ComponentType.Not  ->   createLogicGate "1" true props
            | CommonTypes.ComponentType.And  ->   createLogicGate "&" false props
            | CommonTypes.ComponentType.Or   ->   createLogicGate "≥1" false props
            | CommonTypes.ComponentType.Xor  ->   createLogicGate "=1" false props
            | CommonTypes.ComponentType.Nand ->   createLogicGate "&" true props
            | CommonTypes.ComponentType.Nor  ->   createLogicGate "≥1" true props
            | CommonTypes.ComponentType.Xnor ->   createLogicGate "=1" true props
            | _ -> failwithf "Shouldn't happen"
    , "Gate"
    , equalsButFunctions
    )

let private renderRectSymbol =
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            match props.Symbol.Type with
            | CommonTypes.ComponentType.Decode4      ->   createRectangularSymbol "decode" ["Sel"; "Data"] ["0"; "1"; "2"; "3"] false props
            | CommonTypes.ComponentType.NbitsAdder _ ->   createRectangularSymbol ("adder" + getBusWidthInfo props.Symbol) ["Cin"; "A"; "B"] ["Sum"; "Cout"] false props
            | CommonTypes.ComponentType.NbitsXor _   ->   createRectangularSymbol ("XOR" + getBusWidthInfo props.Symbol) ["P"; "Q"] ["Out"] false props
            | CommonTypes.ComponentType.DFF          ->   createRectangularSymbol "DFF" ["D"] ["Q"] true props
            | CommonTypes.ComponentType.DFFE         ->   createRectangularSymbol "DFF" ["D"; "EN"] ["Q"] true props
            | CommonTypes.ComponentType.Register _   ->   createRectangularSymbol ("REG" + getBusWidthInfo props.Symbol) ["data-in"] ["data-out"] true props
            | CommonTypes.ComponentType.RegisterE _  ->   createRectangularSymbol ("REG" + getBusWidthInfo props.Symbol) ["data-in"; "EN"] ["data-out"] true props
            | CommonTypes.ComponentType.AsyncROM _   ->   createRectangularSymbol "Async-ROM" ["addr"] ["data"] false props
            | CommonTypes.ComponentType.ROM _        ->   createRectangularSymbol "ROM" ["addr"] ["data"] true props
            | CommonTypes.ComponentType.RAM _        ->   createRectangularSymbol "RAM" ["addr"; "data-in"; "write"] ["data-out"] true props
            | CommonTypes.ComponentType.Custom spec ->
                let name, inputLabels, outputLabels = spec.Name, spec.InputLabels |> List.map fst, spec.OutputLabels |> List.map fst
                createRectangularSymbol name inputLabels outputLabels true props
            | _ -> failwithf "Shouldn't happen"
    , "RectangularSymbol"
    , equalsButFunctions
    )






let private renderSymbol (props : RenderSymbolProps) = 
    match props.Symbol.Type with
    | CommonTypes.ComponentType.Input _ -> renderInput props
    | CommonTypes.ComponentType.Output _ -> renderOutput props
    | CommonTypes.ComponentType.IOLabel -> renderIOLabel props
    | CommonTypes.ComponentType.Constant _ -> renderConstant props
    | CommonTypes.ComponentType.BusSelection _ -> renderBusSelection props
    | CommonTypes.ComponentType.BusCompare _ -> renderBusCompare props
    | CommonTypes.ComponentType.Not -> renderGate props
    | CommonTypes.ComponentType.And -> renderGate props
    | CommonTypes.ComponentType.Or -> renderGate props
    | CommonTypes.ComponentType.Xor -> renderGate props
    | CommonTypes.ComponentType.Nand -> renderGate props
    | CommonTypes.ComponentType.Nor -> renderGate props
    | CommonTypes.ComponentType.Xnor -> renderGate props
    | CommonTypes.ComponentType.Decode4 -> renderRectSymbol props 
    | CommonTypes.ComponentType.Mux2 -> renderMux2 props
    | CommonTypes.ComponentType.Demux2 -> renderDemux2 props
    | CommonTypes.ComponentType.NbitsAdder _ -> renderRectSymbol props
    | CommonTypes.ComponentType.NbitsXor _ -> renderRectSymbol props
    | CommonTypes.ComponentType.MergeWires -> renderMergeWires props
    | CommonTypes.ComponentType.SplitWire _ -> renderSplitWire props
    | CommonTypes.ComponentType.DFF -> renderRectSymbol props
    | CommonTypes.ComponentType.DFFE -> renderRectSymbol props
    | CommonTypes.ComponentType.Register _ -> renderRectSymbol props
    | CommonTypes.ComponentType.RegisterE _ -> renderRectSymbol props
    | CommonTypes.ComponentType.AsyncROM _ -> renderRectSymbol props
    | CommonTypes.ComponentType.ROM _ -> renderRectSymbol props
    | CommonTypes.ComponentType.RAM _ -> renderRectSymbol props
    | CommonTypes.ComponentType.Custom _ -> renderRectSymbol props
    // | _ -> failwithf "Shouldn't happen"


/// View function for symbol layer of SVG
let view (model : Model) (dispatch : Msg -> unit) = 
    model
    |> List.map (fun ({Id = CommonTypes.ComponentId id} as symbol) ->
        renderSymbol 
            {
                Symbol = symbol
                Dispatch = dispatch
                key = id
            }
    )
    |> ofList






//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> sym.Pos)

/// Returns the symbol with given Id
let getSymbolWithId (symModel: Model) (sId: CommonTypes.ComponentId) : Symbol option =
    List.tryFind (fun sym -> sym.Id = sId) symModel 


// Returns all Ports of all symbols in the model
let getAllPorts (symModel: Model) : Port List =
    symModel
    |> List.collect (fun sym -> sym.InputPorts @ sym.OutputPorts)

// Returns the bounding box of the symbol with the given Id
let getBoundingBoxOf (symModel: Model) (sId: CommonTypes.ComponentId) : BoundingBox =
    let sym = 
        match (getSymbolWithId symModel sId) with
        | Some sym -> sym
        | None -> failwithf "Symbol with given Id not found"
    
    sym.BBox
    
// Returns all ports of the symbol with the given Id
let getPortOf (symModel: Model) (sId: CommonTypes.ComponentId) : Port list =
    let sym = 
        match (getSymbolWithId symModel sId) with
        | Some sym -> sym
        | None -> failwithf "Symbol with given Id not found"
    sym.InputPorts @ sym.OutputPorts

// let getBusWidthOfPortWithId () ->>> we need an interface function 
    

let getOrientationOfPort (symModel: Model) (port:Port) : PortOrientation = 
    match getSymbolWithId symModel port.HostId with
    | Some sym -> if port.PortType = CommonTypes.PortType.Input then sym.InputOrientation else sym.OutputOrientation
    | _ -> failwithf "The hosting symbol of the given port is not found"



/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"


// change this to get parametersOfSym
/// Returns the buswidth information of the symbol with the given id. 
/// If the buswidth information not known at symbol creation, None is returned.
/// For memory symbols, the first element is the address width, and the second element is the width of the data
let getBusWidthOf (symModel: Model) (sId: CommonTypes.ComponentId) : Option<int list> =
    let sym = 
        match (getSymbolWithId symModel sId) with
        | Some sym -> sym
        | None -> failwithf "Symbol with given Id not found"
    
    match sym.Type with
    | CommonTypes.ComponentType.Input w | CommonTypes.ComponentType.Output w 
    | CommonTypes.ComponentType.NbitsAdder w 
    | CommonTypes.ComponentType.Register w 
    | CommonTypes.ComponentType.RegisterE w -> Some [w]

    | CommonTypes.ComponentType.Constant (w, v) -> Some [w; v]
    | CommonTypes.ComponentType.BusSelection (wIn, wOut) -> Some [wIn; wOut]
    | CommonTypes.ComponentType.AsyncROM memo | CommonTypes.ComponentType.ROM memo | CommonTypes.ComponentType.RAM memo ->
        Some [memo.AddressWidth; memo.WordWidth] 
    | CommonTypes.ComponentType.Custom spec ->
        (spec.InputLabels @ spec.OutputLabels)
        |> List.map snd
        |> Some
    | _ -> None

/// Return the output Buswire width (in bits) if this can be calculated based on known
/// input wire widths, for the symbol wId. The types used here are possibly wrong, since
/// this calculation is based on ports, and the skeleton code does not implement ports or
/// port ids. If This is done the inputs could be expressed in terms of port Ids.
let calculateOutputWidth 
        (wId: CommonTypes.ConnectionId) 
        (outputPortNumber: int) 
        (inputPortWidths: int option list) : int option =
    failwithf "Not implemented"


//----------------------interface to Issie-----------------------------//
let extractComponent 
        (symModel: Model) 
        (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractComponents (symModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"

let createSymbolFromComponent (comp:CommonTypes.Component) (pos:XYPos) : Symbol =
    let h, w = getHeightWidthOf comp.Type
    let hostId = CommonTypes.ComponentId comp.Id
    let inputPortsPosList, outputPortsPosList = getPortPositions comp.Type pos
    let inputPorts, outputPorts = getPorts comp.Type hostId inputPortsPosList outputPortsPosList
    {
        Id = hostId 
        Type = comp.Type
        Label = comp.Label

        InputPorts = inputPorts
        OutputPorts = outputPorts

        InputOrientation = Left
        OutputOrientation = Right

        Pos = pos
        LastDragPos = {X=0. ; Y=0.} // initial value can always be this
        IsDragging = false // initial value can always be this
        IsSelected = false
        HasError = false

        H = h
        W = w
        BBox = calculateBoundingBox h w pos
    }


