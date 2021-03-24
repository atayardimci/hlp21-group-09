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
        NumOfConnections : int
        BusWidth : int Option
        NumOfErrors : int
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

        Pos : XYPos
        LastDragPos : XYPos
        IsDragging : bool
        IsSelected : bool
        HasError : bool
        NumOfConnections : int
        
        H : float
        W : float
        BBox : BoundingBox
    }

type Model = Symbol list

//----------------------------Message Type-----------------------------------//

type Msg =
    | MouseMsg of MouseT
    | AddSymbol of sType:CommonTypes.ComponentType * pos:XYPos // used by demo code to add a circle
    | DeleteSymbols
    | DeselectAllSymbols
    | UpdateInputOrientation of inputOrientation: PortOrientation
    | UpdateOutputOrientation of outputOrientation: PortOrientation 
    | AddErrorToPorts of (Port * Port)
    | RemoveConnections of (Port * Port * bool) list
    | EnforceBusWidth of int * Port
    | SelectSymbolsWithinRegion of box: BoundingBox

    // | UpdateSymbolModelWithComponent of CommonTypes.Component 








//--------------------------some interface functions------------------------//

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

/// Returns a list of the selected symbols
let getSelectedSymbols (symModel : Model) : Symbol list = 
    ([], symModel)
    ||> List.fold (fun acc sym -> if sym.IsSelected then acc @ [sym] else acc)

/// Returns true if two bounding boxes collide, else false
let boxesCollide (boxOne: BoundingBox) (boxTwo: BoundingBox) = 
    let oneTL, oneBR, twoTL, twoBR = boxOne.TopLeft, boxOne.BottomRight, boxTwo.TopLeft, boxTwo.BottomRight
    not (oneBR.X < twoTL.X || oneBR.Y < twoTL.Y || oneTL.X > twoBR.X || oneTL.Y > twoBR.Y)

let updateSymWithPort (newPort: Port) (sym: Symbol) : Symbol =
    let newInputPorts, newOutputPorts = 
        sym.InputPorts  |> List.map (fun port -> if port.Id = newPort.Id then newPort else port),
        sym.OutputPorts |> List.map (fun port -> if port.Id = newPort.Id then newPort else port)
    {sym with InputPorts = newInputPorts; OutputPorts = newOutputPorts}



/// Returns the overall BBox of a collection of symbols
let getOverallBBox (symList: Symbol list) : BoundingBox = 
    let selectedSymList = getSelectedSymbols symList
    if (selectedSymList <> [] ) then
        let minX = 
            selectedSymList
            |>List.minBy (fun sym -> sym.BBox.TopLeft.X)
            |>(fun sym -> sym.BBox.TopLeft.X)

        let maxX = 
            selectedSymList
            |>List.minBy (fun sym -> (-1.0)*(sym.BBox.BottomRight.X))
            |>(fun sym -> sym.BBox.BottomRight.X)

        let minY = 
            selectedSymList
            |>List.minBy (fun sym -> sym.BBox.TopLeft.Y)
            |>(fun sym -> (sym.BBox.TopLeft.Y + 15.0))

        let maxY = 
            selectedSymList
            |>List.minBy (fun sym -> (-1.0)*(sym.BBox.BottomRight.Y))
            |>(fun sym -> sym.BBox.BottomRight.Y)
        createBBoxFromPos {X = minX; Y = minY} {X = maxX; Y = maxY}
    else 
        nullBBox


let getPortWithId sym portId = 
    match (sym.InputPorts @ sym.OutputPorts) |> List.tryFind (fun port -> port.Id = portId) with
    | Some port -> port
    | None -> failwithf "Port with given Id in the given symbol was not found"
    
/// Increments or decrements the number of connections of the given port and symbol according to the given ChangeDU and returns the updated symbol
let changeNumOfConnections (change: ChangeDU) (portToChangeID: string) (symToChange: Symbol) : Symbol = 
    let operand = if change = Increment then 1 else -1
    let portToChange = getPortWithId symToChange portToChangeID
    let newPort = {portToChange with NumOfConnections = portToChange.NumOfConnections + operand}
    {updateSymWithPort newPort symToChange with NumOfConnections = symToChange.NumOfConnections + operand}

/// Returns true if any of the ports of a symbol has an error, else false
let checkSymbolForError (sym:Symbol) : bool = 
    (false, sym.InputPorts @ sym.OutputPorts)
    ||> List.fold (fun hasError port -> if port.NumOfErrors <> 0 then true else hasError)




/// Auto Completed Widths of 5 special components
let autoCompleteWidths (sym: Symbol) =  
    let inputs, outputs = sym.InputPorts, sym.OutputPorts

    match sym.Type with
    | CommonTypes.SplitWire w ->
        match inputs.[0].BusWidth, outputs.[1].BusWidth with
        | Some inW, None -> 
            let outW = if inW > w then (inW - w) else failwithf $"Input width of SplitWire should be larger than {w}"
            updateSymWithPort {outputs.[1] with BusWidth = Some outW} sym
        | None, Some outW -> 
            updateSymWithPort {inputs.[0] with BusWidth = Some (outW + w)} sym
        | _, _ when inputs.[0].NumOfConnections = 0 && outputs.[1].NumOfConnections = 0 ->
            sym
            |> updateSymWithPort {inputs.[0] with BusWidth = None} 
            |> updateSymWithPort {outputs.[1] with BusWidth = None} 
        | _ -> sym

    | CommonTypes.MergeWires ->
        match inputs.[0].BusWidth, inputs.[1].BusWidth, outputs.[0].BusWidth with
        | Some inW0, Some inW1, None ->
            updateSymWithPort {outputs.[0] with BusWidth = Some (inW0 + inW1)} sym
        | Some inW, None, Some outW ->
            let otherInW = if outW > inW then (outW - inW) else failwithf $"Input widths of MergeWires should be less than {outW}"
            updateSymWithPort {inputs.[1] with BusWidth = Some otherInW} sym
        | None, Some inW, Some outW ->
            let otherInW = if outW > inW then (outW - inW) else failwithf $"Input widths of MergeWires should be less than {outW}"
            updateSymWithPort {inputs.[0] with BusWidth = Some otherInW} sym
        | _ when sym.NumOfConnections < 2 ->
            sym
            |> updateSymWithPort {inputs.[0] with BusWidth = if inputs.[0].NumOfConnections = 1 && inputs.[0].NumOfErrors = 0 then inputs.[0].BusWidth else None} 
            |> updateSymWithPort {inputs.[1] with BusWidth = if inputs.[1].NumOfConnections = 1 && inputs.[1].NumOfErrors = 0 then inputs.[1].BusWidth else None} 
            |> updateSymWithPort {outputs.[0] with BusWidth = if outputs.[0].NumOfConnections = 1 && outputs.[0].NumOfErrors = 0 then outputs.[0].BusWidth else None} 
        | _ -> sym

    | CommonTypes.IOLabel ->
        match inputs.[0].BusWidth, outputs.[0].BusWidth with
        | Some inW, None -> 
            updateSymWithPort {outputs.[0] with BusWidth = Some inW} sym
        | None, Some outW -> 
            updateSymWithPort {inputs.[0] with BusWidth = Some outW} sym
        | _ when sym.NumOfConnections = 0 ->
            sym
            |> updateSymWithPort {inputs.[0]  with BusWidth = None} 
            |> updateSymWithPort {outputs.[0] with BusWidth = None} 
        | _ -> sym

    | CommonTypes.Mux2 ->
        match inputs.[0].BusWidth, inputs.[1].BusWidth, outputs.[0].BusWidth with 
        | Some w, None, None | None, Some w, None | None, None, Some w ->
            sym
            |> updateSymWithPort {inputs.[0]  with BusWidth = Some w} 
            |> updateSymWithPort {inputs.[1]  with BusWidth = Some w} 
            |> updateSymWithPort {outputs.[0] with BusWidth = Some w} 
        | _ when sym.NumOfConnections - inputs.[2].NumOfConnections = 0 ->
            sym
            |> updateSymWithPort {inputs.[0]  with BusWidth = None} 
            |> updateSymWithPort {inputs.[1]  with BusWidth = None} 
            |> updateSymWithPort {outputs.[0] with BusWidth = None} 
        | _ -> sym

    | CommonTypes.Demux2 ->
        match inputs.[0].BusWidth, outputs.[0].BusWidth, outputs.[1].BusWidth with 
        | Some w, None, None | None, Some w, None | None, None, Some w ->
            sym
            |> updateSymWithPort {inputs.[0]  with BusWidth = Some w} 
            |> updateSymWithPort {outputs.[0] with BusWidth = Some w} 
            |> updateSymWithPort {outputs.[1] with BusWidth = Some w} 
        | _ when sym.NumOfConnections - inputs.[1].NumOfConnections = 0 ->
            sym
            |> updateSymWithPort {inputs.[0]  with BusWidth = None} 
            |> updateSymWithPort {outputs.[0] with BusWidth = None} 
            |> updateSymWithPort {outputs.[1] with BusWidth = None} 
        | _ -> sym

    | _ -> sym




/// Removes a connection which can have an error or not and returns the model with the updated symbols
let removeConnection (model:Model) (portOne:Port, portTwo:Port, connectionHasError:bool) : Model = 
    (model, [portOne; portTwo])
    ||> List.fold (fun symModel port -> 
        symModel
        |> List.map (fun sym ->  
            if sym.Id = port.HostId then
                if connectionHasError then
                    let newSym = 
                        updateSymWithPort {port with NumOfErrors = port.NumOfErrors - 1} sym
                        |> changeNumOfConnections Decrement port.Id
                    let newSymHasError = checkSymbolForError newSym
                    {newSym with HasError = newSymHasError}
                else
                    sym
                    |> changeNumOfConnections Decrement port.Id 
                |> autoCompleteWidths
            else
                sym
        )
    )


/// Selects all symbols which have their bounding box collide with the given box and returns the updated model
let selectSymbolsInRegion (symModel: Model) (box: BoundingBox) : Model =
    let doesCollide = boxesCollide box
    symModel
    |> List.map (fun sym -> if doesCollide sym.BBox then {sym with IsSelected = true} else sym)




             





// ///Auto Completed Widths of 5 special components
// let autoCompleteWidths (sym : Symbol)  = 
//         let newSym = 
//             match (sym.Type) with 
//             |CommonTypes.SplitWire num ->  
//                                     let completedSymbol =  
//                                         match (sym.InputPorts,sym.OutputPorts) with              //For Ata : These part needs some changing as discussed before 
//                                                                                                  //          especially when connecting SpitWire from the top right.
//                                         | [in1],[out1;out2] when sym.NumberOfConnections = 0 ->  //For Ata : when zero connections refresh the symbol to original state.
//                                                                 {sym with InputPorts = [{in1 with BusWidth = None}] ; 
//                                                                           OutputPorts = [{out1 with BusWidth = None}; {out2 with BusWidth = None}]}
//                                         | [in1],[out1;out2] -> let tmp = 
//                                                                 match (in1.BusWidth,out1.BusWidth,out2.BusWidth) with
//                                                                 | None, Some given, Some x ->   let newInPort = {in1 with BusWidth = Some (given + x) } 
//                                                                                                 {sym with InputPorts = [newInPort]}
//                                                                 | Some x , Some given, None ->  let newOutPort = {out2 with BusWidth = Some (x - given) }
//                                                                                                 {sym with OutputPorts = [out1;newOutPort]}
//                                                                 | _ -> sym
//                                                                tmp
//                                         | _ -> failwithf "Error : Something wrong with SplitWire"
//                                     completedSymbol         
//             |CommonTypes.MergeWires  ->  
//                                     let completedSymbol = 
//                                         match (sym.InputPorts,sym.OutputPorts) with 
//                                         | [in1;in2],[out1] when sym.NumberOfConnections = 0 ->  //For Ata : when zero connections refresh the symbol to original state.
//                                                                 {sym with InputPorts = [{in1 with BusWidth = None}; {in2 with BusWidth = None}] ; 
//                                                                           OutputPorts = [{out1 with BusWidth = None}]}
//                                         | [in1;in2],[out1] -> let tmp =
//                                                                 match (in1.BusWidth,in2.BusWidth,out1.BusWidth) with
//                                                                 | None, Some given, Some x ->   let newInPort = {in1 with BusWidth = Some (x- given)} 
//                                                                                                 {sym with InputPorts = [newInPort; in2]}
//                                                                 | Some given , None, Some x ->  let newIn2Port = {in2 with BusWidth = Some (x - given)}
//                                                                                                 {sym with InputPorts = [in1; newIn2Port]}
//                                                                 | Some x , Some given, None ->  let newOutPort = {out1 with BusWidth = Some (x + given) }
//                                                                                                 {sym with OutputPorts = [newOutPort]}
//                                                                 | _ -> sym
//                                                               tmp
//                                         | _ -> failwithf "Error : Something wrong with MergeWires"
//                                     completedSymbol

//             |CommonTypes.IOLabel  ->
//                                    let completedSymbol =
//                                      match (sym.InputPorts,sym.OutputPorts) with 
//                                      | [in1],[out1] when sym.NumberOfConnections = 0 -> //For Ata : when zero connections refresh the symbol to original state.
//                                                         {sym with InputPorts = [{in1 with BusWidth = None}] ; 
//                                                                   OutputPorts = [{out1 with BusWidth = None}]}
//                                      | [in1],[out1] -> let tmp = 
//                                                          match (in1.BusWidth,out1.BusWidth) with
//                                                          | None, Some x -> let newInPort = {in1 with BusWidth = Some (x)} 
//                                                                            {sym with InputPorts = [newInPort]} 
//                                                          | Some x, None -> let newOutPort = {out1 with BusWidth = Some (x)}
//                                                                            {sym with OutputPorts = [newOutPort]}
//                                                          | _ -> sym
//                                                        tmp
//                                      | _ -> failwithf "Error : Something wrong with MergeWires"
//                                    completedSymbol
//             |CommonTypes.Mux2     ->
//                                   let completedSymbol =
//                                     match (sym.InputPorts,sym.OutputPorts) with 
//                                     | [in1;in2;sel],[out1] when sym.NumberOfConnections = 0 -> //For Ata : when zero connections refresh the symbol to original state.
//                                                                 {sym with InputPorts = [{in1 with BusWidth = None};{in2 with BusWidth = None}; sel] ; 
//                                                                           OutputPorts = [{out1 with BusWidth = None}]}
//                                     | [in1;in2;sel],[out1] -> let tmp = 
//                                                                match (in1.BusWidth,in2.BusWidth,out1.BusWidth) with
//                                                                | None, None, Some x -> let newIn1Port = {in1 with BusWidth = Some (x)} 
//                                                                                        let newIn2Port = {in2 with BusWidth = Some (x)}
//                                                                                        {sym with InputPorts = [newIn1Port;newIn2Port;sel]} 

//                                                                | None, Some x, None -> let newIn1Port = {in1 with BusWidth = Some (x)} 
//                                                                                        let newOutPort = {out1 with BusWidth = Some (x)}
//                                                                                        {sym with InputPorts = [newIn1Port; in2;sel]; OutputPorts = [newOutPort]}  
//                                                                | Some x,None, None ->  let newIn2Port = {in2 with BusWidth = Some (x)} 
//                                                                                        let newOutPort = {out1 with BusWidth = Some (x)}
//                                                                                        {sym with InputPorts = [in1 ; newIn2Port; sel]; OutputPorts = [newOutPort]}  
//                                                                | _ -> sym
//                                                               tmp      
//                                     | _ -> failwithf "Error : Something wrong with Mux2"
//                                   completedSymbol
//             |CommonTypes.Demux2      ->
//                                      let completedSymbol =
//                                        match (sym.InputPorts,sym.OutputPorts) with 
//                                        | [in1;sel],[out1;out2] when sym.NumberOfConnections = 0 -> //For Ata : when zero connections refresh the symbol to original state.
//                                                                 {sym with InputPorts = [{in1 with BusWidth = None};{sel with BusWidth = None}] ; 
//                                                                           OutputPorts = [{out1 with BusWidth = None};{out2 with BusWidth = None}]}
//                                        | [in1;sel],[out1;out2] -> let tmp = 
//                                                                    match (in1.BusWidth,out1.BusWidth,out2.BusWidth) with
//                                                                    | Some x,None, None -> let newOut1Port = {out1 with BusWidth = Some (x)} 
//                                                                                           let newOut2Port = {out2 with BusWidth = Some (x)}
//                                                                                           {sym with  OutputPorts = [newOut1Port;newOut2Port]}  
//                                                                    | None, None, Some x -> let newIn1Port = {in1 with BusWidth = Some (x)} 
//                                                                                            let newOut1Port = {out1 with BusWidth = Some (x)}
//                                                                                            {sym with InputPorts = [newIn1Port;sel] ; OutputPorts = [newOut1Port; out2]} 
//                                                                    | None, Some x, None -> let newIn1Port = {in1 with BusWidth = Some (x)} 
//                                                                                            let newOut2Port = {out2 with BusWidth = Some (x)}
//                                                                                            {sym with InputPorts = [newIn1Port; sel]; OutputPorts = [out1; newOut2Port]}  
                                                                   
//                                                                    | _ -> sym
//                                                                   tmp      
//                                        | _ -> failwithf "Error : Something wrong with DeMux2"
//                                      completedSymbol

//             | _ -> sym

                           
//         newSym
                        

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
    | CommonTypes.ComponentType.Catalogue -> 0., 0.


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
    | CommonTypes.ComponentType.Catalogue -> ([], [])
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
    | CommonTypes.ComponentType.Catalogue -> None
    // | _ -> failwithf "Shouldn't happen"


/// Returns the input and output ports using the hostId of the symbol 
/// together with the input and output ports position lists
let createPorts (sType:CommonTypes.ComponentType) hostId inputPortsPosList outputPortsPosList = 
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
                NumOfConnections = 0
                BusWidth = getBusWidthOfPort sType CommonTypes.PortType.Input idx
                NumOfErrors = 0
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
                NumOfConnections = 0
                BusWidth = getBusWidthOfPort sType CommonTypes.PortType.Output idx
                NumOfErrors = 0
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
let createNewSymbol (sType:CommonTypes.ComponentType) (name:string) (pos:XYPos) =
    let h, w = (getHeightWidthOf sType)
    let hostId = CommonTypes.ComponentId (uuid())
    let inputPortsPosList, outputPortsPosList = getPortPositions sType pos
    let inputPorts, outputPorts = createPorts sType hostId inputPortsPosList outputPortsPosList
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
        IsSelected = false
        HasError = false
        NumOfConnections = 0
        
        H = h
        W = w
        BBox = calculateBoundingBox h w pos
    }

let duplicateSymbol (symList : Symbol list) : XYPos*Symbol list = 
    let overallBBox = getOverallBBox (symList)
    let maxY,minY = overallBBox.BottomRight.Y,overallBBox.TopLeft.Y
    let posDisplacement = {X = 0.0; Y = maxY - minY + 50.0}
    
    let dupList = 
        symList
        |> List.map (fun sym -> 
            createNewSymbol sym.Type sym.Label (posAdd sym.Pos posDisplacement) 
            )
            // {newSym with HasError = sym.HasError}
        |> List.map (fun sym -> {sym with IsSelected = true})
        
    posDisplacement, dupList
                           
let insertSymbol symType name  =
    createNewSymbol (symType) name {X = float (10*64+30); Y=float (1*64+30)} 

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
        createNewSymbol (CommonTypes.ComponentType.Input 2)                 "I1"     {X = float (3*64+30); Y=float (1*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.Output 4)                "O1"     {X = float (4*64+30); Y=float (1*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.IOLabel)                 "IO1"    {X = float (5*64+30); Y=float (1*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.Constant (4, 15))        "C1"     {X = float (6*64+30); Y=float (1*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.BusSelection (4, 2))     "B1"     {X = float (7*64+30); Y=float (1*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.BusCompare (4, 10))      "EQ1"    {X = float (8*64+30); Y=float (1*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.Not)                     "G1"     {X = float (3*64+30); Y=float (2*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.And)                     "G2"     {X = float (4*64+30); Y=float (2*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.Or)                      "G3"     {X = float (5*64+30); Y=float (2*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.Xor)                     "G4"     {X = float (6*64+30); Y=float (2*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.Nand)                    "G5"     {X = float (7*64+30); Y=float (2*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.Nor)                     "G6"     {X = float (8*64+30); Y=float (2*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.Xnor)                    "G7"     {X = float (9*64+30); Y=float (2*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.Decode4)                 "DECO4"  {X = float (3*64+30); Y=float (3*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.Mux2)                    "MUX2"   {X = float (5*64+30); Y=float (3*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.Demux2)                  "DEMUX2" {X = float (6*64+30); Y=float (3*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.NbitsAdder 4)            "A1"     {X = float (7*64+30); Y=float (3*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.NbitsXor   7)            "XOR1"   {X = float (9*64+30); Y=float (3*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.MergeWires)              "MERGE"  {X = float (3*64+30); Y=float (6*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.SplitWire 3)             "SPLIT"  {X = float (4*64+30); Y=float (6*64+30)} 
        // createNewSymbol (CommonTypes.ComponentType.DFF)                     "FF1"    {X = float (5*64+30); Y=float (6*64+30)} 
        // createNewSymbol (CommonTypes.ComponentType.DFFE)                    "FFE1"   {X = float (7*64+30); Y=float (6*64+30)} 
        // createNewSymbol (CommonTypes.ComponentType.Register 5)              "REG1"   {X = float (3*64+30); Y=float (8*64+30)} 
        // createNewSymbol (CommonTypes.ComponentType.RegisterE 3)             "REG2"   {X = float (5*64+30); Y=float (8*64+30)} 
        // createNewSymbol (CommonTypes.ComponentType.AsyncROM fakeMemo)       "AROM1"  {X = float (3*64+30); Y=float (10*64+30)}
        // createNewSymbol (CommonTypes.ComponentType.ROM fakeMemo)            "ROM1"   {X = float (5*64+30); Y=float (10*64+30)}
        // createNewSymbol (CommonTypes.ComponentType.RAM fakeMemo)            "RAM1"   {X = float (7*64+30); Y=float (10*64+30)}
        // createNewSymbol (CommonTypes.ComponentType.Custom fakeCustomParams) "CUST1"  {X = float (10*64+30); Y=float (10*64+30)} 
        createNewSymbol (CommonTypes.ComponentType.Catalogue)               "Catalogue"  {X = 0.; Y= 0.} 
    ]
    , Cmd.none







let update (msg : Msg) (model : Model): Model*Cmd<'a> =
    match msg with
    | AddSymbol (sType, pos) -> 
        let name = initialNameOfComponent model sType
        (createNewSymbol sType name pos) :: model, Cmd.none

    | DeleteSymbols -> 
        let selectedIds = getSelectedSymbolIds model
        model |> List.filter (fun sym -> not (List.contains sym.Id selectedIds) )
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

    | SelectSymbolsWithinRegion box ->
        selectSymbolsInRegion model box, Cmd.none

    | AddErrorToPorts (portOne, portTwo) -> 
        (model, [portOne; portTwo])
        ||> List.fold (fun symModel port ->
            symModel
            |> List.map (fun sym -> 
                if sym.Id = port.HostId then 
                    let newSym = updateSymWithPort {port with NumOfErrors = port.NumOfErrors + 1} sym
                    {newSym with HasError = true}
                else sym 
            )  
        )
        , Cmd.none

    | RemoveConnections connectionList ->
        (model, connectionList)
        ||> List.fold (fun symModel connection ->
            removeConnection symModel connection
        )
        , Cmd.none
    
    | EnforceBusWidth (busWidth, undefinedPort) ->  
        model
        |> List.map (fun sym -> 
            if sym.Id = undefinedPort.HostId then
                let newPort = {undefinedPort with BusWidth = Some busWidth}
                sym
                |> updateSymWithPort newPort 
                |> autoCompleteWidths
            else sym
        )
        , Cmd.none

    | MouseMsg _ -> model, Cmd.none // allow unused mouse messags

















































//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId: CommonTypes.ComponentId) : XYPos = 
    List.find (fun sym -> sym.Id = sId) symModel
    |> (fun sym -> sym.Pos)

/// Returns the symbol with given Id
let getSymbolWithId (symModel: Model) (sId: CommonTypes.ComponentId) : Symbol option =
    List.tryFind (fun sym -> sym.Id = sId) symModel 


// Returns all Ports of all symbols in the model
let getAllPorts (symModel: Model) : Port list =
    symModel
    |> List.collect (fun sym -> sym.InputPorts @ sym.OutputPorts)

// Returns the bounding box of the symbol with the given Id
let getBoundingBoxOf (symModel: Model) (sId: CommonTypes.ComponentId) : BoundingBox =
    let sym = 
        match (getSymbolWithId symModel sId) with
        | Some sym -> sym
        | None -> failwithf "The symbol with given Id not found"
    sym.BBox
    
// Returns all ports of the symbol with the given Id
let getPortsOf (symModel: Model) (sId: CommonTypes.ComponentId) : Port list =
    let sym = 
        match (getSymbolWithId symModel sId) with
        | Some sym -> sym
        | None -> failwithf "The symbol with given Id not found"
    sym.InputPorts @ sym.OutputPorts

    
let getOrientationOfPort (symModel: Model) (port:Port) : PortOrientation = 
    match getSymbolWithId symModel port.HostId with
    | Some sym -> if (sym.Type=CommonTypes.Mux2 && port.PortNumber=Some 2) then Bottom
                  else if (sym.Type=CommonTypes.Demux2 && port.PortNumber=Some 1) then Bottom
                  else if port.PortType = CommonTypes.PortType.Input then sym.InputOrientation else sym.OutputOrientation
    | _ -> failwithf "The hosting symbol of the given port is not found"


/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"


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
    let inputPorts, outputPorts = createPorts comp.Type hostId inputPortsPosList outputPortsPosList
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
        NumOfConnections = 0

        H = h
        W = w
        BBox = calculateBoundingBox h w pos
    }
















































































    
// Returns all ports of the symbol with the given Id
let getPortOf (symModel: Model) (sId: CommonTypes.ComponentId) : Port list =
    let sym = 
        match (getSymbolWithId symModel sId) with
        | Some sym -> sym
        | None -> failwithf "Symbol with given Id not found"
    sym.InputPorts @ sym.OutputPorts

// let getBusWidthOfPortWithId () ->>> we need an interface function 
    

///// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
//let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
//    failwithf "Not Implemented"


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
//let calculateOutputWidth 
//        (wId: CommonTypes.ConnectionId) 
//        (outputPortNumber: int) 
//        (inputPortWidths: int option list) : int option =
//    failwithf "Not implemented"


//----------------------interface to Issie-----------------------------//
    


