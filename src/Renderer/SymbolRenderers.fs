module SymbolRenderers
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Symbol
open Helpers


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
            match port.PortType, props.Symbol.InputOrientation, props.Symbol.OutputOrientation with
            | CommonTypes.PortType.Input , Top   , _ 
            | CommonTypes.PortType.Output, _     , Top    -> createInSymbolText 10. "middle" rotationOfTopAndBottomPorts {port.Pos with Y = port.Pos.Y + 10.} portName
            | CommonTypes.PortType.Input , Left  , _      -> createInSymbolText 10. "start" 0 {port.Pos with X = port.Pos.X + 8.} portName
            | CommonTypes.PortType.Input , Bottom, _ 
            | CommonTypes.PortType.Output, _     , Bottom -> createInSymbolText 10. "middle" rotationOfTopAndBottomPorts {port.Pos with Y = port.Pos.Y - 10.} portName
            | CommonTypes.PortType.Output, _     , Right  -> createInSymbolText 10. "end" 0 {port.Pos with X = port.Pos.X - 8.} portName
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
            let busW, valueInfo = 
                match props.Symbol.Type with
                | CommonTypes.ComponentType.Constant (w, v) ->
                    w, if w > 1 then sprintf "0x%0x"v else $"{v}"
                | _ -> failwithf "Shouldn't happen"
            
            let opacity = if props.Symbol.IsDragging then 0.4 else 1.
            let wireStyle = if busW > 1 then busWireStyle opacity else wireStyle opacity

            g   [] 
                [
                    polygon [ 
                        Points $"{fX},{fY} {fX+10.},{fY+10.} {fX},{fY+20.}"
                        symbolShapeStyle props
                    ] [ ]
                    line [ X1 (fX+10.); Y1 (fY+10.); X2 (fX+39.6); Y2 (fY+10.); wireStyle] []
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
            
            let topWidth, bottomWidth = // wil
                match props.Symbol.InputPorts.[0].BusWidth, props.Symbol.InputPorts.[1].BusWidth with
                | Some w1, Some w2 -> w1, w2
                | Some w1, None    -> w1, 1
                | None   , Some w2 -> 1 , w2
                | _ -> 1, 1

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
            
            let topWidth, bottomWidth = // wil
                match props.Symbol.OutputPorts.[0].BusWidth, props.Symbol.OutputPorts.[1].BusWidth with
                | Some w1, Some w2 -> w1, w2
                | Some w1, None    -> w1, 1
                | _ -> failwithf "Something wrong with SplitWire"

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

let standardText x y sz txt = 
    text [ // a demo text svg element
        X x; 
        Y y; 
        Style [
            TextAnchor "left" // left/right/middle: horizontal algnment vs (X,Y)
            DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
            FontSize (sprintf "%ipx"sz)
            FontWeight "Normal"
            Fill "Black" // demo font color
        ]
    ] [str <| txt]
let private drawRect x y w h clr= 
    rect
        [ 
            //OnMouseUp (mouseUpHelper pr hmm)
            //OnMouseDown (mouseDownHelper pr hmm)
            X x
            Y y
            SVGAttr.Width w
            SVGAttr.Height h
            SVGAttr.Fill clr
            SVGAttr.Stroke "black"
            SVGAttr.StrokeWidth 1
        ]
        [ ]

let private renderCatalogue = 
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            //let lx = props.Symbol.Pos.X
            //let ly = props.Symbol.Pos.Y
            
            g   [ Style [
                     UserSelect UserSelectOptions.None
                     PointerEvents "none"
                    ]
                ]
                [   
                    drawRect 0. 0. 200. "100%" "white"
                    standardText 10 20 20 "Catalogue"
                    drawRect 0. 50. 200. 30. "white"
                    standardText 15 60 15 "Input"
                    drawRect 0. 80. 200. 30. "white"
                    standardText 15 90 15 "Output"
                    drawRect 0. 110. 200. 30. "white"
                    standardText 15 120 15 "Constant"
                    drawRect 0. 140. 200. 30. "white"
                    standardText 15 150 15 "Label"
                    drawRect 0. 170. 200. 30. "white"
                    standardText 15 180 15 "Bus Select"
                    drawRect 0. 200. 200. 30. "white"
                    standardText 15 210 15 "Bus Compare"
                    drawRect 0. 230. 200. 30. "white"
                    standardText 15 240 15 "Merge Wires"
                    drawRect 0. 260. 200. 30. "white"
                    standardText 15 270 15 "Split Wires"

                    drawRect 0. 290. 50. 30. "white"
                    standardText 15 300 15 "Not"
                    drawRect 50. 290. 50. 30. "white"
                    standardText 65 300 15 "And"
                    drawRect 100. 290. 50. 30. "white"
                    standardText 115 300 15 "Or"
                    drawRect 150. 290. 50. 30. "white"
                    standardText 165 300 15 "Xor"
                    drawRect 0. 320. 67. 30. "white"
                    standardText 15 330 15 "Nand"
                    drawRect 67. 320. 67. 30. "white"
                    standardText 82 330 15 "Nor"
                    drawRect 134. 320. 66. 30. "white"
                    standardText 146 330 15 "Xnor"
                    drawRect 0. 800. 100. 30. "skyblue"
                    drawRect 0. 350. 200. 30. "white"
                    standardText 15 360 15 "Mux2"
                    drawRect 0. 380. 200. 30. "white"
                    standardText 15 390 15 "Demux2"
                    drawRect 0. 410. 200. 30. "white"
                    standardText 15 420 15 "Decode4"
                    drawRect 0. 440. 200. 30. "white"
                    standardText 15 450 15 "N bits adder"
                    drawRect 0. 470. 200. 30. "white"
                    standardText 15 480 15 "N bits Xor"
                    drawRect 0. 500. 200. 30. "white"
                    standardText 15 510 15 "D-flip-flop"
                    drawRect 0. 530. 200. 30. "white"
                    standardText 15 540 15 "D-flip-flop with enable"
                    drawRect 0. 560. 200. 30. "white"
                    standardText 15 570 15 "Register"
                    drawRect 0. 590. 200. 30. "white"
                    standardText 15 600 15 "Register with enable"
                    drawRect 0. 620. 200. 30. "white"
                    standardText 15 630 15 "ROM (Asynchronous)"
                    drawRect 0. 650. 200. 30. "white"
                    standardText 15 660 15 "ROM (Synchronous)"
                    drawRect 0. 680. 200. 30. "white"
                    standardText 15 690 15 "RAM"
                    standardText 15 810 15 "Sheet1"
                    drawRect 100. 800. 100. 30. "skyblue"
                    standardText 115 810 15 "Sheet2"
                    
                ]          
    , "Catalogue"
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
    | CommonTypes.ComponentType.Catalogue -> renderCatalogue props






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