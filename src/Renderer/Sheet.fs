﻿module Sheet
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React

open Helpers

type Model = {
    Wire: BusWire.Model
    Canvas: CanvasProps
    SymIdList : CommonTypes.ComponentId list

    Ports : Symbol.Port list
    HoveringPortsToBeRendered : (float* Symbol.Port list) list
    DraggingPortsToBeRendered : (float* Symbol.Port list) list
    OnePortToBeRendered : XYPos * Symbol.Port option
    IsPortDragging : bool
    IdOfPortBeingDragged : string

    IsDrawingRegion : bool

    IsWireSelected : bool
    SelectedWire : CommonTypes.ConnectionId option

    RegionToBeRendered  : Helpers.BoundingBox
    OverallBBoxToBeRendered : Helpers.BoundingBox
    AlignmentLinesToBeRendered : Line list 
    StartDrawingPosition : XYPos option
    }

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL |AltQ
    | AltW | AltA | AltS | AltD
    | AltShiftW | AltShiftA | AltShiftS | AltShiftD

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg
    | Zoom of CanvasProps
    | MouseMsg of MouseT
    | Msg of string

    //Symbols
    | StartDraggingSymbol of sId : CommonTypes.ComponentId list * pagePos: XYPos
    | DraggingSymbol of sId : CommonTypes.ComponentId list * pagePos: XYPos 
    | EndDraggingSymbol of sId : CommonTypes.ComponentId list
    | DeselectAllSymbols

    //Ports
    | StartDraggingPort of Symbol.Port
    | DraggingPort of pagePos : XYPos
    | EndDraggingPort of XYPos
    | DrawFromPortToCursor of XYPos*XYPos * Symbol.Port option
    | RemoveDrawnLine 
    | RenderPorts of sId : (float * Symbol.Port list) list * isHovering : bool
    | RenderOnePort of mousePos : XYPos* Symbol.Port option
    | UpdatePorts

    //Highlight Region
    | StartDrawingRegion of XYPos
    | DrawingRegion of pagePos : XYPos
    | EndDrawingRegion of pagePos : XYPos
    | SelectComponentsWithinRegion of Helpers.BoundingBox
    

    
    // Wire
    | StartDraggingWire of CommonTypes.ConnectionId*XYPos
    | DraggingWire of CommonTypes.ConnectionId option *XYPos
    | EndDraggingWire of CommonTypes.ConnectionId
    | DuplicateWire of displacementPos : XYPos
    | DeselectWire 
    | AddWire of Symbol.Port*Symbol.Port *CreateDU   

    | AlignBoxes of Helpers.BoundingBox 

let within num1 num2 = 
    if (num1 < (num2 + 2.5) ) && (num1 > (num2 - 2.5) ) then true else false

let withinPosX pos1 pos2 =
    if (pos1.X < (pos2.X + 2.5) ) && (pos1.X > (pos2.X - 2.5)) then true else false

let withinPosY pos1 pos2 =
    if (pos1.Y < (pos2.Y + 2.5) ) && (pos1.Y > (pos2.Y - 2.5)) then true else false

let symIsWithin (bBox : BoundingBox) (symList : Symbol.Symbol list) = 
            List.exists (fun (s : Symbol.Symbol) -> 
                let boxTL =  bBox.TopLeft
                let boxBR = bBox.BottomRight
                let sTL =  s.BBox.TopLeft
                let sBR = s.BBox.BottomRight
                // check if we should snap 2 grid
                withinPosX  (calcCentreBBox bBox) (calcCentre ({X = sTL.X ; Y = sTL.Y + 15.0 }) ({X = sBR.X ; Y = sBR.Y}))  ||
                withinPosY  (calcCentreBBox bBox) (calcCentre ({X = sTL.X ; Y = sTL.Y + 15.0 }) ({X = sBR.X ; Y = sBR.Y}))  ||
                (within boxTL.X sTL.X) ||
                (within boxTL.X sBR.X)||   
                (within boxBR.X sTL.X) ||
                (within boxBR.X sBR.X) ||
                (within boxTL.Y  (sTL.Y + 15.0)) ||
                (within boxTL.Y sBR.Y )||
                (within boxBR.Y (sTL.Y + 15.0)) ||
                (within boxBR.Y sBR.Y) 
                
             ) symList

let matchBBoxToPos (bBox : BoundingBox) = 
    match bBox with 
    |{TopLeft = tPos; BottomRight = bPos}  -> tPos,bPos  


let isPortClicked (pos : XYPos) (port: Symbol.Port) : bool = 
    match pos with
    | p when (p.X >= port.BBox.TopLeft.X) && (p.X <= port.BBox.BottomRight.X) &&
             (p.Y >= port.BBox.TopLeft.Y) && (p.Y <= port.BBox.BottomRight.Y)  
         -> true
    | _ -> false

let createLineCoordinates ( (x1,y1): float*float )  ( (x2,y2) : float*float ) : Line= 
    {P1 = {X = x1; Y = y1;}; P2 = {X = x2; Y= y2}}

let isSymAligned (overallBBox : BoundingBox) (symList : Symbol.Symbol list) : (XYPos * Line) list = //draggingBBox is already top Y coordinate is scaled

    let tOver, bOver = matchBBoxToPos overallBBox  //returns top and btm pos of overall box
    let notSelectedSym = List.filter (fun (sym : Symbol.Symbol)-> sym.IsSelected =false) symList
    if (symIsWithin overallBBox notSelectedSym ) then
        let (createLines : (XYPos * Line) list) = 
            ([] ,notSelectedSym)
            ||>List.fold (
                fun lst sym ->  
                    let symPosTuple = matchBBoxToPos sym.BBox
                    match symPosTuple with 

                    | {X = stx ; Y = sty}, {X = sbx ; Y = sby} 
                        when withinPosX  (calcCentreBBox overallBBox) (calcCentre ({X = stx ; Y = sty}) ({X = sbx ; Y = sby}))  
                                              -> let symCentre = calcCentre {X = stx ; Y = sty} {X = sbx ; Y = sby}
                                                 let overCentre = calcCentreBBox overallBBox
                                                 let xDiff = symCentre.X -  overCentre.X
                                                 if (symCentre.Y - overCentre.Y < 0.0 ) then 
                                                     let line =  createLineCoordinates (symCentre.X, sby) (symCentre.X, tOver.Y) 
                                                     (posOf xDiff 0.0, line) ::lst
                                                 else
                                                     let line =  createLineCoordinates (symCentre.X,bOver.Y) (symCentre.X, sty + 15.0) 
                                                     (posOf xDiff 0.0, line) ::lst
                    | {X = stx ; Y = sty}, {X = sbx ; Y = sby} 
                        when withinPosY  (calcCentreBBox overallBBox) (calcCentre ({X = stx ; Y = sty + 15.0}) ({X = sbx ; Y = sby}))  
                                              -> let symCentre = calcCentre {X = stx ; Y = sty + 15.0} {X = sbx ; Y = sby}
                                                 let overCentre = calcCentreBBox overallBBox
                                                 let yDiff = symCentre.Y -  overCentre.Y
                                                 if (symCentre.X - overCentre.X < 0.0 ) then 
                                                        let line =  createLineCoordinates (tOver.X,symCentre.Y) (sbx,symCentre.Y) 
                                                        (posOf 0.0 yDiff, line) ::lst
                                                    else
                                                        let line =  createLineCoordinates (stx,symCentre.Y) (bOver.X,symCentre.Y) 
                                                        (posOf 0.0 yDiff, line) ::lst
                                                               
                    | {X = stx ; Y = sty}, {X = sbx ; Y = sby} 
                        when within tOver.X stx -> let xDiff = (stx - tOver.X)
                                                   printf($"xDiff {xDiff}")
                                                   let line =  createLineCoordinates (stx,tOver.Y) (stx,sby) 
                                                   (posOf xDiff 0.0, line) ::lst                       
                    | {X = stx ; Y = sty}, {X = sbx ; Y = sby} 
                        when within tOver.X sbx -> let xDiff = (sbx-tOver.X)
                                                   let line =  createLineCoordinates (sbx,tOver.Y) (sbx,sby) 
                                                   (posOf xDiff 0.0, line) ::lst
                    
                    | {X = stx ; Y = sty}, {X = sbx ; Y = sby} 
                        when within bOver.X sbx -> let xDiff = (sbx-bOver.X)
                                                   let line =  createLineCoordinates (sbx,tOver.Y) (sbx,sby) 
                                                   (posOf xDiff 0.0, line) ::lst
                    | {X = stx ; Y = sty}, {X = sbx ; Y = sby} 
                        when within bOver.X stx -> let xDiff = (stx-bOver.X)
                                                   let line =  createLineCoordinates (stx,tOver.Y) (stx,sby) 
                                                   (posOf xDiff 0.0, line) ::lst
                    | {X = stx ; Y = sty}, {X = sbx ; Y = sby} 
                        when within (tOver.Y) (sty + 15.0) -> 
                                                 let yDiff = (sty + 15.0 - tOver.Y)
                                                 let line =  createLineCoordinates (tOver.X,sty + 15.0) (sbx,sty + 15.0) 
                                                 (posOf 0.0 yDiff, line) ::lst
                    | {X = stx ; Y = sty}, {X = sbx ; Y = sby} 
                        when within (tOver.Y) (sby) -> 
                                             let yDiff = (sby - tOver.Y)
                                             let line =  createLineCoordinates (tOver.X,sby) (sbx,sby) 
                                             (posOf 0.0 yDiff, line) ::lst
                    | {X = stx ; Y = sty}, {X = sbx ; Y = sby} 
                        when within (bOver.Y) (sty + 15.0) -> 
                                             let yDiff = (sty + 15.0 - bOver.Y)
                                             let line =  createLineCoordinates (tOver.X,sty + 15.0) (sbx,sty + 15.0) 
                                             (posOf 0.0 yDiff, line) ::lst

                    | {X = stx ; Y = sty}, {X = sbx ; Y = sby} 
                        when within (bOver.Y) (sby) -> 
                                             let yDiff = (sby- bOver.Y)
                                             let line =  createLineCoordinates (tOver.X,sby) (sbx,sby) 
                                             (posOf 0.0 yDiff, line) ::lst
                    
                    | _ -> (nullPos, createLineCoordinates (0.0,0.0) (0.0,0.0)) :: lst
                    )
        createLines
    else

        []


let shiftSymbol (sym : Symbol.Symbol) (diffPos : XYPos ) = 
    { sym with
        Pos = posAdd sym.Pos diffPos
        InputPorts = sym.InputPorts 
                     |> List.map (fun port -> 
                        {port with Pos = posAdd port.Pos diffPos ; 
                                   BBox = calcBBoxWithRadius 5. (posAdd port.Pos diffPos)}) 
        OutputPorts = sym.OutputPorts 
                      |> List.map (fun port -> 
                        {port with Pos = posAdd port.Pos diffPos ;
                                   BBox = calcBBoxWithRadius 5. (posAdd port.Pos diffPos)})
        LastDragPos = posAdd sym.LastDragPos diffPos
        BBox = {
            TopLeft = (posAdd sym.BBox.TopLeft diffPos) 
            BottomRight = (posAdd sym.BBox.BottomRight diffPos)
    }
        }

let shiftSelectedSymbols (symList : Symbol.Symbol list ) (diffPos : XYPos ) = 
    symList 
    |>List.map (fun sym -> 
            if sym.IsSelected = false then
                sym
            else
                shiftSymbol sym diffPos)

let isWithinDistanceToPort (pos : XYPos ) (dist : float) (port: Symbol.Port)   : bool = 
    if ( (calcDistance pos port.Pos) < dist ) then true else false

let getDistToOnePort (pos : XYPos) (port : Symbol.Port) : float = 
    let dist = calcDistance pos port.Pos
    dist

let sortDistToSymbol (pos : XYPos) (symList : Symbol.Symbol list) : (float * CommonTypes.ComponentId) list=
    let getDistToCentreofSymbol (pos : XYPos) (sym : Symbol.Symbol) : float * CommonTypes.ComponentId = 
        let dist = calcDistance pos (calcCentreBBox sym.BBox)
        dist,sym.Id
    symList
    |>List.map (getDistToCentreofSymbol pos)
    |>List.sortBy (fun (x,y) -> x)


let tryFindPortByPortId (id : string) (ports : Symbol.Port list) : Symbol.Port option= 
    let findPortByPortId (id : string) (port : Symbol.Port) : bool =   
       id = port.Id

    List.tryFind(findPortByPortId id) ports

let startDraggingSymbol (pagePos: XYPos)  (model : Symbol.Symbol list) sId  =
    model
    |> List.map (fun sym ->
            if sId <> sym.Id then
                sym
            else
                { sym with
                    LastDragPos = pagePos
                    IsDragging = true
                }
        )
let draggingSymbol (pagePos: XYPos)  (model : Symbol.Symbol list) sId  = 
    model
    |> List.map (fun sym -> 
        if sId <> sym.Id then
            sym
        else
            let diffPos = posDiff pagePos sym.LastDragPos      
            shiftSymbol sym diffPos
    )
let endDraggingSymbol (model : Symbol.Symbol list) sId =
    model
    |> List.map (fun sym ->
        if sId <> sym.Id then 
            sym
        else
            { sym with
                IsDragging = false 
                
            }
    )

///return a dist and either Input, Output or All Ports 
let filterPortsMatchingHostId (portList: Symbol.Port list) (portDU : Helpers.PortDU) 
                            (dist : float , hostId : CommonTypes.ComponentId)  
                                : (float * Symbol.Port list) = 
    let newPortList =
        portList
        |>List.filter(fun port ->     
            match portDU with 
            | In -> match port with 
                    |{HostId = hId; PortType = pType} 
                        when (hostId = hId && pType = CommonTypes.Input) -> true     
                    | _ -> false
            | Out -> match port with 
                     |{HostId = hId; PortType = pType} 
                        when (hostId = hId && pType = CommonTypes.Output) -> true 
                     | _ -> false
            | All -> match port with 
                     |{HostId = hId} 
                        when (hostId = hId) -> true 
                     | _ -> false
        )

    (dist,newPortList)

let getPortsWithinMinRange (mousePos : XYPos ) (model : Model) (minDist : float) (portDU : PortDU)  = 
    let sortedSymbols = sortDistToSymbol (mousePos) model.Wire.Symbol
    let symbolsWithinMinRange = 
        sortedSymbols
        |> List.filter (fun x -> (fst x) < minDist)  //only consider those within the minimum distance    
    let portsWithinMinRange = List.map (filterPortsMatchingHostId model.Ports portDU) symbolsWithinMinRange

    portsWithinMinRange


let private renderBBox (bBox : BoundingBox) = 
    let fX, fY = bBox.TopLeft.X, bBox.TopLeft.Y
    let fX_2, fY_2 = bBox.BottomRight.X, bBox.BottomRight.Y
    g   [ ] 
        ([
            polygon [ 
                Points $"{fX},{fY} {fX_2},{fY} {fX_2},{fY_2} {fX},{fY_2}"
                Style [
                StrokeWidth 1
                Stroke "blue"
                Fill "whitesmoke"
                FillOpacity "0.0"
                StrokeDasharray "4.0 4.0"
            ]
            ] [ ]
        ])

let private renderAlignmentLines (lines : Line list) = 
    g   [](
        List.map (fun l ->  
        g   []([
                line [ 
                    SVGAttr.X1 l.P1.X; SVGAttr.Y1 l.P1.Y; SVGAttr.X2 l.P2.X;  SVGAttr.Y2 l.P2.Y;
                    Style [
                    StrokeWidth 1
                    Stroke "purple"
                    StrokeDasharray "4.0 4.0"
                    ]
                ] [ ]
            ])
        ) lines
    )



let private renderPortsHovering (distance: float , portList: Symbol.Port list) = 
    let radius = 6.0
    let opacity = 1.0/distance * 55.0

    g   [] 
        (portList |> List.map (fun port -> 
        circle
            [ 
                Cx port.Pos.X
                Cy port.Pos.Y
                R radius
                SVGAttr.Fill portColor_const
                SVGAttr.Stroke "black"
                SVGAttr.StrokeWidth 1
                SVGAttr.FillOpacity opacity
            ]
            [ ]
        ))

let private renderPortsDragging (distance: float, portList: Symbol.Port list ) =
    let radius = 8.0
    let opacity = 1.0/ distance * 55.0
    g []
        (   (portList 
            |> List.map (fun port ->
                            circle
                                [ 
                                    Cx port.Pos.X
                                    Cy port.Pos.Y
                                    R radius
                                    SVGAttr.Fill portColor_const
                                    SVGAttr.Stroke "black"
                                    SVGAttr.StrokeWidth 1
                                    SVGAttr.FillOpacity opacity
                                ]
                                [ ]
                            )
            )
           
        )
let private renderOnePort (mouseOnPort : XYPos * Symbol.Port option)(reElem : ReactElement) = 
        let mouseOnPortCircle = 
            match mouseOnPort with
            | (mousePos, Some port) when (calcDistance mousePos port.Pos < 10.0)->
                                            circle [Cx port.Pos.X; Cy port.Pos.Y ;
                                                    R 12.0 ; SVGAttr.Fill "lightskyblue"
                                                    SVGAttr.Stroke "lightskyblue"; 
                                                    SVGAttr.StrokeWidth 0.8; 
                                                    SVGAttr.FillOpacity 0.3
                                                   ] []
                                                        
            | _ -> circle [] []

        g [] [mouseOnPortCircle; reElem]
               
        
        


let private renderHighlightRegion (bBox : BoundingBox) =
            let fX, fY = bBox.TopLeft.X, bBox.TopLeft.Y
            let fX_2, fY_2 = bBox.BottomRight.X, bBox.BottomRight.Y
            g   [ ] 
                ([
                    polygon [ 
                        Points $"{fX},{fY} {fX_2},{fY} {fX_2},{fY_2} {fX},{fY_2}"
                        Style [
                        StrokeWidth 1
                        Stroke "black"
                        Fill "whitesmoke"
                        FillOpacity "0.3"
                        StrokeDasharray "10.0 10.0"
                    ]
                    ] [ ]
                ])


let private renderGrid =  //Canvas Grid 
        svg [
            SVGAttr.Width "100%"
            SVGAttr.Height "100%"
        ]
            [
               defs[]
                [ 
                pattern [
                    Id "smallGrid"
                    SVGAttr.Width 24.0;
                    SVGAttr.Height 24.0;
                    SVGAttr.PatternUnits "userSpaceOnUse"
                    ] [path [SVGAttr.D "M 24 0 L 0 0 0 24" ; SVGAttr.Fill "none" ; 
                             SVGAttr.Stroke "silver"; SVGAttr.StrokeWidth 0.5] []]
                ]
               
               rect [
                   SVGAttr.Width "100%"
                   SVGAttr.Height "100%"
                   SVGAttr.Fill "url(#smallGrid)"
               ] []
            ]

let mutable getScrollPos : (unit ->(float*float) option) = (fun () -> None) 

let displaySvgWithZoom (model: Model) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) =

    let mDown (ev:Types.MouseEvent) = 
        if ev.buttons <> 0. then true else false

    let mouseOp op (ev:Types.MouseEvent) (scrollPos : (float*float)) = 
        dispatch <| MouseMsg {Op = op ; 
                              Pos = { X = (ev.clientX  + (fst scrollPos))/(model.Canvas.zoom) ;  
                                      Y = ( ev.clientY + (snd scrollPos))/ (model.Canvas.zoom)}}
        
    let wheelOp op (ev:Types.WheelEvent) =   
        let newZoom = model.Canvas.zoom - (model.Canvas.zoom*ev.deltaY*0.0007)
        dispatch <| Zoom  {model.Canvas with zoom = newZoom;}

    let renderHoveringPortsByDistance = g [] (List.map renderPortsHovering  model.HoveringPortsToBeRendered)
    let renderDraggingPortsByDistance = (g [](List.map renderPortsDragging  model.DraggingPortsToBeRendered))
                                        |> renderOnePort model.OnePortToBeRendered

    div [ Style 
            [ 
                Height "100vh" 
                MaxWidth "100vw"
                CSSProp.OverflowX OverflowOptions.Auto 
                CSSProp.OverflowY OverflowOptions.Auto

            ]
          Ref (fun html ->  
                getScrollPos <- fun () -> Some (html.scrollLeft, html.scrollTop))
                  
          let scrollTop,scrollLeft = 
              match getScrollPos() with
              | Some (scrollLeft,scrollRight) -> (scrollLeft, scrollRight)
              | None -> (0.0,0.0)

          OnMouseDown (fun ev -> (mouseOp Down ev (scrollTop,scrollLeft)))
          OnMouseUp (fun ev -> (mouseOp Up ev (scrollTop,scrollLeft)))
          OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev (scrollTop,scrollLeft) )
          OnWheel (fun ev -> if ev.ctrlKey = true then wheelOp CtrlScroll ev else ())
          
        ] 
        [ 
            svg
                [ Style 
                    [
                        Border "2px solid grey"
                        Height (model.Canvas.height) 
                        Width (model.Canvas.width )          
                    ]
                ]

                [ g 
                    [ Style [Transform  (sprintf "scale(%f)" model.Canvas.zoom)]] 
                    [   
                        renderGrid //background
                        renderAlignmentLines model.AlignmentLinesToBeRendered
                        svgReact   
                        renderDraggingPortsByDistance
                        renderHoveringPortsByDistance
                        renderHighlightRegion model.RegionToBeRendered
                        renderBBox model.OverallBBoxToBeRendered
                        
                    ] 
                 
                ]
             
        ] 


let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom model wireSvg dispatch
       

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Wire wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire
        {model with Wire = wModel}, Cmd.map Wire wCmd
    

    | KeyPress DEL ->  
        let SymModel, newCmd = 
            Symbol.update Symbol.Msg.DeleteSymbols model.Wire.Symbol  

        let newModel = 
            { model with
                 Wire = {model.Wire with Symbol = SymModel}
            }

        let newWireModel, wireCmd = 
            BusWire.update BusWire.Msg.DeleteWire newModel.Wire

        let newModel = 
            { model with 
                Wire = newWireModel
                SymIdList = []
                OverallBBoxToBeRendered = nullBBox
            }
        newModel, Cmd.batch [Cmd.ofMsg (UpdatePorts); Cmd.ofMsg (RenderPorts ([],true))]
    
    ///Duplication : Duplicate Symbols first then duplciate Wires.
    | KeyPress AltC  ->  
        let symbolsToBeDup = Symbol.getSelectedSymbols(model.Wire.Symbol)
        let dupSymbol = Symbol.duplicateSymbol (symbolsToBeDup)
        let newSymModel =
            model.Wire.Symbol
            |> List.map (fun sym -> {sym with IsSelected = false})
            |> List.append (snd dupSymbol)
        let displacement = fst dupSymbol
        let overallBBox =  Symbol.getOverallBBox newSymModel
        let newModel =  //Sym duplicated
            { model with 
                Wire = {model.Wire with Symbol = newSymModel}
                OverallBBoxToBeRendered = overallBBox
            }

        newModel,Cmd.batch[Cmd.ofMsg(UpdatePorts); Cmd.ofMsg(DuplicateWire displacement)] 

    | DuplicateWire displacementPos -> 
        let selectedWireList =
            BusWire.getSelectedWireList model.Wire.WX

        let dupWireList = BusWire.getWiresToBeDuplicated displacementPos selectedWireList model.Ports 

        let createDupWireList = 
            dupWireList
            |> List.map (fun tuple -> 
                match tuple  with
                | Some sourcePort , Some targetPort, w  ->  Cmd.ofMsg (AddWire (sourcePort,targetPort,Duplicate)) 
                | _ , _ , _-> failwithf "Shouldn't happen")
        model,Cmd.batch ((Cmd.ofMsg DeselectWire) :: createDupWireList )
   
    | KeyPress s -> // Updates Orientation Key Presses
        let newSymModel,newCmd =
            match s with 
            | AltA -> Symbol.update (Symbol.Msg.UpdateInputOrientation Left) model.Wire.Symbol
            | AltW -> Symbol.update (Symbol.Msg.UpdateInputOrientation Top) model.Wire.Symbol
            | AltS -> Symbol.update (Symbol.Msg.UpdateInputOrientation Bottom) model.Wire.Symbol
            | AltShiftW -> Symbol.update (Symbol.Msg.UpdateOutputOrientation Top) model.Wire.Symbol
            | AltShiftS -> Symbol.update (Symbol.Msg.UpdateOutputOrientation Bottom) model.Wire.Symbol
            | AltShiftD -> Symbol.update (Symbol.Msg.UpdateOutputOrientation Right) model.Wire.Symbol
        
        let newModel = {
            model with
                Wire = {model.Wire with Symbol = newSymModel}
                SymIdList = []
            }
        newModel, Cmd.ofMsg (UpdatePorts) 


       
    | RenderPorts (floatPortListTuple,isHovering) -> 
        match model.SymIdList with
        | [] when isHovering = true -> {model with HoveringPortsToBeRendered = floatPortListTuple}, Cmd.none
        | [] when isHovering = false -> {model with DraggingPortsToBeRendered = floatPortListTuple}, Cmd.none
        | _  -> {model with HoveringPortsToBeRendered = []; DraggingPortsToBeRendered = []}, Cmd.none

    | RenderOnePort (mousePos, portOption) ->
      
         {model with OnePortToBeRendered = (mousePos, portOption)},Cmd.none   

    | StartDraggingPort (startPort) -> 
        {model with
            IsPortDragging = true 
            IdOfPortBeingDragged = startPort.Id
        }, Cmd.none


    | DraggingPort (mousePos) -> 
        let draggedPort = tryFindPortByPortId model.IdOfPortBeingDragged model.Ports
        let port = 
            match draggedPort with
            |Some port -> port
            |None -> failwithf "Error in Port"

        let portsToBeRenderedTuple =
            match port.PortType with 
            | CommonTypes.Input -> getPortsWithinMinRange mousePos model 120.0 Out //if dragging input only render outputports
            | CommonTypes.Output -> getPortsWithinMinRange mousePos model 120.0 In //if dragging output only render inputports
        let portsToBeRendered =
            portsToBeRenderedTuple
            |> List.collect (fun (x,y) -> y) 
        

        let firstMsg = DrawFromPortToCursor (port.Pos, mousePos, List.tryFind (isWithinDistanceToPort mousePos 10.0) portsToBeRendered)
        let secondMsg = RenderPorts (portsToBeRenderedTuple,false)
        let thirdMsg = RenderOnePort (mousePos,List.tryFind (isWithinDistanceToPort mousePos 10.0) portsToBeRendered)

        model, Cmd.batch [Cmd.ofMsg (firstMsg); Cmd.ofMsg(secondMsg);
                          Cmd.ofMsg (thirdMsg)]

    | DrawFromPortToCursor (startPos,endPos, endPort) -> 
         let newBusModel, newCmd = 
             BusWire.update (BusWire.Msg.DrawFromPortToCursor (startPos,endPos, endPort)) model.Wire
         {model with
             Wire = newBusModel
         }
         , Cmd.none

    | EndDraggingPort mousePos->

        let endOutcome = List.tryFind (isWithinDistanceToPort mousePos 10.0) model.Ports
  
        match endOutcome with 
        | Some endPort  ->  
                let startPort = tryFindPortByPortId model.IdOfPortBeingDragged model.Ports
                let newMsg = 
                    match startPort with
                    |Some startPort when endPort.PortType<>startPort.PortType -> AddWire (startPort,endPort,Init) 
                    |None -> Msg "Error in Port" 
                    | _ -> Msg "endPort must be a different PortType then startPort"

                {model with 
                    IsPortDragging = false;
                    IdOfPortBeingDragged = "null";
                 }, Cmd.batch ([Cmd.ofMsg(newMsg); Cmd.ofMsg(RemoveDrawnLine); Cmd.ofMsg(UpdatePorts)])
        | _ -> 
                {model with 
                    IsPortDragging = false;
                    IdOfPortBeingDragged = "null";
                }, Cmd.batch ([Cmd.ofMsg(RemoveDrawnLine); Cmd.ofMsg(UpdatePorts)])
      
    | UpdatePorts  ->
        let newPorts = Symbol.getAllPorts (model.Wire.Symbol)
        let newBusModel, newCmd = 
            BusWire.update (BusWire.Msg.UpdatedPortsToBusWire newPorts) model.Wire
                    
        {model with 
            Ports = newPorts
            Wire = newBusModel
        }, Cmd.none
    
    | RemoveDrawnLine ->
        let newBusModel, newMsg = 
            BusWire.update (BusWire.Msg.RemoveDrawnLine) model.Wire
        {model with
            Wire = newBusModel
            OnePortToBeRendered = {X = 0.0; Y = 0.0},None;
            DraggingPortsToBeRendered = [];
        }, Cmd.none

    | StartDraggingWire (connectionId, mousePos) -> 
        let newBusModel, newCmd = 
            BusWire.update (BusWire.Msg.StartDraggingWire (connectionId,mousePos)) model.Wire  
        
        {model with 
            Wire = newBusModel
            IsWireSelected = true
            SelectedWire = Some connectionId
        }
        , Cmd.map Wire newCmd

    | DraggingWire (connectionIdOpt, mousePos) -> 
        let connectionId =   
            match connectionIdOpt with 
            |Some x -> x 
            |None -> failwithf "DraggingWire called but not selecting any wire" 
        let newBusModel, newCmd = 
            BusWire.update (BusWire.Msg.DraggingWire (connectionId, mousePos)) model.Wire
        {model with 
            Wire = newBusModel
            IsWireSelected = true
        }
        , Cmd.none

    | AddWire (startPort,endPort,createDU) -> 
        let newBusModel, newCmd = 
            BusWire.update (BusWire.Msg.AddWire (startPort,endPort,createDU)) model.Wire
        {model with
            Wire = newBusModel
        }
        , Cmd.batch [Cmd.ofMsg (RemoveDrawnLine); Cmd.ofMsg(UpdatePorts)]

    | DeselectWire -> 
        let newBusModel, newCmd = 
            BusWire.update (BusWire.Msg.DeselectWire) model.Wire
        {model with 
            Wire = newBusModel
            IsWireSelected = false
            SelectedWire = None
        }
        , Cmd.none

    | StartDrawingRegion pos -> 
        {model with 
            IsDrawingRegion = true
            StartDrawingPosition = Some pos
        }, Cmd.batch [Cmd.ofMsg(DeselectWire); Cmd.ofMsg(DeselectAllSymbols)]

    | DrawingRegion mousePos ->
        let startPos = 
            match model.StartDrawingPosition with
            | Some startPos -> startPos
            | None -> failwithf "START DRAWING POSITON ERROR "
        {model with 
            RegionToBeRendered = createBBoxFromPos startPos mousePos
        }, Cmd.none

    | EndDrawingRegion endPos  -> 
        let startPos = 
            match model.StartDrawingPosition with 
            | Some x -> x 
            | None -> failwithf "StartingPosition is not defined but we are drawing region"
        {model with
            IsDrawingRegion = false
            StartDrawingPosition = None
            RegionToBeRendered = createBBoxFromPos {X = 0.0; Y =0.0} {X = 0.0; Y =0.0}

        }, Cmd.ofMsg(SelectComponentsWithinRegion (createBBoxFromPos startPos endPos))

    | SelectComponentsWithinRegion (bbox) ->             
        let WireModel, newCmd =
            BusWire.update (BusWire.Msg.SelectWiresWithinRegion(bbox)) model.Wire
        let SymModel, newCmd = 
            Symbol.update (Symbol.Msg.SelectSymbolsWithinRegion(bbox)) model.Wire.Symbol
        let newWireModel =  { WireModel with
                                Symbol = SymModel
                            }

        let newModel  =     { model with 
                                Wire = newWireModel
                            }   
        let overallBBox =  Symbol.getOverallBBox newModel.Wire.Symbol
    
        {newModel with
            OverallBBoxToBeRendered = overallBBox}
        , Cmd.none

    | StartDraggingSymbol (sIdList, pagePos) ->
        let newSymModel = 
            (model.Wire.Symbol, sIdList)
            ||> List.fold (startDraggingSymbol pagePos)
            |> List.map (fun sym -> if List.contains sym.Id sIdList then {sym with IsSelected = true} else sym)
        let overallBBox =  Symbol.getOverallBBox newSymModel

        { model with 
            Wire = {model.Wire with Symbol = newSymModel}
            SymIdList = sIdList
            OverallBBoxToBeRendered = overallBBox
        }
        , Cmd.batch [Cmd.ofMsg(RenderPorts ([],false) );] //render no ports

    | DraggingSymbol (sIdList, pagePos) ->
       
        let newSymModel = 
            (model.Wire.Symbol, sIdList)
            ||> List.fold (draggingSymbol pagePos)

        let overallBBox =  Symbol.getOverallBBox newSymModel

        { model with 
            Wire = {model.Wire with Symbol = newSymModel}
            OverallBBoxToBeRendered = overallBBox
        }
        , Cmd.batch [Cmd.ofMsg (UpdatePorts) ;Cmd.ofMsg(AlignBoxes overallBBox)]

    | EndDraggingSymbol sIdList ->
        let newSymModel = 
             (model.Wire.Symbol, sIdList)
             ||> List.fold (endDraggingSymbol)

        { model with 
            Wire = {model.Wire with Symbol = newSymModel}
            SymIdList = []
            AlignmentLinesToBeRendered = []
        }
        ,Cmd.none

    | AlignBoxes bbox -> 
        let flineList = model.Wire.Symbol
                       |>isSymAligned bbox 
        printf ($"focus : {flineList}")
        let firstDiff = flineList  
                        |>List.tryFind (fun (diffPos,line) -> diffPos <> nullPos) 
        let newSym,newOverallBBox = 
            match firstDiff with 
            |Some (diffPos,line) -> shiftSelectedSymbols model.Wire.Symbol diffPos, {
                                    TopLeft = (posAdd model.OverallBBoxToBeRendered.TopLeft diffPos ) ;
                                    BottomRight = (posAdd model.OverallBBoxToBeRendered.BottomRight diffPos);
                                 }
            |None -> model.Wire.Symbol,model.OverallBBoxToBeRendered
                      
        { model with
             AlignmentLinesToBeRendered = List.map (snd) flineList
             OverallBBoxToBeRendered =  newOverallBBox
             Wire = {model.Wire with Symbol = newSym}
        }
        ,Cmd.none

    | DeselectAllSymbols  ->
        let newSymModel, newCmd = 
            Symbol.update (Symbol.Msg.DeselectAllSymbols) model.Wire.Symbol
        { model with 
            Wire = {model.Wire with Symbol = newSymModel}
            SymIdList = []
            OverallBBoxToBeRendered = nullBBox
        }
         ,Cmd.ofMsg (UpdatePorts)

    | MouseMsg mMsg ->     
        let command = 
            match mMsg.Op with 
            | Move -> 
                let portsWithinMinRange = getPortsWithinMinRange mMsg.Pos model 90.0 All
                [RenderPorts (portsWithinMinRange, true)] //isHovering
            ///Clicking order : Ports -> Wire -> Symbol         
            | Down -> 
                let ClickedPort = List.tryFind (isPortClicked mMsg.Pos) model.Ports  
                match ClickedPort with 
                | Some port -> [DeselectAllSymbols; DeselectWire; StartDraggingPort (port)]
                | None  -> 
                    let ClickedWire = BusWire.wireToSelectOpt model.Wire mMsg.Pos
                    match ClickedWire with 
                    | Some wireId -> [DeselectAllSymbols;DeselectWire; StartDraggingWire (wireId,mMsg.Pos)]
                    | None ->
                        let ClickedSym = List.tryFind (Symbol.isSymClicked mMsg.Pos) model.Wire.Symbol
                        let sIdList = Symbol.getSelectedSymbolIds model.Wire.Symbol
                        match ClickedSym with
                        | Some sym when (sym.IsSelected = false) -> [DeselectAllSymbols;DeselectWire; StartDraggingSymbol ([sym.Id], mMsg.Pos)] 
                        | Some sym when (sym.IsSelected = true) -> [StartDraggingSymbol (sIdList, mMsg.Pos)]
                        | None -> [StartDrawingRegion mMsg.Pos]          
                        | _ -> failwithf "Won't Happen"
            
            | Drag when (model.IsPortDragging = true) -> [DraggingPort (mMsg.Pos)]
            
            | Drag when (Symbol.getSelectedSymbolIds model.Wire.Symbol <> []) -> 
                    let sIdList = Symbol.getSelectedSymbolIds model.Wire.Symbol
                    [DraggingSymbol (sIdList, mMsg.Pos)]
                             
            | Drag when (model.IsDrawingRegion = true) -> [DrawingRegion (mMsg.Pos)]

            | Drag when (model.IsWireSelected = true) -> [DraggingWire (model.SelectedWire,mMsg.Pos)]
            
            | Up when (model.IsPortDragging = true )-> [EndDraggingPort (mMsg.Pos)] 
            
            | Up when (model.IsDrawingRegion = true) -> [EndDrawingRegion (mMsg.Pos)]
            
            | Up ->[EndDraggingSymbol (model.SymIdList)]
            
            | _ -> failwithf ""

        let listOfCommands =  
            command
            |> List.map (Cmd.ofMsg)  
                    
        model, Cmd.batch(listOfCommands) // allow unused mouse messags 
   
    | Msg str -> printf $"{str}"
                 model, Cmd.none

    | Zoom msg -> {model with Canvas = msg}, Cmd.none 


let init() = 
    let wModel,cmdw = (BusWire.init 0)()
    {
        Wire = wModel
        Canvas = {height = CommonTypes.draw2dCanvasHeight ; width = CommonTypes.draw2dCanvasWidth; zoom = 1.25}
        SymIdList =  []
        Ports = Symbol.getAllPorts (wModel.Symbol)
        HoveringPortsToBeRendered = []
        DraggingPortsToBeRendered = []
        IsPortDragging = false
        IdOfPortBeingDragged = "null"
        IsWireSelected = false
        IsDrawingRegion = false
        RegionToBeRendered = nullBBox
        AlignmentLinesToBeRendered = []
        StartDrawingPosition = None
        SelectedWire = None
        OnePortToBeRendered = nullPos,None
        OverallBBoxToBeRendered = nullBBox
    }, Cmd.map Wire cmdw
