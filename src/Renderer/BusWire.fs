module BusWire

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers


//------------------------------------------------------------------------//
//------------------------------BusWire Types-----------------------------//
//------------------------------------------------------------------------//

type ConnectPoint = {
    Centre: XYPos
    Id: CommonTypes.ComponentId
    Select: bool
}


type Wire = {
    Id : CommonTypes.ConnectionId 
    SourceIds : CommonTypes.ComponentId * string
    TargetIds : CommonTypes.ComponentId * string
    IsSelected : bool
    HasError : bool 
    relativPositions : XYPos list
    PrevPositions : XYPos list
    BeingDragged : int
    BusWidth : int Option
}





type Model = {
    Symbol: Symbol.Model
    WX: Wire list
    Color: CommonTypes.HighLightColor
    Countselected: int // Capitalize start of words
    PortToCursor : XYPos * XYPos * Symbol.Port option
}

//----------------------------Message Type-----------------------------------//
type Msg =
    | Symbol of Symbol.Msg
    | SetColor of CommonTypes.HighLightColor
    | MouseMsg of MouseT
    | DrawFromPortToCursor of XYPos*XYPos* Symbol.Port option
    | AddWire of Symbol.Port*Symbol.Port*CreateDU
    | SelectWire of CommonTypes.ConnectionId
    | DeleteWire
    // | UpdateBusWirePorts
    | DeselectWire
    | StartDraggingWire of CommonTypes.ConnectionId*XYPos
    | DraggingWire of CommonTypes.ConnectionId*XYPos
    | RemoveDrawnLine
    | SelectWiresWithinRegion of Helpers.BoundingBox
    | EnforceBusWidth of int * Wire
    | UpdateWires

let origin = {X=0.0 ; Y=0.0}

let posOf x y = {X=x;Y=y}

/// look up wire in WireModel
let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
    let correctwire cable =
        cable.Id = wId
    match wModel.WX |> List.tryFind (correctwire) with
    | Some wr -> wr
    | None    -> failwithf "Ghost"

type RenderWireProps = {
    key : CommonTypes.ConnectionId
    Wire: Wire
    ColorP: string
    StrokeWidthP: string 
    SrcOrient: PortOrientation
    TgtOrient: PortOrientation
    Dispatch : Dispatch<Msg>
    SrcPos: XYPos
    TrgtPos: XYPos
}

let getPortsOfWire (wModel: Model) (w: Wire)  = 
    let srcPort = 
        Symbol.getSymbolWithId wModel.Symbol (fst w.SourceIds)
        |> Symbol.getPortWithId (snd w.SourceIds)
    let trgtPort = 
        Symbol.getSymbolWithId wModel.Symbol (fst w.TargetIds)
        |> Symbol.getPortWithId (snd w.TargetIds)
    srcPort, trgtPort

let getSrcAndTrgtOrientation (wModel: Model) (wire: Wire) =
    let srcPort, trgtPort = getPortsOfWire wModel wire
    Symbol.getOrientationOfPort wModel.Symbol srcPort,
    Symbol.getOrientationOfPort wModel.Symbol trgtPort

let selectBoundedWires (wModel: Model) (boundary: BoundingBox) =
    let selectWireinBounds (wr: Wire) =
        let inBounds (point: XYPos) =
            point.X > boundary.TopLeft.X && point.X < boundary.BottomRight.X && point.Y > boundary.TopLeft.Y && point.Y < boundary.BottomRight.Y
        let srcPort, trgtPort = getPortsOfWire wModel wr
            
        if inBounds srcPort.Pos && inBounds trgtPort.Pos then
            {wr with IsSelected = true}
        else wr
    List.map selectWireinBounds wModel.WX

let drawLineToCursor (startPos : XYPos, endPos : XYPos, endPort : Symbol.Port option) = 

    let drawLineColor,strokeWidth,strokeDashArray = 
        match endPort with
        | Some port -> drawLineColor_special, "5.0", "15.0 15.0"
        | None -> drawLineColor_const, "2.0", "7.0 7.0"

    g   []
        [    
            line [
                X1 startPos.X
                Y1 startPos.Y
                X2 endPos.X
                Y2 endPos.Y
                SVGAttr.Stroke drawLineColor
                SVGAttr.StrokeWidth strokeWidth
                SVGAttr.StrokeDasharray strokeDashArray
        ] []
        ]

let busWidthAnnotation (wire : Wire) = 
    match wire.BusWidth with
    | None | Some 1 -> [] 
    | Some w -> [str $"{w}"]


let pairListElements sequence  = 
    let revseq = List.rev sequence
    match sequence,revseq with 
    |hd::bodyone , tl::bodytwo -> List.rev bodytwo , bodyone
    |_ -> sequence,sequence

let vertexlstZero (cable: Wire) (wModel: Model) (srcOrient: PortOrientation) (tgtOrient: PortOrientation) =
    let srcPort, trgtPort = getPortsOfWire wModel cable
    let startpt, endpt = srcPort.Pos, trgtPort.Pos

    let midX = (startpt.X+endpt.X)/2.0
    let midY = (startpt.Y+endpt.Y)/2.0

    let leftof ptwo pone = pone.X < ptwo.X
    let above ptwo pone = pone.Y < ptwo.Y
    let below ptwo pone = pone.Y > ptwo.Y
    let rightof ptwo pone = pone.X > ptwo.X

    let moveright x p = {p with X = p.X + x}
    let movedown y p = {p with Y = p.Y + y}
    
    let moveHorizontallyTo x p = {p with X = x}
    let moveVerticallyTo y p = {p with Y = y}

    match srcOrient,tgtOrient with
    | Right, Bottom -> 
        match (startpt |> leftof endpt) , (startpt |> below endpt) with
        | true, true -> [startpt ; startpt |> moveHorizontallyTo endpt.X ; endpt]
        | true, false 
        | false, true  -> [startpt ; startpt |> moveright 15.0 ; {X=startpt.X+15.0 ; Y=endpt.Y+15.0} ; endpt |> movedown 15.0 ; endpt]
        | false, false -> [startpt ; startpt |> moveright 100.0 ; {X=startpt.X+100.0;Y=endpt.Y+15.0} ; endpt |> movedown 15.0 ; endpt]
    | Right, Top -> 
        match (startpt |> leftof endpt) , (startpt |> above endpt) with
        | true, true -> [startpt ; {X=endpt.X;Y=startpt.Y} ; endpt]
        | true, false 
        | false, true -> [startpt ; startpt |> moveright 15.0 ; {X=startpt.X+15.0;Y=endpt.Y-15.0} ; endpt |> movedown -15.0 ; endpt]
        | false, false -> [startpt ; startpt |> moveright 100.0 ; {X=startpt.X+100.0;Y=endpt.Y-15.0} ; endpt |> movedown -15.0 ; endpt]
    | Right, Left -> 
        match (startpt |> leftof endpt) with 
        | true -> [startpt ; startpt |> moveHorizontallyTo midX ; endpt |> moveHorizontallyTo midX ; endpt]
        | false -> [startpt ; startpt |> moveright 15.0 ; {X=startpt.X+15.0;Y=midY} ; {X=endpt.X-15.0;Y=midY} ; endpt |> moveright -15.0 ; endpt]
    | Right, Right -> [startpt ; {X=startpt.X+15.0;Y=startpt.Y} ; {X=startpt.X+15.0;Y=endpt.Y} ; endpt]
    | Left, Bottom -> 
        match (startpt |> rightof endpt) , (startpt |> below endpt) with
        | true, true -> [startpt ; {X=endpt.X;Y=startpt.Y} ; endpt]
        | true, false 
        | false, true -> [startpt ; {X=startpt.X-15.0;Y=startpt.Y} ; {X=startpt.X-15.0;Y=endpt.Y+15.0} ; {X=endpt.X;Y=endpt.Y+15.0} ; endpt]
        | false, false -> [startpt ; {X=startpt.X-100.0;Y=startpt.Y} ; {X=startpt.X-100.0;Y=endpt.Y+15.0} ; {X=endpt.X;Y=endpt.Y+15.0} ; endpt]
    | Left, Top -> 
        match (startpt |> rightof endpt) , (startpt |> above endpt) with 
        | true, true -> [startpt ; startpt |> moveHorizontallyTo endpt.X ; endpt]
        | true, false 
        | false, true -> [startpt ; {X=startpt.X-15.0;Y=startpt.Y} ; {X=startpt.X-15.0;Y=endpt.Y-15.0} ; {X=endpt.X;Y=endpt.Y-15.0} ; endpt]
        | false, false -> [startpt ; {X=startpt.X-100.0;Y=startpt.Y} ; {X=startpt.X-100.0;Y=endpt.Y-15.0} ; {X=endpt.X;Y=endpt.Y-15.0} ; endpt]
    | Left, Right -> 
        match (startpt |> rightof endpt) with 
        | true  -> [startpt ; startpt |> moveHorizontallyTo midX ; endpt |> moveHorizontallyTo midX ; endpt]
        | false -> [startpt ; {X=startpt.X-15.0;Y=startpt.Y} ; {X=startpt.X-15.0;Y=midY} ; {X=endpt.X+15.0;Y=midY} ; {X=endpt.X+15.0;Y=endpt.Y} ; endpt]
    | Top, Bottom -> 
        match (startpt |> above endpt) with
        | true  -> [startpt ; startpt |> moveVerticallyTo midY ; endpt |> moveVerticallyTo midY ; endpt]
        | false -> [startpt ; startpt |> movedown -15.0 ; {X=midX;Y=startpt.Y-15.0} ; {X=midX;Y=endpt.Y+15.0} ; endpt |> movedown 15.0 ; endpt]
    | Top, Left -> 
        match (startpt |> leftof endpt) , (startpt |> below endpt) with
        | true, true -> [startpt ; startpt |> moveVerticallyTo endpt.Y ; endpt]
        | true, false
        | false, true  -> [startpt ; {X=startpt.X;Y=startpt.Y-15.0} ; {X=endpt.X-15.0;Y=startpt.Y-15.0} ; {X=endpt.X-15.0;Y=endpt.Y} ; endpt]
        | false, false -> [startpt ; {X=startpt.X;Y=startpt.Y-100.0} ; {X=endpt.X-15.0;Y=startpt.Y-100.0} ; {X=endpt.X-15.0;Y=endpt.Y} ; endpt]
    | Top, Right -> 
        match (startpt |> rightof endpt) , (startpt |> below endpt) with
        | true, true -> [startpt ; {X=startpt.X;Y=endpt.Y} ; endpt]
        | true, false
        | false, true  -> [startpt ; {X=startpt.X;Y=startpt.Y-15.0} ; {X=endpt.X+15.0;Y=startpt.Y-15.0} ; {X=endpt.X+15.0;Y=endpt.Y} ; endpt]
        | false, false -> [startpt ; {X=startpt.X;Y=startpt.Y+100.0} ; {X=endpt.X-15.0;Y=startpt.Y+100.0} ; {X=endpt.X+15.0;Y=endpt.Y} ; endpt]
    | Top, Top -> 
        match (startpt |> above endpt) with
        | true  -> [startpt ; startpt |> movedown -15.0 ; startpt |> movedown -15.0 |> moveHorizontallyTo endpt.X ; endpt]
        | false -> [startpt ; endpt |> movedown -15.0 |> moveHorizontallyTo startpt.X ; endpt |> movedown -15.0 ; endpt]
    | Bottom, Bottom -> 
        match (startpt |> below endpt) with
        | true ->  [startpt ; {X=startpt.X;Y=startpt.Y+15.0} ; {X=endpt.X;Y=startpt.Y+15.0} ; endpt]
        | false -> [startpt ; {X=startpt.X;Y=endpt.Y+15.0} ; {X=endpt.X;Y=endpt.Y+15.0} ; endpt]
    | Bottom, Top ->  
        match (startpt |> above endpt) with
        | true  -> [startpt ; {X=startpt.X;Y=midY} ; {X=endpt.X;Y=midY} ; endpt]
        | false -> [startpt ; {X=startpt.X;Y=startpt.Y+15.0} ; {X=midX;Y=startpt.Y+15.0} ; {X=midX;Y=endpt.Y-15.0} ; {X=endpt.X;Y=endpt.Y-15.0} ; endpt]
    | Bottom, Left -> 
        match (startpt |> leftof endpt) , (startpt |> above endpt) with
        | true, true -> [startpt ; {X=startpt.X;Y=endpt.Y} ; endpt]
        | true, false
        | false, true -> [startpt ; {X=startpt.X;Y=startpt.Y+15.0} ; {X=endpt.X-15.0;Y=startpt.Y+15.0} ; {X=endpt.X-15.0;Y=endpt.Y} ; endpt]
        | false, false -> [startpt ; {X=startpt.X;Y=startpt.Y+100.0} ; {X=endpt.X-15.0;Y=startpt.Y+100.0} ; {X=endpt.X-15.0;Y=endpt.Y} ; endpt]
    | Bottom, Right -> 
        match (startpt |> rightof endpt) , (startpt |> above endpt) with
        | true, true -> [startpt ; startpt |> moveVerticallyTo endpt.Y ; endpt]
        | true, false
        | false, true -> [startpt ; {X=startpt.X;Y=startpt.Y+15.0} ; {X=endpt.X+15.0;Y=startpt.Y+15.0} ; {X=endpt.X+15.0;Y=endpt.Y} ; endpt]
        | false, false -> [startpt ; {X=startpt.X;Y=startpt.Y+100.0} ; {X=endpt.X+15.0;Y=startpt.Y+100.0} ; {X=endpt.X+15.0;Y=endpt.Y} ; endpt]
    | _ ->  [startpt ; {X=midX;Y=startpt.Y} ; {X=midX;Y=endpt.Y} ; endpt]







let vertexList (wModel: Model) (wire: Wire) = 
    let a, b, c = 
        match wire.relativPositions with
        | [a; b; c] -> a, b, c
        | _ -> failwithf "Shouldn't happen?"
    
    let srcPort, trgtPort = getPortsOfWire wModel wire
    let srcOrient, trgtOrient = getSrcAndTrgtOrientation wModel wire

    let startpt, endpt = srcPort.Pos, trgtPort.Pos
    
    let midX = (startpt.X+endpt.X)/2.0
    let midY = (startpt.Y+endpt.Y)/2.0
    
    let leftof ptwo pone = pone.X < ptwo.X
    let above ptwo pone = pone.Y < ptwo.Y
    let below ptwo pone = pone.Y > ptwo.Y
    let rightof ptwo pone = pone.X > ptwo.X

    let moveright x p = {p with X = p.X + x}
    let movedown y p = {p with Y = p.Y + y}
    
    let moveHorizontallyTo x p = {p with X = x}
    let moveVerticallyTo y p = {p with Y = y}

    match srcOrient,trgtOrient with
    | Right, Bottom -> 
        match (startpt |> leftof endpt) , (startpt |> below endpt) with
        | true,true -> [startpt ; startpt |> moveHorizontallyTo endpt.X ; endpt]
        | true,false 
        | false,true -> [startpt ; {X=startpt.X+15.0+a.X;Y=startpt.Y} ; {X=startpt.X+15.0+a.X;Y=endpt.Y+15.0+b.Y} ; {X=endpt.X;Y=endpt.Y+15.0+b.Y} ; endpt]
        | false,false -> [startpt ; {X=startpt.X+100.0+a.X;Y=startpt.Y} ; {X=startpt.X+100.0+a.X;Y=endpt.Y+15.0+b.Y} ; {X=endpt.X;Y=endpt.Y+15.0+b.Y} ; endpt]
    | Right, Top -> 
        match (startpt |> leftof endpt) , (startpt |> above endpt) with
        | true,true -> [startpt ; {X=endpt.X;Y=startpt.Y} ; endpt]
        | true,false 
        | false,true -> [startpt ; startpt |> moveright(15.0+a.X) ; {X=startpt.X+15.0+a.X;Y=endpt.Y-15.0+b.Y} ; endpt |> movedown(-15.0+b.Y) ; endpt]
        | false,false -> [startpt ; startpt |> moveright(100.0+a.X) ; {X=startpt.X+100.0+a.X;Y=endpt.Y-15.0+b.Y} ; endpt |> movedown(-15.0+b.Y) ; endpt]
    | Right, Left -> 
        match (startpt |> leftof endpt) with 
        | true -> [startpt ; startpt |> moveHorizontallyTo (midX+a.X) ; endpt |> moveHorizontallyTo (midX+a.X) ; endpt]
        | false -> [startpt ; {X=startpt.X+15.0+a.X;Y=startpt.Y} ; {X=startpt.X+15.0+a.X;Y=midY+b.Y} ; {X=endpt.X-15.0+c.X;Y=midY+b.Y} ; {X=endpt.X-15.0+c.X;Y=endpt.Y} ; endpt]
    | Right, Right -> [startpt ; {X=startpt.X+15.0;Y=startpt.Y} ; {X=startpt.X+15.0;Y=endpt.Y} ; endpt]
    | Left, Bottom -> 
        match (startpt |> rightof endpt) , (startpt |> below endpt) with
        | true,true -> [startpt ; startpt |> moveHorizontallyTo midX ; endpt]
        | true,false 
        | false,true -> [startpt ; {X=startpt.X-15.0+a.X;Y=startpt.Y} ; {X=startpt.X-15.0+a.X;Y=endpt.Y+15.0+b.Y} ; {X=endpt.X;Y=endpt.Y+15.0+b.Y} ; endpt]
        | false,false -> [startpt ; {X=startpt.X-100.0+a.X;Y=startpt.Y} ; {X=startpt.X-100.0+a.X;Y=endpt.Y+15.0+b.Y} ; {X=endpt.X;Y=endpt.Y+15.0+b.Y} ; endpt]
    | Left, Top -> 
        match (startpt |> rightof endpt) , (startpt |> above endpt) with
        | true,true -> [startpt ; startpt |> moveHorizontallyTo endpt.X ; endpt]
        | true,false 
        | false,true -> [startpt ; startpt |> moveright(-15.0+a.X) ; {X=startpt.X-15.0+a.X;Y=endpt.Y-15.0+b.Y} ; endpt |> movedown(-15.0+b.Y) ; endpt]
        | false,false -> [startpt ; startpt |> moveright(-100.0+a.X) ; {X=startpt.X-100.0+a.X;Y=endpt.Y-15.0+b.Y} ; endpt |> movedown(-15.0+b.Y) ; endpt]
    | Left, Right -> 
        match (startpt |> rightof endpt) with 
        | true -> [startpt ; {X=midX+a.X;Y=startpt.Y} ; {X=midX+a.X;Y=endpt.Y} ; endpt]
        | false -> [startpt ; {X=startpt.X-15.0+a.X;Y=startpt.Y} ; {X=startpt.X-15.0+a.X;Y=midY+b.Y} ; {X=endpt.X+15.0+c.X;Y=midY+b.Y} ; {X=endpt.X+15.0+c.X;Y=endpt.Y} ; endpt]
    | Top, Bottom -> 
        match (startpt |> below endpt) with
        | true -> [startpt ; {X=startpt.X;Y=midY+a.Y} ; {X=endpt.X;Y=midY+a.Y} ; endpt]
        | false -> [startpt ; {X=startpt.X;Y=startpt.Y-15.0+a.Y} ; {X=midX+b.X;Y=startpt.Y-15.0+a.Y} ; {X=midX+b.X;Y=endpt.Y+15.0+c.Y} ; {X=endpt.X;Y=endpt.Y+15.0+c.Y} ; endpt]
    | Top, Left -> 
        match (startpt |> leftof endpt) , (startpt |> below endpt) with
        | true,true -> [startpt ; {X=startpt.X;Y=endpt.Y} ; endpt]
        | true,false
        | false,true -> [startpt ; {X=startpt.X;Y=startpt.Y-15.0+a.Y} ; {X=endpt.X-15.0+b.X;Y=startpt.Y-15.0+a.Y} ; {X=endpt.X-15.0+b.X;Y=endpt.Y} ; endpt]
        | false,false -> [startpt ; {X=startpt.X;Y=startpt.Y-100.0+a.Y} ; {X=endpt.X-15.0+b.X;Y=startpt.Y-100.0+a.Y} ; {X=endpt.X-15.0+b.X;Y=endpt.Y} ; endpt]
    | Top, Right -> 
        match (startpt |> rightof endpt) , (startpt |> below endpt) with
        | true,true -> [startpt ; {X=startpt.X;Y=endpt.Y} ; endpt]
        | true,false
        | false,true -> [startpt ; startpt |> movedown (-15.0+a.Y) ; {X=endpt.X+15.0+b.X;Y=startpt.Y-15.0+a.Y} ; endpt |> moveright (15.0+b.X) ; endpt]
        | false,false -> [startpt ; startpt |> movedown (-100.0+a.Y) ; {X=endpt.X+15.0+b.X;Y=startpt.Y-100.0+a.Y} ; endpt |> moveright (15.0+b.X) ; endpt]
    | Top, Top -> 
        match (startpt |> above endpt) with
        | true -> [startpt ; {X=startpt.X;Y=startpt.Y-15.0+a.Y} ; {X=endpt.X;Y=startpt.Y-15.0+a.Y} ; endpt]
        | false -> [startpt ; {X=startpt.X;Y=endpt.Y-15.0+a.Y} ; {X=endpt.X;Y=endpt.Y-15.0+a.Y} ; endpt]
    | Bottom, Bottom -> 
        match (startpt |> below endpt) with
        | true -> [startpt ; startpt |> movedown (15.0+a.Y) ; {X=endpt.X;Y=startpt.Y+15.0+a.Y} ; endpt]
        | false -> [startpt ; {X=startpt.X;Y=endpt.Y+15.0+a.Y} ; endpt |> movedown (15.0+a.Y) ; endpt]
    | Bottom, Top ->  
        match (startpt |> above endpt) with
        | true  -> [startpt ; {X=startpt.X;Y=midY+a.Y} ; {X=endpt.X;Y=midY+a.Y} ; endpt]
        | false -> [startpt ; startpt |> movedown (15.0+a.Y) ; {X=midX+b.X;Y=startpt.Y+15.0+a.Y} ; {X=midX+b.X;Y=endpt.Y-15.0+c.Y} ; endpt |> movedown (-15.0+c.Y) ; endpt]
    | Bottom, Left -> 
        match (startpt |> leftof endpt) , (startpt |> above endpt) with
        | true, true -> [startpt ; startpt |> moveVerticallyTo endpt.Y ; endpt]
        | true, false
        | false, true  -> [startpt ; startpt |> movedown(15.0+a.Y) ; {X=endpt.X-15.0+b.X;Y=startpt.Y+15.0+a.Y} ; endpt |> moveright (-15.0+b.X) ; endpt]
        | false, false -> [startpt ; {X=startpt.X;Y=startpt.Y+100.0+a.Y} ; {X=endpt.X-15.0+b.X;Y=startpt.Y+100.0+a.Y} ; {X=endpt.X-15.0+b.X;Y=endpt.Y} ; endpt]
    | Bottom, Right -> 
        match (startpt |> rightof endpt) , (startpt |> above endpt) with
        | true, true -> [startpt ; startpt |> moveVerticallyTo endpt.Y ; endpt]
        | true, false
        | false, true  -> [startpt ; startpt |> movedown (15.0+a.Y) ; {X=endpt.X+15.0+b.X;Y=startpt.Y+15.0+a.Y} ; endpt |> moveright (15.0+b.X) ; endpt]
        | false, false -> [startpt ; startpt |> movedown (100.0+a.Y) ; {X=endpt.X+15.0+b.X;Y=startpt.Y+100.0+a.Y} ; endpt |> moveright (100.0+b.X) ; endpt]
    | _ ->  [startpt ; {X=midX;Y=startpt.Y} ; {X=midX;Y=endpt.Y} ; endpt]

let bounds (cable: Wire) (wModel: Model) =
    let individualBound (start:XYPos) (final: XYPos) =
        if (final.Y > start.Y || final.X > start.X) then
            {TopLeft = {X=start.X-10.0;Y=start.Y-10.0} ;
             BottomRight = {X=final.X+10.0;Y=final.Y+10.0}}
         else {TopLeft = {X=final.X-10.0;Y=final.Y-10.0} ;
             BottomRight = {X=start.X+10.0;Y=start.Y+10.0}}
    let vlst = vertexList wModel cable 
    let startlst , endlst = pairListElements vlst
    List.map2 individualBound startlst endlst

let displayWireSegment (props: RenderWireProps) (start: XYPos) (final: XYPos) =
    let color =
        match props.Wire.IsSelected, props.Wire.HasError, props.Wire.BusWidth with
        | true, _, _ -> "green" 
        | _, true, _ -> "red" 
        | _, _, Some w when w > 1 -> "purple"
        | _, _, None -> "purple"
        | _ -> "black"

    g   []
        [    
            line [
                X1 start.X
                Y1 start.Y
                X2 final.X
                Y2 final.Y
                SVGAttr.Stroke color
                SVGAttr.StrokeWidth props.StrokeWidthP
            ] []
        ]

let displayFullWire wModel (props: RenderWireProps) (vertices: list<XYPos>) = 
    
    let srcPort, trgtPort = getPortsOfWire wModel props.Wire
    
    let startlst, endlst = pairListElements vertices
    let reactlst = List.map2 (displayWireSegment props) startlst endlst 
    let annotatePos = if srcPort.PortType = CommonTypes.Output then srcPort.Pos else trgtPort.Pos 

    text [
        X (annotatePos.X + 20.0)
        Y (annotatePos.Y + 2.0)
        Style [
            TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
            DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
            FontSize "10px"
            FontWeight "Bold"
            Fill "Black"                // demo font color
            UserSelect UserSelectOptions.None
            PointerEvents "none"
        ]
    ]  (busWidthAnnotation props.Wire) :: reactlst
        |> (fun reactElemLst -> 
        g [] reactElemLst)
  

let autoSingleWireView wModel  = 
    FunctionComponent.Of (
        fun (props: RenderWireProps) ->  

            g   []
                [
                    displayFullWire wModel props (vertexList wModel props.Wire)
                ]
        , "Wire"
        ,equalsButFunctions)

let view (wModel:Model) (dispatch: Msg -> unit)= 
    let wires = 
        wModel.WX
        |> List.map (fun wire ->
            let srcPort, trgtPort = getPortsOfWire wModel wire
            let props = {
                Dispatch = dispatch
                key = wire.Id
                Wire = wire
                ColorP = wModel.Color.Text()
                StrokeWidthP = 
                    match wire.HasError, wire.BusWidth with
                    | false, Some w when w = 1 -> "1px"
                    | _ -> "3px" 
                SrcPos = srcPort.Pos
                TrgtPos = trgtPort.Pos
                SrcOrient = fst (getSrcAndTrgtOrientation wModel wire)
                TgtOrient = snd (getSrcAndTrgtOrientation wModel wire)
            }
            autoSingleWireView wModel props
        )
    let symbols = SymbolRenderers.view wModel.Symbol (fun sMsg -> dispatch (Symbol sMsg))
    let lineToCursor= drawLineToCursor wModel.PortToCursor
    
    g [] [(g [] wires) ; symbols; lineToCursor]


let init () =
    let symbols, cmd = Symbol.init()   
    {WX=[];Symbol=symbols; Color=CommonTypes.Red  ; Countselected = 0 ; PortToCursor = ({X = 0.0; Y= 0.0},{X = 0.0; Y= 0.0}, None)},Cmd.none 


let createWire (srcPort: Symbol.Port) (trgtPort: Symbol.Port)  =
    {
        Id = CommonTypes.ConnectionId (uuid())
        SourceIds = srcPort.HostId, srcPort.Id
        TargetIds = trgtPort.HostId, trgtPort.Id
        IsSelected = false 
        HasError = false 
        relativPositions = [{X=0.0 ; Y=0.0} ; origin ; origin]
        PrevPositions = [origin ; origin ; origin]
        BeingDragged = -1
        BusWidth = 
            match srcPort.BusWidth, trgtPort.BusWidth with
            | Some wStart, Some wEnd -> Some wStart
            | None, Some w | Some w, None -> Some w
            | None, None -> None
    }

let getWiresToBeDuplicated (wModel) (displacementPos: XYPos ) (wireList: Wire list) (portList: Symbol.Port list) : (Symbol.Port option * Symbol.Port option* Wire) list  =    
    wireList
    |> List.map (fun wire -> 
        let srcPort, trgtPort = getPortsOfWire wModel wire
        let sourcePort =  
            portList
            |> List.tryFind (fun port ->  
                (calcDistance (posAdd srcPort.Pos displacementPos) port.Pos) < 0.001)                   
        let endPort = 
            portList
            |> List.tryFind (fun port ->  
                (calcDistance (posAdd trgtPort.Pos displacementPos) port.Pos) < 0.001) 
        sourcePort, endPort, wire
    )
                
let enforceBusWidth (symModel: Symbol.Symbol list) (busWidth: int) (undefPort: Symbol.Port) : Symbol.Symbol list =
    symModel
    |> List.map (fun sym -> 
        match (sym.Id = undefPort.HostId) with 
        | true ->  
            sym
            |> Symbol.updateSymWithPort {undefPort with BusWidth = Some busWidth} 
            |> Symbol.autoCompleteWidths
        | false -> sym        
    )
    

let addWire (startPortTmp : Symbol.Port) (endPortTmp : Symbol.Port) (createDU : CreateDU) (symModel : Symbol.Symbol list) =
    let inputPort = if startPortTmp.PortType = CommonTypes.Input then startPortTmp else endPortTmp
    if inputPort.NumOfConnections <> 0 then failwithf "createError : InputPort can only have One Driver. If you want to merge two wires use MergeWire"
    
    let duplicate =      
        match createDU with 
        | Init -> false
        | Duplicated-> true

    let startPort = {startPortTmp with NumOfConnections = startPortTmp.NumOfConnections + 1}
    let endPort   = {endPortTmp   with NumOfConnections = endPortTmp.NumOfConnections   + 1}
    let tmpSymModel  =
        symModel
        |> List.map (fun sym -> if sym.Id = startPort.HostId then Symbol.changeNumOfConnections Increment startPort.Id sym  else sym)
        |> List.map (fun sym -> if sym.Id = endPort.HostId   then Symbol.changeNumOfConnections Increment endPort.Id   sym  else sym)
    
    let newWire, newSym = 
        match startPort, startPort.BusWidth, endPort, endPort.BusWidth with 
        | _, Some startWidth, _,  Some endWidth ->
            match startWidth = endWidth with
            | true -> 
                let wire = createWire startPort endPort 
                Some wire, tmpSymModel
            | false -> 
                let errorWire = { (createWire startPort endPort) with HasError = true} 
                let newSymModel, symMsg = 
                    Symbol.update (Symbol.Msg.AddErrorToPorts (startPort, endPort)) tmpSymModel
                Some errorWire, newSymModel
       
        | _, Some width, undefinedPort, None | undefinedPort, None, _, Some width ->
            let wire = createWire startPort endPort  
            let newSymModel = enforceBusWidth tmpSymModel width undefinedPort
            Some wire, newSymModel

        | _-> let wire = createWire startPort endPort   //failwithf " BusWidths of sourcePort and targetPort are not specified !!!!"
              Some wire, tmpSymModel
    if (duplicate) then 
        match newWire with
        | Some w -> Some {w with IsSelected = true}, newSym
        | None -> None, newSym
    else newWire, newSym




 
let startWireDragging (cable: Wire) (pos: XYPos) (wModel: Model)=
    let inShape (b:BoundingBox) =
        pos.X > b.TopLeft.X && pos.X < b.BottomRight.X && pos.Y > b.TopLeft.Y && pos.Y < b.BottomRight.Y
    let draggedSegment =
        match List.map inShape (bounds cable wModel) with
        | [false ; true ; false] -> 0
        | [false ; true ; false ; false] -> 0
        | [false ; false ; true ; false] -> 1
        | [false ; true ; false ; false ; false] -> 0
        | [false ; false ; true ; false ; false] -> 1
        | [false ; false ; false ; true ; false] -> 2
        | _ -> -1
    {cable with BeingDragged = draggedSegment}

let dragAWire (cable: Wire) (pos: XYPos) (wModel: Model) (srcOrient: PortOrientation) (tgtOrient: PortOrientation) =
    let vLs = 
        match vertexlstZero cable wModel srcOrient tgtOrient with
        | [g ; h; i ; j ; k ; u] -> h, i, j
        | [s ; t ; u ; v ; w] -> t, u, v
        | [l ; m ; n ; v] -> m, n, v
        | _ -> origin, origin, origin
    let o, p, q = vLs
    let a, b, c = 
        match cable.relativPositions with
        | [g ; h ; i] -> g , h , i
        | _ -> origin , origin , origin 
    let d, e, f = 
        match cable.PrevPositions with
        | [g ; h ; i] -> g , h , i
        | _ -> origin , origin , origin 
    match cable.BeingDragged with
    | 0 ->  
        let offset = o
        let correctedPos = posDiff pos offset 
        let diff = posDiff correctedPos d           
        { cable  with
            relativPositions = [(posAdd a diff) ; b ; c]
            PrevPositions = [correctedPos ; e ; f]
        }
    | 1 ->  
        let offset = p
        let correctedPos = posDiff pos offset
        let diff = posDiff correctedPos e 
        { cable with 
            relativPositions = [a ; (posAdd diff b) ; c]
            PrevPositions = [d ; correctedPos ; f]
        }
    | 2 ->  
        let offset = q
        let correctedPos = posDiff pos offset
        let diff = posDiff correctedPos f
        { cable with 
            relativPositions = [a ; b ; (posAdd diff) c]
            PrevPositions = [d ; e ; correctedPos]
        }   
    | _ -> cable      

let endWireDragging (cable: Wire) =
    {cable with BeingDragged = -1}

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd
    | SetColor c -> {model with Color = c}, Cmd.none
    | SelectWire wId->         
        let clickedWire =
            List.map (fun w ->
                        if w.Id <> wId then w
                        else {w with IsSelected = true}
                      )
        let newWls = clickedWire model.WX 
        {model with WX = newWls} ,Cmd.none
    // | UpdateBusWirePorts ->
    //     let portList = Symbol.getAllPorts (model.Symbol)
    //     let newW wir =
    //         let updateWS wr =
    //             let foundsrc (prt: Symbol.Port) = (wr.SourcePort.Id = prt.Id)
    //             match List.tryFind (foundsrc) portList with
    //             | Some pt -> {wr with SourcePort = pt}
    //             | None -> wr
    //         let updateWT wr =
    //             let foundtgt (prt: Symbol.Port) = (wr.TargetPort.Id = prt.Id)
    //             match List.tryFind (foundtgt) portList with
    //             | Some pt -> {wr with TargetPort = pt}
    //             | None -> wr
    //         wir |> updateWS |> updateWT
    //     let updateNetList nLs = List.map newW nLs
    //     {model with WX = updateNetList model.WX} , Cmd.none

    | AddWire (startPort, endPort, createDU) ->  
        let newWireSym = addWire startPort endPort createDU model.Symbol
        match newWireSym with 
        | Some newWire, symModel -> {model with WX = newWire :: model.WX ; Symbol = symModel} , Cmd.none
        | None , symModel -> model,Cmd.none
    
    | DeselectWire  ->
        let newWls =
            model.WX |> List.map (fun w ->
                               {w with IsSelected = false}
                               ) 
        {model with WX = newWls} , Cmd.none
        
    | StartDraggingWire (wId , p) ->
        let newWls wLs = 
            wLs 
            |> List.map (fun w ->
                if w.Id <> wId then w
                else 
                   startWireDragging w p model)
        {model with WX = newWls model.WX} , Cmd.ofMsg( SelectWire wId)
    | DraggingWire (wid,pos) ->      
        let nwWls wLs = 
            wLs 
            |> List.map (fun wire ->
                if wire.Id <> wid then
                    wire
                else
                    let srcOrient, trgtOrient = getSrcAndTrgtOrientation model wire
                    dragAWire wire pos model srcOrient trgtOrient
                ) 
        {model with WX = nwWls model.WX} , Cmd.none
    | DeleteWire -> 
        let wireToBeRenderedFirst, wireToBeDeletedFirst =
            let wireRender,wireDelete =
                model.WX
                |>List.partition (fun w -> w.IsSelected = false)
            let wireRender2,wireDelete2 =  //when a symbol is deleted remove wire as well
                wireRender
                |>List.partition (fun w -> Symbol.symbolInModel (model.Symbol) (fst w.SourceIds)) 
            let wireRender3,wireDelete3 =  //when a symbol is deleted remove wire as well
                wireRender2                              
                |>List.partition (fun w -> Symbol.symbolInModel (model.Symbol) (fst w.TargetIds)) // when symbol is deleted, delete wire as well 
            let tmpRender = wireRender3 
            let tmpDelete = wireDelete @ wireDelete2 @ wireDelete3
            (tmpRender,tmpDelete)

        let allToBeDeletedPorts= //for all the wires that are gonna be deleted check if it has createError 
            wireToBeDeletedFirst
            |> List.collect (fun w -> [(fst (getPortsOfWire model w), snd (getPortsOfWire model w), w.HasError)])
            |> Set.ofList
            |> Set.toList
        let wireToBeRendered =
            wireToBeRenderedFirst
            |>Set.ofList
            |>Set.toList

        let newSymModel,ignoreMsg = 
            Symbol.update (Symbol.Msg.RemoveConnections allToBeDeletedPorts) model.Symbol

        {model with WX = wireToBeRendered; Symbol = newSymModel} , Cmd.none

    | DrawFromPortToCursor (startPos,endPos,endPort) -> 
        {model with PortToCursor = (startPos,endPos,endPort)}, Cmd.none
    | RemoveDrawnLine -> 
        {model with PortToCursor = ({X = 0.0; Y = 0.0}, {X = 0.0; Y=0.0}, None)}, Cmd.none //zack 
    | SelectWiresWithinRegion bbox ->
        {model with WX =  selectBoundedWires (model) bbox},Cmd.none
    | UpdateWires ->
        let newWireModel,newSymModel =
            ( (model.WX,model.Symbol), model.WX )
            ||>List.fold  (fun (wModel,sModel) wire ->
                let srcPort,endPort = getPortsOfWire model wire 
                let wBusWidth =    
                    match (srcPort.BusWidth, endPort.BusWidth) with
                    | Some width, None -> Some width
                    | _ ,Some width -> Some width
                    | None, None    -> None

                wModel
                |>List.map (fun w -> if (w.Id = wire.Id) then {w with BusWidth = wBusWidth} else w),
                sModel
                |>List.map (fun sym -> 
                    sym
                    |>Symbol.updateSymWithPort {srcPort with BusWidth = wBusWidth}
                    |>Symbol.updateSymWithPort {endPort with BusWidth = wBusWidth}
                    |>Symbol.autoCompleteWidths
                )
            )   
        
        {model with WX = newWireModel; Symbol = newSymModel},Cmd.none

    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol (Symbol.MouseMsg mMsg))


// Interface Functions
let wireToSelectOpt (wModel: Model) (pos: XYPos) : CommonTypes.ConnectionId option =
    let inShape (b:BoundingBox) =
        pos.X >= b.TopLeft.X && pos.X <= b.BottomRight.X && pos.Y >= b.TopLeft.Y && pos.Y <= b.BottomRight.Y 
    let inWireBounds (conId: CommonTypes.ConnectionId) =
        let cable = wire wModel conId 
        match List.tryFind (inShape) (bounds cable wModel) with
        |Some b -> true
        |None -> false
    let name cable = cable.Id
    List.tryFind (inWireBounds) (List.map name wModel.WX)


let getSelectedWireList (wireList : Wire list) : Wire list = 
    List.filter (fun w -> w.IsSelected) wireList


let deselectWire (WX : Wire list)  =
    let newWX =
        WX 
        |> List.map (fun w ->
           {w with IsSelected = false})        
    newWX

//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractWires (wModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"












    



