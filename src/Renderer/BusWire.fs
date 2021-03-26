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

type SegmentForDragging =
    | Second
    | Third
    | Forth
    | NoSeg

type Wire = {
    Id : CommonTypes.ConnectionId 
    SourcePort : Symbol.Port
    TargetPort : Symbol.Port
    IsSelected : bool
    HasError : bool 
    RelativePos : XYPos*XYPos*XYPos
    PrevPositions : XYPos*XYPos*XYPos
    BeingDragged : SegmentForDragging
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
    | UpdateBusWirePorts
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
let findWire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
    let correctWire wire =
        wire.Id = wId
    match wModel.WX |> List.tryFind (correctWire) with
    | Some wr -> wr
    | None    -> failwithf "Ghost"

type RenderWireProps = {
    key : CommonTypes.ConnectionId
    WireP: Wire
    ColorP: string
    StrokeWidthP: string 
    SrcOrient: PortOrientation
    TgtOrient: PortOrientation
    }

let getPortsOfWire (wModel :Model) (wire : Wire)  = 
    let sym1,sym2 =
        match  Symbol.getSymbolWithId wModel.Symbol wire.SourcePort.HostId, 
               Symbol.getSymbolWithId wModel.Symbol wire.TargetPort.HostId with 
        | Some sym1, Some sym2 -> sym1,sym2
        | _ ,_ -> failwithf "Impossible : You can't have wires floating around unconnected !"

    let srcPort = (Symbol.getPortWithId sym1 wire.SourcePort.Id)
    let tgtPort = (Symbol.getPortWithId sym2 wire.TargetPort.Id)
    srcPort, tgtPort

let selectBoundedWires (wModel: Model) (boundary: BoundingBox) =
    let selectWireinBounds (wr: Wire) =
        let inBounds (point: XYPos) =
            point.X > boundary.TopLeft.X && point.X < boundary.BottomRight.X && point.Y > boundary.TopLeft.Y && point.Y < boundary.BottomRight.Y
        if (inBounds wr.SourcePort.Pos && inBounds wr.TargetPort.Pos) then
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
    | _ when wire.HasError -> []
    | None | Some 1 -> [] 
    | Some w -> [str $"{w}"]


let pairListElements sequence  = 
    let revseq = List.rev sequence
    match sequence,revseq with 
    |hd::bodyone , tl::bodytwo -> List.rev bodytwo , bodyone
    |_ -> sequence,sequence

let vertexList (wire: Wire) (srcOrient: PortOrientation) (tgtOrient: PortOrientation) (initial : bool)= 
    let a,b,c = 
        match initial with
        | true -> nullPos,nullPos,nullPos
        | false -> wire.RelativePos
          
    let startPt = wire.SourcePort.Pos 
    let endPt = wire.TargetPort.Pos
    let midX = (startPt.X+endPt.X)/2.0
    let midY = (startPt.Y+endPt.Y)/2.0
    
    let leftOf pTwo pOne = pOne.X < pTwo.X
    let above pTwo pOne = pOne.Y < pTwo.Y
    let below pTwo pOne = pOne.Y > pTwo.Y
    let rightOf pTwo pOne = pOne.X > pTwo.X

    let moveRight x p = {p with X = p.X + x}
    let moveDown y p = {p with Y = p.Y + y}
    
    let moveHorizontallyTo x p = {p with X = x}
    let moveVerticallyTo y p = {p with Y = y}

    match srcOrient,tgtOrient with
    |Right,Bottom -> match (startPt |> leftOf endPt) , (startPt |> below endPt) with
                     |true,true -> [startPt ; startPt |> moveHorizontallyTo endPt.X ; endPt]
                     |true,false 
                     |false,true -> [startPt ; startPt |> moveRight(15.0+a.X) ; {X=startPt.X+15.0+a.X;Y=endPt.Y+15.0+b.Y} ; endPt |> moveDown(15.0+b.Y) ; endPt]
                     |false,false -> [startPt ; startPt |> moveRight(100.0+a.X) ; {X=startPt.X+100.0+a.X;Y=endPt.Y+15.0+b.Y} ; endPt |> moveDown(15.0+b.Y) ; endPt]
    |Right,Top -> match (startPt |> leftOf endPt) , (startPt |> above endPt) with
                  |true,true -> [startPt ; startPt |> moveHorizontallyTo endPt.X ; endPt]
                  |true,false 
                  |false,true -> [startPt ; startPt |> moveRight(15.0+a.X) ; {X=startPt.X+15.0+a.X;Y=endPt.Y-15.0+b.Y} ; endPt |> moveDown(-15.0+b.Y) ; endPt]
                  |false,false -> [startPt ; startPt |> moveRight(100.0+a.X) ; {X=startPt.X+100.0+a.X;Y=endPt.Y-15.0+b.Y} ; endPt |> moveDown(-15.0+b.Y) ; endPt]
    |Right,Left -> match (startPt |> leftOf endPt) with 
                   |true -> [startPt ; startPt |> moveHorizontallyTo (midX+a.X) ; endPt |> moveHorizontallyTo (midX+a.X) ; endPt]
                   |false -> [startPt ; startPt |> moveRight(15.0+a.X) ; {X=startPt.X+15.0+a.X;Y=midY+b.Y} ; {X=endPt.X-15.0+c.X;Y=midY+b.Y} ; endPt |> moveRight(-15.0+c.X) ; endPt]
    |Right,Right -> [startPt ; {X=startPt.X+15.0;Y=startPt.Y} ; {X=startPt.X+15.0;Y=endPt.Y} ; endPt]
    |Left,Bottom -> match (startPt |> rightOf endPt) , (startPt |> below endPt) with
                    |true,true -> [startPt ; startPt |> moveHorizontallyTo midX ; endPt]
                    |true,false 
                    |false,true -> [startPt ; startPt |> moveRight(-15.0+a.X) ; {X=startPt.X-15.0+a.X;Y=endPt.Y+15.0+b.Y} ; endPt |> moveDown(15.0+b.Y) ; endPt]
                    |false,false -> [startPt ; {X=startPt.X-100.0+a.X;Y=startPt.Y} ; {X=startPt.X-100.0+a.X;Y=endPt.Y+15.0+b.Y} ; {X=endPt.X;Y=endPt.Y+15.0+b.Y} ; endPt]
    |Left,Top -> match (startPt |> rightOf endPt) , (startPt |> above endPt) with
                 |true,true -> [startPt ; startPt |> moveHorizontallyTo endPt.X ; endPt]
                 |true,false 
                 |false,true -> [startPt ; startPt |> moveRight(-15.0+a.X) ; {X=startPt.X-15.0+a.X;Y=endPt.Y-15.0+b.Y} ; endPt |> moveDown(-15.0+b.Y) ; endPt]
                 |false,false -> [startPt ; startPt |> moveRight(-100.0+a.X) ; {X=startPt.X-100.0+a.X;Y=endPt.Y-15.0+b.Y} ; endPt |> moveDown(-15.0+b.Y) ; endPt]
    |Left,Right -> match (startPt |> rightOf endPt) with 
                   |true -> [startPt ; startPt |> moveHorizontallyTo (midX+a.X) ; {X=midX+a.X;Y=endPt.Y} ; endPt]
                   |false -> [startPt ; startPt |> moveRight (-15.0+a.X) ; {X=startPt.X-15.0+a.X;Y=midY+b.Y} ; {X=endPt.X+15.0+c.X;Y=midY+b.Y} ; {X=endPt.X+15.0+c.X;Y=endPt.Y} ; endPt]
    |Top,Bottom -> match (startPt |> below endPt) with
                   |true -> [startPt ; startPt |> moveVerticallyTo (midY+a.Y) ; endPt |> moveVerticallyTo (midY+a.Y) ; endPt]
                   |false -> [startPt ; startPt |> moveDown(-15.0+a.Y) ; {X=midX+b.X;Y=startPt.Y-15.0+a.Y} ; {X=midX+b.X;Y=endPt.Y+15.0+c.Y} ; endPt |> moveDown(15.0+c.Y) ; endPt]
    |Top,Left -> match (startPt |> leftOf endPt) , (startPt |> below endPt) with
                 |true,true -> [startPt ; startPt |> moveVerticallyTo endPt.Y ; endPt]
                 |true,false
                 |false,true -> [startPt ; startPt |> moveDown(-15.0+a.Y) ; {X=endPt.X-15.0+b.X;Y=startPt.Y-15.0+a.Y} ; endPt |> moveRight(-15.0+b.X) ; endPt]
                 |false,false -> [startPt ; startPt |> moveDown(-100.0+a.Y) ; {X=endPt.X-15.0+b.X;Y=startPt.Y-100.0+a.Y} ; endPt |> moveRight(-15.0+b.X) ; endPt]
    |Top,Right -> match (startPt |> rightOf endPt) , (startPt |> below endPt) with
                  |true,true -> [startPt ; startPt |> moveVerticallyTo endPt.Y ; endPt]
                  |true,false
                  |false,true -> [startPt ; startPt |> moveDown (-15.0+a.Y) ; {X=endPt.X+15.0+b.X;Y=startPt.Y-15.0+a.Y} ; endPt |> moveRight (15.0+b.X) ; endPt]
                  |false,false -> [startPt ; startPt |> moveDown (-100.0+a.Y) ; {X=endPt.X+15.0+b.X;Y=startPt.Y-100.0+a.Y} ; endPt |> moveRight (15.0+b.X) ; endPt]
    |Top,Top -> match (startPt |> above endPt) with
                |true -> [startPt ; {X=startPt.X;Y=startPt.Y-15.0+a.Y} ; {X=endPt.X;Y=startPt.Y-15.0+a.Y} ; endPt]
                |false -> [startPt ; {X=startPt.X;Y=endPt.Y-15.0+a.Y} ; {X=endPt.X;Y=endPt.Y-15.0+a.Y} ; endPt]
    |Bottom,Bottom -> match (startPt |> below endPt) with
                      |true -> [startPt ; startPt |> moveDown (15.0+a.Y) ; {X=endPt.X;Y=startPt.Y+15.0+a.Y} ; endPt]
                      |false -> [startPt ; {X=startPt.X;Y=endPt.Y+15.0+a.Y} ; endPt |> moveDown (15.0+a.Y) ; endPt]
    |Bottom,Top ->  match (startPt |> above endPt) with
                    |true -> [startPt ; {X=startPt.X;Y=midY+a.Y} ; {X=endPt.X;Y=midY+a.Y} ; endPt]
                    |false -> [startPt ; startPt |> moveDown (15.0+a.Y) ; {X=midX+b.X;Y=startPt.Y+15.0+a.Y} ; {X=midX+b.X;Y=endPt.Y-15.0+c.Y} ; endPt |> moveDown (-15.0+c.Y) ; endPt]
    |Bottom,Left -> match (startPt |> leftOf endPt) , (startPt |> above endPt) with
                    |true,true -> [startPt ; startPt |> moveVerticallyTo endPt.Y ; endPt]
                    |true,false
                    |false,true -> [startPt ; startPt |> moveDown(15.0+a.Y) ; {X=endPt.X-15.0+b.X;Y=startPt.Y+15.0+a.Y} ; endPt |> moveRight (-15.0+b.X) ; endPt]
                    |false,false -> [startPt ; {X=startPt.X;Y=startPt.Y+100.0+a.Y} ; {X=endPt.X-15.0+b.X;Y=startPt.Y+100.0+a.Y} ; {X=endPt.X-15.0+b.X;Y=endPt.Y} ; endPt]
    |Bottom,Right -> match (startPt |> rightOf endPt) , (startPt |> above endPt) with
                     |true,true -> [startPt ; startPt |> moveVerticallyTo endPt.Y ; endPt]
                     |true,false
                     |false,true -> [startPt ; startPt |> moveDown (15.0+a.Y) ; {X=endPt.X+15.0+b.X;Y=startPt.Y+15.0+a.Y} ; endPt |> moveRight (15.0+b.X) ; endPt]
                     |false,false -> [startPt ; startPt |> moveDown (100.0+a.Y) ; {X=endPt.X+15.0+b.X;Y=startPt.Y+100.0+a.Y} ; endPt |> moveRight (15.0+b.X) ; endPt]
    |_ ->  [startPt ; {X=midX;Y=startPt.Y} ; {X=midX;Y=endPt.Y} ; endPt]

let bounds (wire: Wire) (wModel: Model)=
    let individualBound (start:XYPos) (final: XYPos) =
        if (final.Y > start.Y || final.X > start.X) then
            {TopLeft = {X=start.X-5.0;Y=start.Y-5.0} ;
             BottomRight = {X=final.X+5.0;Y=final.Y+5.0}}
         else {TopLeft = {X=final.X-5.0;Y=final.Y-5.0} ;
             BottomRight = {X=start.X+5.0;Y=start.Y+5.0}}
    let vLst = vertexList wire (Symbol.getOrientationOfPort wModel.Symbol wire.SourcePort) (Symbol.getOrientationOfPort wModel.Symbol wire.TargetPort) false
    let startList , endList = pairListElements vLst 
    List.map2 individualBound startList endList

let displayWireSegment (props: RenderWireProps) (start: XYPos) (final: XYPos) =
    let color =
        match props.WireP.IsSelected, props.WireP.HasError, props.WireP.BusWidth with
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

let displayFullWire (props: RenderWireProps) (vertices: list<XYPos>) = 
    let srcPos = props.WireP.SourcePort.Pos
    let tgtPos = props.WireP.TargetPort.Pos

    let startList,endList = pairListElements vertices
    let reactlst = List.map2 (displayWireSegment props) startList endList 
    let annotatePos = if (props.WireP.SourcePort.PortType = CommonTypes.Output) then srcPos else tgtPos 

    text [
        X (annotatePos.X + 20.0)
        Y (annotatePos.Y + 2.0)
        Style [
            TextAnchor "middle"
            DominantBaseline "hanging" 
            FontSize "10px"
            FontWeight "Bold"
            Fill "Black" 
            UserSelect UserSelectOptions.None
            PointerEvents "none"
        ]
    ]  (busWidthAnnotation props.WireP) :: reactlst
        |> (fun reactElemLst -> 
        g [] reactElemLst)
  

let wireView  = 
    FunctionComponent.Of (
        fun (props: RenderWireProps) ->  

            g   []
                [
                    displayFullWire props (vertexList props.WireP props.SrcOrient props.TgtOrient false)
                ]
        , "Wire"
        , equalsButFunctions
    )

let view (wModel:Model) (dispatch: Msg -> unit)= 
    let wires = 
        wModel.WX
        |> List.map (fun wire ->
            let srcPort, tgtPort = wire.SourcePort, wire.TargetPort
            let props = {
                key = wire.Id
                WireP = {wire with SourcePort = srcPort ; TargetPort = tgtPort}
                ColorP = wModel.Color.Text()
                StrokeWidthP = 
                    match wire.HasError, wire.BusWidth with
                    | false, Some w when w = 1 -> "1px"
                    | _ -> "3px" 
                SrcOrient = Symbol.getOrientationOfPort wModel.Symbol wire.SourcePort
                TgtOrient = Symbol.getOrientationOfPort wModel.Symbol wire.TargetPort
            }
            wireView props
        )
    let symbols = SymbolRenderers.view wModel.Symbol (fun sMsg -> dispatch (Symbol sMsg))
    let lineToCursor= drawLineToCursor wModel.PortToCursor
    
    g [] [(g [] wires) ; symbols; lineToCursor]


let init () =
    let symbols, cmd = Symbol.init()   
    {WX=[];Symbol=symbols; Color=CommonTypes.Red  ; Countselected = 0 ; PortToCursor = ({X = 0.0; Y= 0.0},{X = 0.0; Y= 0.0}, None)},Cmd.none 



let createWire (startPort: Symbol.Port) (endPort: Symbol.Port)  =
    {
        Id = CommonTypes.ConnectionId (uuid())
        SourcePort = startPort
        TargetPort = endPort
        IsSelected = false 
        HasError = false 
        RelativePos = origin,origin,origin
        PrevPositions = origin,origin,origin
        BeingDragged = NoSeg
        BusWidth = 
            match startPort.BusWidth, endPort.BusWidth with
            | Some wStart, Some wEnd -> Some wStart
            | None, Some w | Some w, None -> Some w
            | None, None -> None
    }

let getWiresToBeDuplicated (displacementPos: XYPos ) (wireList: Wire list) (portList: Symbol.Port list) : (Symbol.Port option * Symbol.Port option* Wire) list  =    
    wireList
    |> List.map (fun wire ->  
        let sourcePort =  
            portList
            |> List.tryFind (fun port ->  
                (calcDistance (posAdd wire.SourcePort.Pos displacementPos) port.Pos) < 0.001)                   
        let endPort = 
            portList
            |> List.tryFind (fun port ->  
                (calcDistance (posAdd wire.TargetPort.Pos displacementPos) port.Pos) < 0.001) 
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
        | Some wire -> Some {wire with IsSelected = true}, newSym
        | None -> None, newSym
    else newWire, newSym


let startWireDragging (wire: Wire) (pos: XYPos) (wModel: Model)=
    let inShape (b:BoundingBox) =
        pos.X > b.TopLeft.X && pos.X < b.BottomRight.X && pos.Y > b.TopLeft.Y && pos.Y < b.BottomRight.Y
    let draggedSegment =
        match List.map inShape (bounds wire wModel) with
        | [false ; true ; false] -> Second
        | [false ; true ; false ; false] -> Second
        | [false ; false ; true ; false] -> Third
        | [false ; true ; false ; false ; false] -> Second
        | [false ; false ; true ; false ; false] -> Third
        | [false ; false ; false ; true ; false] -> Forth
        | _ -> NoSeg
    {wire with BeingDragged = draggedSegment}

let dragAWire (wire: Wire) (pos: XYPos) (wModel: Model) (srcOrient: PortOrientation) (tgtOrient: PortOrientation) =
    let vLs = 
        match vertexList wire srcOrient tgtOrient true with
        | [g ; h; i ; j ; k ; u] -> h, i, j
        | [s ; t ; u ; v ; w] -> t, u, v
        | [l ; m ; n ; v] -> m, n, v
        | _ -> origin, origin, origin
    let o, p, q = vLs
    let a, b, c = wire.RelativePos    
    let d, e, f = wire.PrevPositions 
 
    match wire.BeingDragged with
    | Second ->  
        let offset = o
        let correctedPos = posDiff pos offset 
        let diff = posDiff correctedPos d           
        { wire  with
            RelativePos = (posAdd a diff), b, c
            PrevPositions = correctedPos, e, f
        }
    | Third ->  
        let offset = p
        let correctedPos = posDiff pos offset
        let diff = posDiff correctedPos e 
        { wire with 
            RelativePos = a, (posAdd diff b), c
            PrevPositions = d, correctedPos, f
        }
    | Forth ->  
        let offset = q
        let correctedPos = posDiff pos offset
        let diff = posDiff correctedPos f
        { wire with 
            RelativePos = a, b, (posAdd diff) c
            PrevPositions = d , e , correctedPos
        }   
    | _ -> wire      

let endWireDragging (wire: Wire) =
    {wire with BeingDragged = NoSeg}

let update (msg : Msg) (wModel : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg wModel.Symbol
        {wModel with Symbol=sm}, Cmd.map Symbol sCmd
    | SetColor c -> {wModel with Color = c}, Cmd.none
    | SelectWire wId->         
        let clickedWire =
            List.map (fun wire ->
                        if wire.Id <> wId then wire
                        else {wire with IsSelected = true}
                      )
        let newWls = clickedWire wModel.WX 
        {wModel with WX = newWls} ,Cmd.none
    | UpdateBusWirePorts ->
        let portList = Symbol.getAllPorts (wModel.Symbol)
        let newW wir =
            let updateWS wr =
                let foundsrc (prt: Symbol.Port) = (wr.SourcePort.Id = prt.Id)
                match List.tryFind (foundsrc) portList with
                | Some pt -> {wr with SourcePort = pt}
                | None -> wr
            let updateWT wr =
                let foundtgt (prt: Symbol.Port) = (wr.TargetPort.Id = prt.Id)
                match List.tryFind (foundtgt) portList with
                | Some pt -> {wr with TargetPort = pt}
                | None -> wr
            wir |> updateWS |> updateWT
        let updateNetList nLs = List.map newW nLs
        {wModel with WX = updateNetList wModel.WX} , Cmd.none

    | AddWire (startPort, endPort, createDU) ->  
        let newWireSym = addWire startPort endPort createDU wModel.Symbol
        match newWireSym with 
        | Some newWire, symModel -> {wModel with WX = newWire :: wModel.WX ; Symbol = symModel} , Cmd.none
        | None , symModel -> wModel,Cmd.none
    
    | DeselectWire  ->
        let newWls =
            wModel.WX |> List.map (fun wire ->
                               {wire with IsSelected = false}
                               ) 
        {wModel with WX = newWls} , Cmd.none
        
    | StartDraggingWire (wId, pos) ->
        let newWls wLs = 
            wLs 
            |> List.map (fun wire ->
                if wire.Id <> wId then 
                    wire
                else 
                    printfn $"wireeeeeeeee: \n {wire}"
                    startWireDragging wire pos wModel)
        {wModel with WX = newWls wModel.WX} , Cmd.ofMsg( SelectWire wId)
    | DraggingWire (wid,p) ->      
        let nwWls wLs = 
            wLs 
            |> List.map (fun wire ->
                if wire.Id <> wid then
                    wire
                else
                    let srcO = Symbol.getOrientationOfPort wModel.Symbol wire.SourcePort
                    let tgtO = Symbol.getOrientationOfPort wModel.Symbol wire.TargetPort  
                    dragAWire wire p wModel srcO tgtO
                ) 
        {wModel with WX = nwWls wModel.WX} , Cmd.none
    | DeleteWire -> 
        let wireToBeRenderedFirst, wireToBeDeletedFirst =
            let wireRender, wireDelete =
                wModel.WX
                |>List.partition (fun wire -> wire.IsSelected = false)
            let wireRender2, wireDelete2 =  //when a symbol is deleted remove wire as well
                wireRender
                |>List.partition (fun wire -> (Symbol.getSymbolWithId (wModel.Symbol) (wire.SourcePort.HostId)) <> None) 
            let wireRender3, wireDelete3 =  //when a symbol is deleted remove wire as well
                wireRender2                              
                |>List.partition (fun wire -> (Symbol.getSymbolWithId (wModel.Symbol) (wire.TargetPort.HostId)) <> None) // when symbol is deleted, delete wire as well 
            let tmpRender = wireRender3 
            let tmpDelete = wireDelete @ wireDelete2 @ wireDelete3
            (tmpRender,tmpDelete)

        let allToBeDeletedPorts= //for all the wires that are gonna be deleted check if it has createError 
            wireToBeDeletedFirst
            |> List.collect (fun wire -> [(wire.SourcePort, wire.TargetPort, wire.HasError)])
            |> Set.ofList
            |> Set.toList
        let wireToBeRendered =
            wireToBeRenderedFirst
            |>Set.ofList
            |>Set.toList

        let newSymModel,ignoreMsg = 
            Symbol.update (Symbol.Msg.RemoveConnections allToBeDeletedPorts) wModel.Symbol

        {wModel with WX = wireToBeRendered; Symbol = newSymModel} , Cmd.none

    | DrawFromPortToCursor (startPos,endPos,endPort) -> 
        {wModel with PortToCursor = (startPos,endPos,endPort)}, Cmd.none
    | RemoveDrawnLine -> 
        {wModel with PortToCursor = ({X = 0.0; Y = 0.0}, {X = 0.0; Y=0.0}, None)}, Cmd.none //zack 
    | SelectWiresWithinRegion bbox ->
        {wModel with WX =  selectBoundedWires (wModel) bbox},Cmd.none
    | UpdateWires ->
        let newWireModel, newSymModel =
            ( (wModel.WX, wModel.Symbol), wModel.WX )
            ||> List.fold (fun (wModel,sModel) wire ->
                let srcPort, endPort = wire.SourcePort, wire.TargetPort 
                let wBusWidth =    
                    match srcPort.BusWidth, endPort.BusWidth with
                    | Some width, None -> Some width
                    | _ ,Some width -> Some width
                    | None, None    -> None

                wModel
                |> List.map (fun wire -> if (wire.Id = wire.Id) then {wire with BusWidth = wBusWidth} else wire),
                sModel
                |> List.map (fun sym -> 
                    sym
                    |>Symbol.updateSymWithPort {srcPort with BusWidth = wBusWidth}
                    |>Symbol.updateSymWithPort {endPort with BusWidth = wBusWidth}
                    |>Symbol.autoCompleteWidths
                )
            )   
        {wModel with WX = newWireModel; Symbol = newSymModel}, Cmd.none

    | MouseMsg mMsg -> wModel, Cmd.ofMsg (Symbol (Symbol.MouseMsg mMsg))


// Interface Functions
let wireToSelectOpt (wModel: Model) (pos: XYPos) : CommonTypes.ConnectionId option =
    let inShape (b:BoundingBox) =
        pos.X >= b.TopLeft.X && pos.X <= b.BottomRight.X && pos.Y >= b.TopLeft.Y && pos.Y <= b.BottomRight.Y 
    let inWireBounds (conId: CommonTypes.ConnectionId) =
        let wire = findWire wModel conId 
        match List.tryFind (inShape) (bounds wire wModel) with
        |Some b -> true
        |None -> false
    let name wire = wire.Id
    List.tryFind (inWireBounds) (List.map name wModel.WX)


let getSelectedWireList (wireList : Wire list) : Wire list = 
    List.filter (fun wire -> wire.IsSelected) wireList


let deselectWire (WX : Wire list)  =
    let newWX =
        WX 
        |> List.map (fun wire ->
           {wire with IsSelected = false})        
    newWX

//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractWires (wModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"












    



