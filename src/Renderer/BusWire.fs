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
    Id: CommonTypes.ConnectionId 
    SrcSymbol: Symbol.Port
    TargetSymbol: Symbol.Port
    Selected: bool
    BusWidth: int
    relativPositions: XYPos list
    PrevPositions: XYPos list// create a list of boundingboxes
    }

type Model = {
    Symbol: Symbol.Model
    WX: Wire list
    Color: CommonTypes.HighLightColor
    Countselected: int // Capitalize start of words
    Portlst: ConnectPoint list
    PortToCursor : XYPos * XYPos * Symbol.Port option
    }

//----------------------------Message Type-----------------------------------//
type Msg =
    | Symbol of Symbol.Msg
    | SetColor of CommonTypes.HighLightColor
    | MouseMsg of MouseT
    | DrawFromPortToCursor of XYPos*XYPos* Symbol.Port option
    | AddWire of Symbol.Port*Symbol.Port
    | SelectWire of CommonTypes.ConnectionId*XYPos
    | DeleteWire
    | UpdatedPortsToBusWire of sId : Symbol.Port list
    | DeselectWire
    | DraggingWire of CommonTypes.ConnectionId*XYPos
    | DuplicateWire of XYPos *Symbol.Port list
    | RemoveDrawnLine
    | SelectWiresWithinRegion of Helpers.BoundingBox

let origin = {X=0.0 ; Y=0.0}

let posOf x y = {X=x;Y=y}

/// look up wire in WireModel
let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
    let correctwire cable =
        cable.Id = wId
    match wModel.WX |> List.tryFind (correctwire) with
    | Some wr -> wr
    | None    -> failwithf "Ghost"

type WireRenderProps = {
    key : CommonTypes.ConnectionId
    WireP: Wire
    SrcP: XYPos 
    TgtP: XYPos
    ColorP: string
    StrokeWidthP: string 
    }


let selectBoundedWires (wModel: Model) (boundary: BoundingBox) =
    let selectWireinBounds (wr: Wire) =
        let inBounds (point: XYPos) =
            point.X > boundary.TopLeft.X && point.X < boundary.BottomRight.X && point.Y > boundary.TopLeft.Y && point.Y < boundary.BottomRight.Y
        if (inBounds wr.SrcSymbol.Pos && inBounds wr.TargetSymbol.Pos) then
            {wr with Selected = true}
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


let pairListElements sequence = 
        let revseq = List.rev sequence
        match sequence,revseq with 
        |hd::bodyone , tl::bodytwo -> List.rev bodytwo , bodyone
        |_ -> sequence,sequence

let vertexlstZero (cable: Wire) =
    let startpt = cable.SrcSymbol.Pos 
    let endpt = cable.TargetSymbol.Pos
    let midX = (startpt.X+endpt.X)/2.0
    let midY = (startpt.Y+endpt.Y)/2.0
    if (cable.SrcSymbol.PortType=CommonTypes.Input && cable.TargetSymbol.PortType=CommonTypes.Input) then
        if (startpt.X < endpt.X) then
            [startpt ; {X=startpt.X-20.0;Y=startpt.Y} ; {X=startpt.X-20.0;Y=endpt.Y} ; endpt]
        else 
            [startpt ; {X=endpt.X-20.0;Y=startpt.Y} ; {X=endpt.X-20.0;Y=endpt.Y} ; endpt]

    else if (cable.SrcSymbol.PortType=CommonTypes.Output && cable.TargetSymbol.PortType=CommonTypes.Output) then
        if (startpt.X > endpt.X) then
            [startpt ; {X=startpt.X+20.0;Y=startpt.Y} ; {X=startpt.X+20.0;Y=endpt.Y} ; endpt]
        else 
            [startpt ; {X=endpt.X+20.0;Y=startpt.Y} ; {X=endpt.X+20.0;Y=endpt.Y} ; endpt]

    else if (cable.SrcSymbol.PortType=CommonTypes.Input && cable.TargetSymbol.PortType=CommonTypes.Output) then
        if (startpt.X > endpt.X) then
            [startpt ; {X=midX;Y=startpt.Y} ; {X=midX;Y=endpt.Y} ; endpt]
        else 
            [startpt ; {X=startpt.X-20.0;Y=startpt.Y} ; {X=startpt.X-20.0;Y=midY} ; {X=endpt.X+20.0;Y=midY} ; 
            {X=endpt.X+20.0;Y=endpt.Y} ; endpt]

    else 
        if (startpt.X < endpt.X) then 
            [startpt ; {X=midX;Y=startpt.Y} ; {X=midX;Y=endpt.Y} ; endpt]
        else if (startpt.Y-endpt.Y < -100.0 || startpt.Y-endpt.Y > 100.0) then
            [startpt ; {X=startpt.X+25.0;Y=startpt.Y} ; {X=startpt.X+25.0;Y=midY} ; {X=endpt.X-25.0;Y=midY} ; 
            {X=endpt.X-25.0;Y=endpt.Y} ; endpt]
        else 
            [startpt ; {X=startpt.X+20.0;Y=startpt.Y} ; {X=startpt.X+20.0;Y=startpt.Y+100.0} ; {X=endpt.X-20.0;Y=startpt.Y+100.0} ; 
            {X=endpt.X-20.0;Y=endpt.Y} ; endpt]

let vertexlist (cable: Wire) = 
    let [a ; b ; c] = cable.relativPositions
    let startpt = cable.SrcSymbol.Pos 
    let endpt = cable.TargetSymbol.Pos
    let midX = (startpt.X+endpt.X)/2.0
    let midY = (startpt.Y+endpt.Y)/2.0
    if (cable.SrcSymbol.PortType=CommonTypes.Input && cable.TargetSymbol.PortType=CommonTypes.Input) then
        if (startpt.X < endpt.X) then
            [startpt ; {X=startpt.X-20.0+a.X;Y=startpt.Y} ; {X=startpt.X-20.0+a.X;Y=endpt.Y} ; endpt]
        else 
            [startpt ; {X=endpt.X-20.0+a.X;Y=startpt.Y} ; {X=endpt.X-20.0+a.X;Y=endpt.Y} ; endpt]

    else if (cable.SrcSymbol.PortType=CommonTypes.Output && cable.TargetSymbol.PortType=CommonTypes.Output) then
        if (startpt.X > endpt.X) then
            [startpt ; {X=startpt.X+20.0+a.X;Y=startpt.Y} ; {X=startpt.X+20.0+a.X;Y=endpt.Y} ; endpt]
        else 
            [startpt ; {X=endpt.X+20.0+a.X;Y=startpt.Y} ; {X=endpt.X+20.0+a.X;Y=endpt.Y} ; endpt]

    else if (cable.SrcSymbol.PortType=CommonTypes.Input && cable.TargetSymbol.PortType=CommonTypes.Output) then
        if (startpt.X > endpt.X) then
            [startpt ; {X=midX+a.X;Y=startpt.Y} ; {X=midX+a.X;Y=endpt.Y} ; endpt]
        else 
            [startpt ; {X=startpt.X-20.0+a.X;Y=startpt.Y} ; {X=startpt.X-20.0+a.X;Y=midY+b.Y} ; {X=endpt.X+20.0+c.X;Y=midY+b.Y} ; 
            {X=endpt.X+20.0+c.X;Y=endpt.Y} ; endpt]

    else 
        if (startpt.X < endpt.X) then 
            [startpt ; {X=midX+a.X;Y=startpt.Y} ; {X=midX+a.X;Y=endpt.Y} ; endpt]
        else if (startpt.Y-endpt.Y < -100.0 || startpt.Y-endpt.Y > 100.0) then
            [startpt ; {X=startpt.X+25.0+a.X;Y=startpt.Y} ; {X=startpt.X+25.0+a.X;Y=midY+b.Y} ; {X=endpt.X-25.0+c.X;Y=midY+b.Y} ; 
            {X=endpt.X-25.0+c.X;Y=endpt.Y} ; endpt]
        else 
            [startpt ; {X=startpt.X+20.0+a.X;Y=startpt.Y} ; {X=startpt.X+20.0+a.X;Y=startpt.Y+100.0+b.Y} ; {X=endpt.X-20.0+c.X;Y=startpt.Y+100.0+b.Y} ; 
            {X=endpt.X-20.0+c.X;Y=endpt.Y} ; endpt]

let individualBound (start:XYPos) (final: XYPos) =
        if (final.Y > start.Y || final.X > start.X) then
            {TopLeft = {X=start.X-15.0;Y=start.Y-15.0} ;
             BottomRight = {X=final.X+15.0;Y=final.Y+15.0}}
         else {TopLeft = {X=final.X-15.0;Y=final.Y-15.0} ;
             BottomRight = {X=start.X+15.0;Y=start.Y+15.0}}

let bounds (cable: Wire) =
    let vlst = vertexlist cable
    let startlst , endlst = pairListElements vlst
    List.map2 individualBound startlst endlst

let displayFullWire (cable: Wire) (props: WireRenderProps) (vertices: list<XYPos>) = 
    let displayWireSegment (start: XYPos) (final: XYPos) =
        let boundary = individualBound start final
        let topLeft = boundary.TopLeft
        let bottomRight = boundary.BottomRight
        let topRight = {X=bottomRight.X;Y=topLeft.Y}
        let bottomLeft = {X=topLeft.X;Y=bottomRight.Y}
        let color =
                    if props.WireP.Selected then
                        "green"
                    else
                        props.ColorP
        g   [ Style [ 
            // the transform here does rotation, scaling, and translation
            // the rotation and scaling happens with TransformOrigin as fixed point first
                TransformOrigin "0px 50px" // so that rotation is around centre of line
                Transform (sprintf "translate(%fpx,%fpx) rotate(%ddeg) scale(%f)" 0.0 0.0 0 1.0)
                ]
            ]
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

    let startlst,endlst = pairListElements vertices
    let reactlst = List.map2 displayWireSegment startlst endlst 
    
    reactlst
    |> (fun lineEl -> 
        g [] lineEl)

let createpoint (comp: Symbol.Symbol) =
    {Centre = comp.Pos ; Id = comp.Id ; Select = false}

let autosingleWireView = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->                                         
            g   [ Style [ 
                TransformOrigin "0px 50px"
                Transform (sprintf "translate(%fpx,%fpx) rotate(%ddeg) scale(%f) " 0.0 0.0 0 1.0)
                ]
                ]
                [
                displayFullWire props.WireP props (vertexlist props.WireP)
                ]
                )

let view (model:Model) (dispatch: Msg -> unit)= 
    
   // let conpoints = connectionlist model.Symbol
    let wires = 
        model.WX
        |> List.map (fun w ->
            let props = {
                key = w.Id
                WireP = w
                SrcP = w.SrcSymbol.Pos 
                TgtP = w.TargetSymbol.Pos
                ColorP = model.Color.Text()
                StrokeWidthP = "2px" }
            autosingleWireView props)
    let symbols = Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg))
    let lineToCursor= drawLineToCursor model.PortToCursor
    
    g [] [(g [] wires) ; symbols; lineToCursor]

/// dummy init for testing: real init would probably start with no wires.
/// this initialisation is not realistic - ports are not used
/// this initialisation depends on details of Symbol.Model type.
let init n () =
    let symbols, cmd = Symbol.init()
    let pointlst = List.map createpoint symbols    
    {WX=[];Symbol=symbols; Color=CommonTypes.Red ; Countselected = 0 ; Portlst = pointlst; PortToCursor = ({X = 0.0; Y= 0.0},{X = 0.0; Y= 0.0}, None)},Cmd.none 

let findSeg (cable: Wire) (pos: XYPos) =
    let inShape (b:BoundingBox) =
        pos.X > b.TopLeft.X && pos.X < b.BottomRight.X && pos.Y > b.TopLeft.Y && pos.Y < b.BottomRight.Y
    match List.map inShape (bounds cable) with
    | [false ; true ; false] -> 0
    | [false ; true ; false ; false ; false] -> 0
    | [false ; false ; true ; false ; false] -> 1
    | [false ; false ; false ; true ; false] -> 2
    | _ -> -1

let createWire (startPort: Symbol.Port) (endPort: Symbol.Port) (createDU : CreateDU) =
    {
        Id = CommonTypes.ConnectionId (uuid())
        SrcSymbol = startPort
        TargetSymbol = endPort
        Selected = if (createDU = Duplicate) then true else false
        BusWidth = 1 ;
        relativPositions = [{X=0.0 ; Y=0.0} ; origin ; origin]
        PrevPositions = [origin ; origin ; origin]
    }

let duplicateWire (displacementPos : XYPos ) (wireList : Wire list) (portList : Symbol.Port list) : (Symbol.Port option *Symbol.Port option) list  =
    
    wireList
    |> List.map (fun wire ->  

                let sourcePort =  List.tryFind (fun (p : Symbol.Port)  ->  
                                                let dist = calcDistance (posAdd (wire.SrcSymbol.Pos)(displacementPos)) p.Pos
                                                (dist < 0.001) ) portList
                                        
                let endPort =  List.tryFind (fun (p : Symbol.Port)  ->  
                                             let dist = calcDistance (posAdd (wire.TargetSymbol.Pos)(displacementPos)) p.Pos
                                             (dist < 0.001) ) portList
                (sourcePort,endPort)

                )

    

let dragAWire (cable: Wire) (pos: XYPos) =
    let vLs = match vertexlstZero cable with
              | [g ; h; i ; j ; k ; u] ->  h , i , j
              | [l ; m ; n ; v] -> m , l , n
              | _ -> origin , origin , origin
    let o , p , q = vLs
    let (a,b,c) = match cable.relativPositions with
                  | [g ; h ; i] -> g , h , i
                  | _ -> origin , origin , origin 
    let (d,e,f) = match cable.PrevPositions with
                  | [g ; h ; i] -> g , h , i
                  | _ -> origin , origin , origin 
    match findSeg cable pos with
    | 0 ->  let offset = o
            let correctedPos = posDiff pos offset 
            let diff = posDiff correctedPos d           
            { cable  with
                relativPositions = [(posAdd a diff) ; b ; c]
                PrevPositions = [correctedPos ; e ; f]
            }
    | 1 ->  let offset = p
            let correctedPos = posDiff pos offset
            let diff = posDiff correctedPos e 
            { cable with 
                relativPositions = [a ; (posAdd diff b) ; c]
                PrevPositions = [d ; correctedPos ; f]
            }
    | 2 ->  let offset = q
            let correctedPos = posDiff pos offset
            let diff = posDiff correctedPos f
            { cable with 
                relativPositions = [a ; b ; (posAdd diff) c]
                PrevPositions = [d ; e ; correctedPos]
            }   
    |_ -> cable        

let deleteWire (model: Model) (wName: CommonTypes.ConnectionId) =
    let found conn =
        conn.Id <> wName
    List.filter (found) model.WX

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg model.Symbol
        let followCircle (sm: Symbol.Symbol) cpt =
            {cpt with Centre = sm.Pos}
        let nwpts = List.map2 followCircle model.Symbol model.Portlst
        {model with Symbol=sm ; Portlst = nwpts}, Cmd.map Symbol sCmd
    | SetColor c -> {model with Color = c}, Cmd.none
    | SelectWire (wId,p) ->         
        let clickedWire =
            List.map (fun w ->
                        if w.Id <> wId then w
                        else {w with Selected = true}
                      )
        let newWls = clickedWire model.WX 
        {model with WX = newWls} , Cmd.none
    | UpdatedPortsToBusWire plst ->
        let newW wir =
            let updateWS wr =
                let foundsrc (prt: Symbol.Port) = (wr.SrcSymbol.Id = prt.Id)
                match List.tryFind (foundsrc) plst with
                | Some pt -> {wr with SrcSymbol = pt}
                | None -> wr
            let updateWT wr =
                let foundtgt (prt: Symbol.Port) = (wr.TargetSymbol.Id = prt.Id)
                match List.tryFind (foundtgt) plst with
                | Some pt -> {wr with TargetSymbol = pt}
                | None -> wr
            wir |> updateWS |> updateWT
        let updateNetList nLs = List.map newW nLs
        {model with WX = updateNetList model.WX} , Cmd.none
    | AddWire (s , f) -> 
        let nwW = createWire s f Init
        let addThisWire wLs= nwW::wLs
        {model with WX = addThisWire model.WX} , Cmd.none 
    | DeselectWire  ->
        let newWls =
            model.WX |> List.map (fun w ->
                               {w with Selected = false}
                               ) 
        {model with WX = newWls} , Cmd.none
    | DraggingWire (wid,p) -> 
        let nwWls wLs = wLs |> List.map (fun w ->
                                if w.Id <> wid then
                                    w
                                else  
                                    dragAWire w p
                              ) 
        {model with WX = nwWls model.WX} , Cmd.none
    | DeleteWire -> 
        let notHighlighted w = not w.Selected
        let newWireList =
            List.filter (notHighlighted) model.WX
        {model with WX = newWireList} , Cmd.none
    | DuplicateWire (displacementPos,portList) ->
        let selectedWireList =
            List.filter (fun w -> w.Selected) model.WX
        let dupWireList = duplicateWire displacementPos selectedWireList portList 
        
        let createDupWireList = 
            dupWireList
            |> List.map (fun w -> 
                match w  with
                | Some sourcePort , Some targetPort -> createWire sourcePort targetPort Duplicate
                | _ , _ -> failwithf "Shouldn't happen")
        
        let newWireList  = model.WX @ createDupWireList
        let newModel = {model with WX = newWireList}
        newModel,Cmd.none
    | DrawFromPortToCursor (startPos,endPos,endPort) -> 
        {model with PortToCursor = (startPos,endPos,endPort)}, Cmd.none
    | RemoveDrawnLine -> 
        {model with PortToCursor = ({X = 0.0; Y = 0.0}, {X = 0.0; Y=0.0}, None)}, Cmd.none //zack 
    | SelectWiresWithinRegion bbox ->
        {model with WX =  selectBoundedWires (model) bbox},Cmd.none

    | MouseMsg mMsg -> model, Cmd.ofMsg (Symbol (Symbol.MouseMsg mMsg))

//---------------Other interface functions--------------------//

/// Given a point on the canvas, returns the wire ID of a wire within a few pixels
/// or None if no such. Where there are two close wires the nearest is taken. Use
/// to determine which winire (if any) to select on a mouse click
let wireToSelectOpt (wModel: Model) (pos: XYPos) : CommonTypes.ConnectionId option =
    let inShape (b:BoundingBox) =
        pos.X >= b.TopLeft.X && pos.X <= b.BottomRight.X && pos.Y >= b.TopLeft.Y && pos.Y <= b.BottomRight.Y 
    let blst (cable: Wire) = bounds cable
    let inWireBounds (conId: CommonTypes.ConnectionId) =
        let cable = wire wModel conId 
        match List.tryFind (inShape) (blst cable) with
        |Some b -> true
        |None -> false
    let name cable = cable.Id
    List.tryFind (inWireBounds) (List.map name wModel.WX)


//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractWires (wModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"



    



