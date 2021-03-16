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
    SourcePort : Symbol.Port
    TargetPort : Symbol.Port
    isSelected : bool
    hasError : bool 
    relativPositions : XYPos list
    PrevPositions : XYPos list
    BeingDragged : int// create a list of boundingboxes
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
    | AddWire of Symbol.Port*Symbol.Port
    | SelectWire of CommonTypes.ConnectionId*XYPos
    | DeleteWire
    | UpdatedPortsToBusWire of sId : Symbol.Port list
    | DeselectWire
    | StartDraggingWire of CommonTypes.ConnectionId*XYPos
    | DraggingWire of CommonTypes.ConnectionId*XYPos
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
        if (inBounds wr.SourcePort.Pos && inBounds wr.TargetPort.Pos) then
            {wr with isSelected = true}
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
    let startpt = cable.SourcePort.Pos 
    let endpt = cable.TargetPort.Pos
    let midX = (startpt.X+endpt.X)/2.0
    let midY = (startpt.Y+endpt.Y)/2.0
    
    if (cable.SourcePort.PortType=CommonTypes.Input && cable.TargetPort.PortType=CommonTypes.Output) then
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

let vertexlist (cable: Wire) (wModel: Model) = 
    let [a ; b ; c] = cable.relativPositions
    let startpt = cable.SourcePort.Pos 
    let endpt = cable.TargetPort.Pos
    let midX = (startpt.X+endpt.X)/2.0
    let midY = (startpt.Y+endpt.Y)/2.0
    
    if (cable.SourcePort.PortType=CommonTypes.Input && cable.TargetPort.PortType=CommonTypes.Output) then
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

let bounds (cable: Wire) (wModel: Model)=
    let individualBound (start:XYPos) (final: XYPos) =
        if (final.Y > start.Y || final.X > start.X) then
            {TopLeft = {X=start.X-15.0;Y=start.Y-15.0} ;
             BottomRight = {X=final.X+15.0;Y=final.Y+15.0}}
         else {TopLeft = {X=final.X-15.0;Y=final.Y-15.0} ;
             BottomRight = {X=start.X+15.0;Y=start.Y+15.0}}
    let vlst = vertexlist cable wModel
    let startlst , endlst = pairListElements vlst
    List.map2 individualBound startlst endlst

let autosingleWireView (wModel: Model)= 
    let displayFullWire (cable: Wire) (props: WireRenderProps) (vertices: list<XYPos>) = 
        let displayWireSegment (start: XYPos) (final: XYPos) =
            let color =
                    if props.WireP.hasError then
                        "red"
                    else if props.WireP.isSelected then
                        "green"
                    else 
                        "black"
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
    
        reactlst |> (fun lineEl -> 
            g [] lineEl)

    FunctionComponent.Of(
        fun (props: WireRenderProps) ->                                         
            g   [ Style [ 
                TransformOrigin "0px 50px"
                Transform (sprintf "translate(%fpx,%fpx) rotate(%ddeg) scale(%f) " 0.0 0.0 0 1.0)
                ]
                ]
                [
                displayFullWire props.WireP props (vertexlist props.WireP wModel)
                ]
                )

let view (model:Model) (dispatch: Msg -> unit)= 
    let wires = 
        model.WX
        |> List.map (fun w ->
            let props = {
                key = w.Id
                WireP = w
                SrcP = w.SourcePort.Pos 
                TgtP = w.TargetPort.Pos
                ColorP = model.Color.Text()
                StrokeWidthP = "2px" }
            autosingleWireView model props)
    let symbols = Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg))
    let lineToCursor= drawLineToCursor model.PortToCursor
    
    g [] [(g [] wires) ; symbols; lineToCursor]


let init n () =
    let symbols, cmd = Symbol.init()   
    {WX=[];Symbol=symbols; Color=CommonTypes.Red ; Countselected = 0 ; PortToCursor = ({X = 0.0; Y= 0.0},{X = 0.0; Y= 0.0}, None)},Cmd.none 


let createWire (startPort: Symbol.Port) (endPort: Symbol.Port) (createDU : CreateDU) =
    {
        Id = CommonTypes.ConnectionId (uuid())
        SourcePort = startPort
        TargetPort = endPort
        isSelected = if (createDU = Duplicate || createDU = DuplicateError) then true else false
        hasError = if (createDU = Error || createDU = DuplicateError) then true else false
        relativPositions = [{X=0.0 ; Y=0.0} ; origin ; origin]
        PrevPositions = [origin ; origin ; origin]
        BeingDragged = -1
    }

let getWiresToBeDuplicated (displacementPos : XYPos ) (wireList : Wire list) (portList : Symbol.Port list) : (Symbol.Port option *Symbol.Port option* Wire) list  =    
    wireList
    |> List.map (fun wire ->  
    
                let sourcePort =  List.tryFind (fun (p : Symbol.Port)  ->  
                                                let dist = calcDistance (posAdd (wire.SourcePort.Pos)(displacementPos)) p.Pos
                                                (dist < 0.001) ) portList
                                        
                let endPort =  List.tryFind (fun (p : Symbol.Port)  ->  
                                             let dist = calcDistance (posAdd (wire.TargetPort.Pos)(displacementPos)) p.Pos
                                             (dist < 0.001) ) portList
                (sourcePort,endPort,wire)

                )
 
let startWireDragging (cable: Wire) (pos: XYPos) (wModel: Model)=
    let inShape (b:BoundingBox) =
        pos.X > b.TopLeft.X && pos.X < b.BottomRight.X && pos.Y > b.TopLeft.Y && pos.Y < b.BottomRight.Y
    let draggedSegment =
        match List.map inShape (bounds cable wModel) with
        | [false ; true ; false] -> 0
        | [false ; true ; false ; false ; false] -> 0
        | [false ; false ; true ; false ; false] -> 1
        | [false ; false ; false ; true ; false] -> 2
        | _ -> -1
    {cable with BeingDragged = draggedSegment}

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
    match cable.BeingDragged with
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
    | _ -> cable      

let endWireDragging (cable: Wire) =
    {cable with BeingDragged = -1}

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    match msg with
    | Symbol sMsg -> 
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd
    | SetColor c -> {model with Color = c}, Cmd.none
    | SelectWire (wId,p) ->         
        let clickedWire =
            List.map (fun w ->
                        if w.Id <> wId then w
                        else {w with isSelected = true}
                      )
        let newWls = clickedWire model.WX 
        {model with WX = newWls} ,Cmd.none
    | UpdatedPortsToBusWire plst ->
        let newW wir =
            let updateWS wr =
                let foundsrc (prt: Symbol.Port) = (wr.SourcePort.Id = prt.Id)
                match List.tryFind (foundsrc) plst with
                | Some pt -> {wr with SourcePort = pt}
                | None -> wr
            let updateWT wr =
                let foundtgt (prt: Symbol.Port) = (wr.TargetPort.Id = prt.Id)
                match List.tryFind (foundtgt) plst with
                | Some pt -> {wr with TargetPort = pt}
                | None -> wr
            wir |> updateWS |> updateWT
        let updateNetList nLs = List.map newW nLs
        {model with WX = updateNetList model.WX} , Cmd.none
    | AddWire (startPort , endPort) ->  
        let symModel = 
            model.Symbol
            |>List.map (fun sym -> Symbol.changePortStateIsConnected startPort sym Increment)
            |>List.map (fun sym -> Symbol.changePortStateIsConnected endPort sym Increment)
        let newWireSym  = 
            match (startPort.BusWidth, endPort.BusWidth ) with 
            | Some startWidth, Some endWidth  when (startWidth = endWidth) && startPort.PortType = CommonTypes.Input ->  //Buswidth match but inputPort has many drivers
                                               if (startPort.NumberOfConnections = 0) then 
                                                    let wire = createWire startPort endPort Init
                                                    
                                                    (wire,symModel) 
                                                else
                                                    let wire = createWire startPort endPort Error
                                                    printf ("Error : InputPort can only have One Driver. If you want to merge two wires you a MergeWire Component")
                                                    (wire,symModel)
                                                
            | Some startWidth, Some endWidth  when startWidth = endWidth && endPort.PortType = CommonTypes.Input ->  //Buswidth match but inputPort has many drivers
                                                let symModel = 
                                                    model.Symbol
                                                    |>List.map (fun sym -> Symbol.changePortStateIsConnected startPort sym Increment)
                                                    |>List.map (fun sym -> Symbol.changePortStateIsConnected endPort sym Increment)
                                                
                                                if (endPort.NumberOfConnections = 0) then 
                                                    let wire = createWire startPort endPort Init
                                                                                        
                                                    (wire,symModel) 
                                                else
                                                    let wire = createWire startPort endPort Error
                                                    printf ("Error : InputPort can only have One Driver.")
                                                    (wire,symModel)

            | Some startWidth, Some endWidth  when startWidth = endWidth ->  //Buswidth match but inputPort has many drivers
                                                    let wire = createWire startPort endPort Init
                                                   
                                                    (wire,symModel)
            | Some startWidth, Some endWidth  when startWidth <> endWidth ->  //Buswidth dont match 
                                                let wire = createWire startPort endPort Error
                                                let sym,symMsg = Symbol.update (Symbol.Msg.AddErrorToErrorList
                                                                  [startPort;endPort]
                                                                 )symModel

                                                (wire,sym)
            | Some startWidth, None  -> let wire = createWire startPort endPort Init     //change BusWidth None to BusWidth of newly connected port
                                        let sym,symMsg  = Symbol.update (Symbol.Msg.EnforceBusWidth 
                                                            (startWidth,endPort,EnforceEndPort)
                                                          )symModel


                                        (wire,sym)
            | None , Some endWidth  -> let wire = createWire startPort endPort Init     //change BusWidth None to BusWidth of newly connected port
                                       let sym,symMsg  = Symbol.update (Symbol.Msg.EnforceBusWidth 
                                                            (endWidth,startPort,EnforceStartPort)
                                                          )symModel
                                       (wire,sym)

            | _ -> failwithf " BusWidths of sourcePort and targetPort are not specified !!!!"



        {model with WX = (fst newWireSym) :: model.WX ; Symbol = (snd newWireSym)} , Cmd.none 
    
    | DeselectWire  ->
        let newWls =
            model.WX |> List.map (fun w ->
                               {w with isSelected = false}
                               ) 
        {model with WX = newWls} , Cmd.none
    | StartDraggingWire (wid , p) ->
        let newWls wLs = wLs |> List.map (fun w ->
                                if w.Id <> wid then w
                                else 
                                    startWireDragging w p model)
        {model with WX = newWls model.WX} , Cmd.ofMsg( SelectWire (wid, p))
    | DraggingWire (wid,p) -> 
        let nwWls wLs = wLs |> List.map (fun w ->
                                if w.Id <> wid then
                                    w
                                else  
                                    dragAWire w p
                              ) 
        {model with WX = nwWls model.WX} , Cmd.none
    | DeleteWire -> 
        let wireToBeRenderedFirst, wireToBeDeletedFirst =
            let wireRender,wireDelete =
                model.WX
                |>List.partition (fun w -> w.isSelected = false)
            let wireRender2,wireDelete2 = 
                wireRender
                |>List.partition (fun w -> (Symbol.getSymbolWithId (model.Symbol) (w.SourcePort.HostId)) <> None) 
            let wireRender3,wireDelete3 =
                wireRender2                              
                |>List.partition (fun w -> (Symbol.getSymbolWithId (model.Symbol) (w.TargetPort.HostId)) <> None) // when symbol is deleted, delete wire as well 
            let tmpRender = wireRender3 
            let tmpDelete = wireDelete @ wireDelete2 @ wireDelete3
            (tmpRender,tmpDelete)

        let allToBeDeletedPorts= //for all the wires that are gonna be deleted check if it has error 
            wireToBeDeletedFirst
            |> List.collect (fun w -> [w.SourcePort;w.TargetPort])
            |> Set.ofList
            |> Set.toList
        let wireToBeRendered =
            wireToBeRenderedFirst
            |>Set.ofList
            |>Set.toList

        let newSymModel,ignoreMsg = 
            Symbol.update (Symbol.Msg.RemoveErrorFromErrorList allToBeDeletedPorts) model.Symbol

        {model with WX = wireToBeRendered; Symbol = newSymModel} , Cmd.none


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
    let inWireBounds (conId: CommonTypes.ConnectionId) =
        let cable = wire wModel conId 
        match List.tryFind (inShape) (bounds cable wModel) with
        |Some b -> true
        |None -> false
    let name cable = cable.Id
    List.tryFind (inWireBounds) (List.map name wModel.WX)


let getSelectedWireList (wireList : Wire list) : Wire list = 
    List.filter (fun w -> w.isSelected) wireList

//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"

let extractWires (wModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"












    



