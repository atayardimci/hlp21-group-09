module Helpers
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open Electron
open Fable.React

//-------------------------------------------------------------------------//
//------------------------------Types--------------------------------------//
//-------------------------------------------------------------------------//

/// position on SVG canvas
type XYPos = 
    {
        X : float
        Y : float
    }

type MouseOp = 
    /// button up
    | Up
    /// button down
    | Down
    /// Move with button up
    | Move 
    /// Move with button Down
    | Drag
    | CtrlScroll
    | Scroll

type CanvasProps = 
    {
        height: int
        width : int 
        zoom  : float
    }

type MouseT = 
    {
        Pos: XYPos
        Op: MouseOp
    }

type BoundingBox = 
    {
        TopLeft : XYPos
        BottomRight : XYPos
    }

type Line = 
    {
        P1 : XYPos 
        P2 : XYPos
    }

type PortOrientation = 
    | Top
    | Bottom
    | Left
    | Right

type PortDU =
    | In
    | Out
    | All

type CreateDU =
    | Init      //initialized
    | Duplicate // duplicated
    | DuplicateError
    | Error

type BusWidthDU = 
    | EnforceStartPort
    | EnforceEndPort

type ChangeDU = 
    | Increment
    | Decrement

let posScaled (pos:XYPos) (zoom:float) = {X = pos.X/zoom; Y= pos.Y/zoom} 

let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

let nullPos = {X = 0.0; Y =0.0 }
let nullBBox = {TopLeft = nullPos; BottomRight = nullPos}   


//-------------------------------------------------------------------------//
//------------------------------By Sheet-----------------------------------//
//-------------------------------------------------------------------------//





//--------------------------------------------------------------------------//
//-----------------------------Helpers--------------------------------------//
//--------------------------------------------------------------------------//
let calcBBoxWithRadius (r:float) (pos: XYPos) = 
    {
      TopLeft=  {X = pos.X - r ; Y = pos.Y - r}
      BottomRight = {X = pos.X + r ; Y = pos.Y + r }
    }

let calculateBoundingBox (h:float) (w:float) (pos:XYPos) = 
    {
        TopLeft = {X=pos.X ; Y=pos.Y - 15.}
        BottomRight = {X=pos.X + w ; Y=pos.Y + h}
    }

let calcDistance (posOne : XYPos) (posTwo : XYPos) : float = 
    let distance = ( (posTwo.Y - posOne.Y)**2.0 + (posTwo.X - posOne.X)**2.0 ) ** (1.0/2.0)
    distance

let createBBoxFromPos (firstPos : XYPos) (sndPos : XYPos) : BoundingBox = 
    let topLeftX,bottomRightX = if (firstPos.X < sndPos.X) then firstPos.X, sndPos.X else sndPos.X, firstPos.X
    let topLeftY,bottomRightY = if (firstPos.Y < sndPos.Y) then firstPos.Y, sndPos.Y else sndPos.Y, firstPos.Y

    {TopLeft = {X = topLeftX ; Y = topLeftY }; BottomRight = {X = bottomRightX ; Y = bottomRightY}}

let calcCentreBBox (bBox : BoundingBox) : XYPos =  
    let center = 
        {X = (bBox.TopLeft.X +  bBox.BottomRight.X) /2.0 ; Y = (bBox.TopLeft.Y + bBox.BottomRight.Y) / 2.0}
    center
let calcCentre (P1 : XYPos) (P2 : XYPos) =
    let center = 
        {X = (P1.X +  P2.X) /2.0 ; Y = (P1.Y + P2.Y) / 2.0}
    center

//--------------------------------------------------------------------------//
//----------------------Port Position Calculations--------------------------//
//--------------------------------------------------------------------------//

let getGateInputPositionsBottom (h:float) (w:float) (pos:XYPos) =
    let in0 = {X=pos.X + w*0.25 ; Y=pos.Y + h}
    let in1 = {X=pos.X + w*0.75 ; Y=pos.Y + h}
    [in0; in1]
let getGateInputPositionsTop (h:float) (w:float) (pos:XYPos) =
    let in0 = {X=pos.X + w*0.25 ; Y=pos.Y}
    let in1 = {X=pos.X + w*0.75 ; Y=pos.Y}
    [in0; in1]
let getGateInputPositionsLeft (h:float) (w:float) (pos:XYPos) =
    let in0 = {X=pos.X ; Y=pos.Y + h*0.25}
    let in1 = {X=pos.X ; Y=pos.Y + h*0.75}
    [in0; in1]

let getGatePortPositions (inOrientation: PortOrientation) (inverted:bool) (h:float) (w:float) (pos:XYPos) =
    let wTmp = if inverted then w - 9. else w 
    let inputs = 
        match inOrientation with
        | Top -> getGateInputPositionsTop h wTmp pos
        | Bottom -> getGateInputPositionsBottom h wTmp pos
        | _ -> getGateInputPositionsLeft h w pos
    let out0 = {X=pos.X + w ; Y=pos.Y + h/2.}
    inputs, [out0]
    

let getMux2PortPositions (h:float) (w:float) (pos:XYPos) =
    let in0 = {X=pos.X ; Y=pos.Y + h*0.30}
    let in1 = {X=pos.X ; Y=pos.Y + h*0.70}
    let in2 = {X=pos.X + w/2. ; Y=pos.Y + h - 6.5}
    let out0 = {X=pos.X + w ; Y=pos.Y + h/2.}
    [in0; in1; in2], [out0]
let getDemux2PortPositions (h:float) (w:float) (pos:XYPos) =
    let in0 = {X=pos.X ; Y=pos.Y + h/2.}
    let in1 = {X=pos.X + w/2. ; Y=pos.Y + h - 6.5}
    let out0 = {X=pos.X + w ; Y=pos.Y + h*0.30}
    let out1 = {X=pos.X + w ; Y=pos.Y + h*0.70}
    [in0; in1], [out0; out1]

let getMergeWiresPortPositions (h:float) (w:float) (pos:XYPos) =
    let in0 = {X=pos.X ; Y=pos.Y}
    let in1 = {X=pos.X ; Y=pos.Y + h}
    let out0 = {X=pos.X + w ; Y=pos.Y + h/2.}
    [in0; in1], [out0]
let getSplitWirePortPositions (h:float) (w:float) (pos:XYPos) =
    let in0 = {X=pos.X ; Y=pos.Y + h/2.}
    let out0 = {X=pos.X + w ; Y=pos.Y}
    let out1 = {X=pos.X + w ; Y=pos.Y + h}
    [in0], [out0; out1]

let getPositionsOn portOrientation numOfPorts h w pos =
    match portOrientation with
    | Left -> 
        let lengthBetween = h / float (numOfPorts+1)
        [1..numOfPorts] |> List.map ((fun n -> float n * lengthBetween) >> (fun num -> {X=pos.X ; Y=pos.Y + num}))
    | Right -> 
        let lengthBetween = h / float (numOfPorts+1)
        [1..numOfPorts] |> List.map ((fun n -> float n * lengthBetween) >> (fun num -> {X=pos.X + w ; Y=pos.Y + num}))
    | Top -> 
        let lengthBetween = w / float (numOfPorts+1)
        [1..numOfPorts] |> List.map ((fun n -> float n * lengthBetween) >> (fun num -> {X=pos.X + num ; Y=pos.Y}))
    | Bottom -> 
        let lengthBetween = w / float (numOfPorts+1)
        [1..numOfPorts] |> List.map ((fun n -> float n * lengthBetween) >> (fun num -> {X=pos.X + num ; Y=pos.Y + h}))

let getRegularPortPositions (portOrientationIn, portOrientationOut) (nIn:int) (nOut:int) (h:float) (w:float) (pos:XYPos) =
    let inputs =
        match portOrientationIn with 
        | Top -> getPositionsOn Top nIn h w pos
        | Bottom -> getPositionsOn Bottom nIn h w pos
        | _ -> getPositionsOn Left nIn h w pos
    let outputs =
        match portOrientationOut with 
        | Top -> getPositionsOn Top nOut h w pos
        | Bottom -> getPositionsOn Bottom nOut h w pos
        | _ -> getPositionsOn Right nOut h w pos
    inputs, outputs



        



/// return a v4 (random) universally unique identifier (UUID)
let uuid():string = import "v4" "uuid"


//-----------------Code to record and print execution time statistics-------//

let timeNowInMicroS() = 
    System.DateTime.Now.Ticks
    |> (fun t -> t /10L)

type Stats = {
    Min: float
    Max: float
    Av: float
    Num: float
    }

/// add time t to st
let addTimeToStats (t:float) (st:Stats) =
    {
        Min = min st.Min t
        Max = max st.Max t
        Av = (st.Av*st.Num + t)/(st.Num+1.)
        Num = st.Num + 1.
    }

/// execution time stats indexed by name in recordExecutionStats
let mutable executionStats = Map<string,Stats> []

/// Run (f arg) recording its time in executionStats under name.
/// NB - this will run f multiple times if needed to estimate average speed more accurately.
/// If an execution time of 5ms for this function is too long reduce timeLimit.
/// The multiple time execution will not work, and will give lower than real results, if
/// f is memoised. In that case set timeLimit to 0. for only one execution.
let recordExecutionTimeStats (name: string) (f: 'a -> 'b) (arg: 'a) : 'b =
    let timeLimit = 0. // time in ms to execute f for.
    let t1 = timeNowInMicroS()
    let execTime() = float (timeNowInMicroS() - t1) / 1000.
    let res = f arg // do f
    let mutable iterations = 1
    while execTime() < timeLimit do // do f multiple times if it is fast to get more accurate speed statistics
        iterations <- iterations + 1
        f arg |> ignore // do f again
    let t = execTime() / float iterations
    executionStats <-
        Map.tryFind name executionStats
        |> Option.map (addTimeToStats t)
        |> Option.defaultValue {Min=t;Max=t;Av=t;Num=1.}  
        |> (fun st -> Map.add name st executionStats)
    res

/// print
let printStats() =
    executionStats
    |> Map.toList
    |> List.iter (fun (name,st) -> 
        printfn "%s time: min=%.3fms max=%.3fms av=%.3fms samples:%d" name st.Min st.Max st.Av (int st.Num))
    executionStats <- Map [] // reset stats

//--------------------------------Constants----------------------------------//

/// these determine the size of the canvas relative to the objects on it.
let canvasUnscaledDimensions : XYPos = 
    {X = 1000. ; Y = 1000.}

//let dragColor  = "DarkKhaki" // Old version DarkTurquoise
let dragColor  = "Turquoise" 
let constColor = "lightgrey"
let errorColor = "FireBrick"
let busColor = "Purple"
let drawLineColor_const = "green"
let drawLineColor_special = "royalblue"
let portColor_const = "slategrey"


let VerticalAdjustment = 7.5
    

