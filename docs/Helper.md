```

type BoundingBox = {
    TopLeft : XYPos
    BottomRight : XYPos
}

let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posOf x y = {X=x;Y=y}

type MouseOp = 
    /// button up
    | Up
    /// button down
    | Down
    /// Move with button up
    | Move 
    /// Move with button Down
    | Drag
    ///Scrolling
    | CtrlScroll
    | Scroll

type CanvasProps = {
    height:float
    width :float 
    zoom  :float
    }

type MouseT = {
    Op: MouseOp
    Pos: XYPos
    }

let posDiff a b =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd a b =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

let posScaled (pos:XYPos) (zoom:float) = {X = pos.X/zoom; Y= pos.Y/zoom } 

    
```


