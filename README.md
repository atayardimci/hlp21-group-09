# SVG Draw Application Project Skeleton Code For HLP Project 2021

See [The SVG Demo README](https://github.com/tomcl/hlp21-svg-elmish-demo/blob/master/README.md) for 
code structure and how to build the dev environment: this is the same as the WS 3 SVG demo.

You should already have looked at the SVG Demo code and have some understanding of how an Elmish application works, using
a `model`, `view`, and `update` function with messages used to update the model. The project skeleton code 
illustrates a more complex example of Elmish code, 
with three separate modules: `Symbol`, `BusWire`, `Sheet` each operating as individual Elmish components.

Fork, clone, and build this repo. Look at the code in an IDE with intellisense.

In order to see the functionality currently used by [Issie](https://github.com/tomcl/ISSIE) you can 
download the Issie binaries and try it out. There is a video which introduces the project objectives and explains
what is good and bad about the current (Draw2D) Issie Draw block functionality. You can also fork and clone the Issie repo, 
and build its code. The build is very similar to the project skeleton
but a lot more complex (10K lines of F#).

The project work specification contains details of expected work during individual work phase in each of the three modules.

## Introduction

The skeleton code draws circles and (random) lines on an svg canvas. It is interactive; circles can be moved as in the WS 3 demo.
In addition lines are drawn between circles that move with the circles; this is similar to the functionality needed for symbols and
wires. The given implementation will need to be modified even for this basic functionality because:
* The skeleton drag mechanism does not use programmatic bounding boxes, but instead HTML to determine which circle is clicked.
* The skeleton `Symbol` code does not used correctly compensated coordinates. Try it setting zoom=3. How does dragging of circles
go wrong?
* The skeleton code does not understand ports - connection points on the edge of symbols.
* The skeleton wires are not right-angled multi-segment.

The skeleton introduces a modular structure which can be used to combine individual coding from 3 people into a single 
fully working schematic draw program, and some initial messages most of which will be useful in project code. You are free to
change message types as you like within  a group. Messages or functions `Symbol:symbolPos` owned by any component
used to communicate with other components should not be changed in a way that alters their specification without 
consultation with those affected.


## Information on Elmish & Code Structure

In the demo code the three modules each have views that create objects on the SVG canvas:
* Symbol: the component outlines
* BusWire: the connecting wires and wire annotations
* Sheet: The rubber band showing the new connection during drag-and drop connection. The dyanmically sized circles showing 
connection end-points. Possibly dynamic guides showing component algnment.

These have a parent child relationship:

`Sheet --> BusWire --> Symbol`

So the `BusWire` module can have a view function that depends on the `Symbol` module state (`Symbol.model`). The `Symbol` view 
function cannot depend on anything except `Symbol.model`. This dependence is needed because wire endpoints are defined 
by symbol and port positions. The alternative, keeping `BusWire` and `Symbol` as separate children of `Sheet`:

```
Sheet --> BusWire 
      --> Symbol
```

would require 
everything that updated port positions to also send a message to `Wire` to update wire endpoints. Thus the endpoint data is  effectively duplicated. That might sometimes be required for performance reasons, but is bad practice unless forced because it makes it more difficult to write correct code.


Good overview of how Elmish large-scale structure is supported by various built-in Elmish functions. This is written in the context
of a web application with (effectively) multiple pages written as a SPA (single-page application). The sections on Commands are helpful, some other parts less so.
* [Working with Elmish](https://medium.com/@MangelMaxime/my-tips-for-working-with-elmish-ab8d193d52fd). 

## Information on SVG React Helpers for Elmish

See SVG syntax below, and the skeleton, and [w3schools](https://www.w3schools.com/graphics/svg_intro.asp) for how to use SVG elements. 
For project work the SVG elements certainly needed are:
* `line` (as in skeleton `Buswire`)
   * Possibly better alternatives `polyline` and `polygon` which take a sequence of points. See skeleton for example of polygon.
* For more complex objects `path` which can do anything.
* `text` for text - see [w3schools](https://www.w3schools.com/graphics/svg_text.asp) etc
* `g` which groups elemnets (like `div` for normal HTML), see skeleton.
* `svg` which creates an SVG canvas of specified size, see skeleton.

Note also that the order in which elements are put onto a canvas matters. the *last* element will be on top. In the skeleton
* the blue overlay translucent triangle is on top, because it is last in its `g` list of elements.
* That means that it alters the color of the elements underneath (opacity 0.1). Also mouse clicks are collected by that element
and not passed through to the underlying svg circles. Therefore the skeleton dragging does not work. 
The solution is to use bounding boxes and determine which object is clicke dprogrammatically.

## Keyboard Interface Gotcha

The SVG window is not focussed to receive key presses - electron does not therefore support them directly, and the `OnKeyPress`, 
`OnKeyDown` DOM attributes will now work. Instead you can use a subscription and define electron global shortcuts for 
the keys you need in (perhaps invisible) electron menus as in the skeleton code.


## SVG Geometry

### Zoom

The skeleton code uses a fixed size SVG canvas of 1000X1000 pixels. This is then transformed by zoom. Possibly 1000X1000
is too small (though good for a demo) 2000X2000 would be more typical for zoom=1 to fill the screen, but it is not very critical.
Because zoom is variable all
that matters is the relative size of components (symbols) and the canvas, which determines how large is the canvas 
relative to the objects on it. Symbols will probably also have fixed size text and this must be readable after zoom.

It is important that zoom is separated from all other use of coordinates, so that the current zoom 
can be ignored when writing all other SVG code.

The skeleton code has working zoom, but does not quite do this perfectly. It would need to 



### Bounding boxes

A common technique used in 2d drawing is that of bounding boxes. Objects are approximated by rectangles, specified by two pairs
of corner coordinates: *bounding boxes*. It is easy to write code that determines:
* Is a point within a bounding box?
* Do two bounding boxes intersect?
Code to do these things is inherently fast can get slower if there are a very large number of objects.
In that case there are various speedups by sorting the
box coordinates along one dimension and doing a binary search. The Draw2d (as currently used by Issie) authors claim that Draw2d is slow 
because of the very inefficient checks of bounding boxes used in its implementation. Even fast techniques can be a problem if
used badly!

One typical example is in deciding which object (or objects) are clicked by mouse clicks and rectangular drags:
* The skeleton code follows WS2 svg demo and uses custom mouse event handlers embedded in the svg.
* This is not flexible enough for project work.
* You should use the `mouseMsg` message to detect mouse `up`, `down` and movement. 
* You can determine which object is clicked programmatically by using bounding boxes and checking whether the click coordinates
lie inside the bounding box. This allows:
   * two objects close together to be disambiguated as required
   * wires that are thin to have larger bounding boxes
   * symbols that are complex to have suitably sized bounding boxes which are processed by other code as well as for mouse clicks.


## Elmish Helper SVG Syntax


React virtual DOM elements are created in Elmish using F# helper functions of form:

```
elementName (props:IProp list) (children: ReactElement list)
```

They divide into SVG elements (that can be used as children of an `svg` element that creates an SVG canvas) and DOM elments 
that correspond to HTML DOM elements. There are also some `void` elements:

```
voidElementName (props: IProp list)
```

That deals with elements that are not allowed to have children.

See [Fable standard react documentation](https://github.com/fable-compiler/fable-react/blob/master/src/Fable.React.Standard.fs) 
for the list of SVG, DOM and Void React element helper functions. The large number of valid `props` can be found listed
[here](https://github.com/fable-compiler/fable-react/blob/master/src/Fable.React.Props.fs). Props can also be discovered 
from the IDE by autocompletion for example `SVGattr.`

## Electron Documentation

The Electron framework API - and associated Node libraries - is extensive and complex. 
Luckily 99% of what you need for the project work is demonstrated in this project skeleton. In order to change or
enhance these features you will sometimes need the [electron API documentation](https://www.electronjs.org/docs). Note
that 95% of the electron features are accessible via F# interfaces which define types functions and discriminated
unions.

One way to find useful code examples is the [Issie application](https://github.com/tomcl/ISSIE). The files under `src/renderer/UI`
all contain code using the electron framework. Fork and clone (or clone) the Issie repo to get a copy of the files. 
Build them once to get fully working intellisense and view the code (with type information and documentation) in an IDE.




