# HLP21 GROUP 9 



Issie (Interactive Schematic Simulator with Integrated Editor) is an application for digital circuit design and simulation initially developed by Marco Selvatici. It is targeted at students and hobbyists that want to get a grasp of Digital Electronics concepts in a simple and fun way. Issie is designed to be beginner-friendly and guide the users toward their goals via clear error messages and visual clues. However, Issie has several bugs, the main bug being its schematic drawing library Draw2D which is implemented in Javascript. This is very slow and buggy, therefore thre main aim of the project will be to reimplement a drawing library specifcally catered to Issie with a pure F# implementation using the Elmish MVU (pure functional) web framework. 
[[1]](https://intranet.ee.ic.ac.uk/t.clarke/hlp/project.html)

An Elmish application works by using `model`, `view`, and `update` functions with messages used to update the model. Our project includes three separate modules: `Symbol`, `BusWire`, `Sheet` each operating as individual Elmish components.

Fork, clone, and build this repo. Look at the code in an IDE with intellisense to contribute.

This markdown file contains a summary of all the features implemented in the group stage of our project with additional information on how to run and contribute to the project.

A documentation for interfacing between modules can be found under ./docs directory. A document is included for each of the three modules, that is Symbol, BusWire, and Sheet.

## File Structure

| Files     | Description |
| :---      | :--- |
| Helpers.fs   | Contains helper functions commonly used by all of the other modules |
| CommonTypes.fs  | Contains data types that are used within the Issie application |
| SymbolRenderers.fs  | Collection of functions that generates the ReactElement of Symbols|
| Symbol.fs  |  Contains the algorithms for `Symbol` and `Port` generation, manipulation and also interface functions to access symbols within Symbol.fs |
| BusWire.fs   | Contains the algorithms for wiring and buswidth inference. It has access to `Symbol` Model. |      
| Sheet.fs  |  Manages coordination of mouse actions and key presses on the Canvas. It has access to `Symbol` and `BusWire` Model. |    


**For MAC users, use Option in replacement of Alt for keypresses.**
buswidth inference
### Main Features: 
1. All 27 Issie symbols implemented.
2. Symbol creation with automated naming
3. Symbol and wire selection, selecting multiple symbols with alt key, selecting multiple symbols within a box, deselecting.
4. Error highlighting (symbol and wires): when the buswidths of two connecting symbols do not match, both symbol and buswire highlight RED to indicate that there is an error
5. Drag a group of symbols: when a group of symbols are highlighted, you can move the symbols across the canvas.
6. Highlight ports interactively when moving - Small circles are drawn on symbol ports and its opacity (darkness) increases as the cursor moves closer to them and decrease in opacity as the cursor moves further away.
7. Highlight ports interactively when connecting, i.e. when connecting wires from one Port to another, Sheet will enlarge only connectable Ports. A small blue circle is drawn around the port to indicate cursor is within the range to complete the connection.
8. Symbol and wire deletion - when Del is pressed, a message is sent to remove all selected symbols and wires from the canvas. If the symbols have connections, connections will also be deleted.
9. Interactive draging and dropping og connections, i.e. while drawing a line from one Port to another, a dashed blue line is simultaneously drawn to show the user he is making a connection.
13. Dragging a Wire: Individual Wire segments can be moved around to adjust the routing.
14. Wire rerouting: We implemented a port rotation (port orientation) feature, which allows user to rotate the positions of the input and output ports. When the ports are rotated, the wires adjust automatically to new port positions.
15. Buswidth annotation: the buswidth of a wire is written alongside the wire.
16. Buswidth inference: buswidth inference - For symbols in which all ports buswidth is not specified, Buswidth inference infers the undefined port buswidth based on the defined widths(can be input or output), extended to work for Multiplexer, Demultiplexer, MergeWire, Splitwire and IO Label and groups of these symbols.
17. Undo: when Alt-Z is clicked, the last action performed will be undone on the canvas.
18. Redo: when Alt-Shift-Z is clicked, the last action undone will be redone on the canvas 
19. Zoom: Zoom in and zoom out respectively by holding down Alt while scrolling - transforms the canvas accordingly so that all other mouse functions are not affected.
20. Copy and Past symbol/wire or a group of symbols and wires. Alt-C copies selected symbol(s)/wire(s). Alt-V pastes selected symbol(s)/wires(s) on the canvas with a slight offset.
21. Copy and paste to new sheet - Selected symbols/ group of their connections can be pasted to new canvas.
22. Multiple Sheets: multiple sheets are implemented, for instance, switching between Sheets. Switching between sheets can be done via the Catalogue.
23. Symbol alignment: symbols that are close to each other are automatically aligned to enhance the canvas's neatness.
24. Input Ports, changing the orientation (rotate symbol): Alt-A,W,S are used to shift the orientation of the input ports, left, up, and down respectively; it has the effect of rotating the symbol.
30. Output Port, changing the orientation  (rotate symbol): Alt-Shift-W,S,D are used to shift the orientation of the output ports, up, down and, right respectively.
32. New symbols can be created by using the catalogue on the left by clicking on the desired symbol type.






## Prerequisites

* If npm is not present it must be installed.
* Dotnet Core SDK. Version >= 3.1

## Running the Project
* After cloning the repo, navigate to the root directory
* Run build.sh on Macos or linux or build.cmd on Windows.
* The demo can be run by executing npm run dev in the terminal window after building once.




### TODO

* Implement the integration with Issie.
* Improve Wire Routing and perhaps allow users to create and delete individual segments.
* Allow select and change the orientation of ports one by one.
* Merge the 3 models, i.e. Sheet, Buswire and Symbol models (to eliminate redundancy). Improve interfacing between modules to remove the need for the same data being present in more than one module.


