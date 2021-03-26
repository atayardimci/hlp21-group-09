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

### Main Features: 
1. All 27 Issie symbols implemented
2. Symbol creation - When the AddSymbol message is received, CreateNewSymbol is called, and a new symbol is added to the Canvas
3. Symbol selection - When a Symbol on the screen is clicked, it is selected and turns turquoise. You can select multiple by holding down Alt while clicking or by dragging a region to include the symbols.
4. Symbol deselection - When a symbol is deselected, it returns to its grey hue
6. Drag a group of symbols - When a group of symbols are highlighted, you can move the symbols across the canvas by holding left-click.
7. Highlight ports interactively when moving - Small circles are drawn on symbol ports and its opacity (darkness) increases as the cursor moves closer to them and decrease in opacity as the cursor moves further away
8. Highlight ports interactively when connecting -When connecting wires from one Port to another, Sheet will enlarge only connectable Ports. A small blue circle is drawn around the port to indicate cursor is within the range to complete the connection.
9. Symbol deletion - when Del is pressed, DeleteSymbol message is sent to Symbol which removes the symbol from the canvas. If the symbols have connections, connections will also be deleted.
10. Draw lines during drag and drop connection - While drawing a line from one Port to another, a dashed blue line is simultaneously drawn to show the user he is making a connection.
11. Selecting a Wire: - Clicking in a Wire's BoundingBox selects it turns green. You can select multiple either by holding Alt while clicking or dragging a region to include the wires you want to select.
12. Deselecting a Wire: - Unclicking on a wire deselects it, and it reverts back to original color (green, red, black or purple) depending on the symbol and error message.
13. Dragging a Wire: - Individual Wire segments can be moved around to adjust the routing.
14. Deleting a Wire: - When the Delete message is received all Selected wires are deleted.
15. Wire rerouting: We implemented a port rotation feature, which allows us to rotate the positions of the input and output ports. When the ports are rotated, the wires adjust automatically to new port positions.
16. Select region of wires/symbols on canvas - Drag cursor over multiple symbols and/or their connections to select them. They would turn green to indicate their selection.
17. Deselect region of wires/symbols on canvas - Click away from the selected symbol(s) and/or their connections to deselect them, they should revert to the colours they were before selection.
18. Buswidth annotation - the buswidth of a wire is written alongside the wire.
19. Undo - When Alt-Z is clicked, the last action performed will be undone on the canvas
20. Redo - When Alt-Shift-Z is clicked, the last action undone will be redone on the canvas 
21. Zoom - Zoom in and zoom out respectively by holding down Alt while scrolling - transforms the canvas accordingly so that all other mouse functions are not affected
22. Copy symbol/wire or a group of symbols and wires - Alt-C copies selected symbol(s)/wire(s).
23. Copy and paste to new sheet - Selected symbols/ group of their connections can be pasted to new canvas.
24. Paste symbol/ wire or a group of symbols and wires - Alt-V pastes selected symbol(s)/wires(s) on the canvas with a slight offset.
25. Multiple Sheets - Multiple sheets are implemented, for instance, switching between Sheets. Switching between sheets can be done via the Catalogue
26. Buswidth inference - For symbols in which all ports buswidth is not specified, Buswidth inference infers the undefined port buswidth based on the defined widths(can be input or output), extended to work for Multiplexer, Demultiplexer, MergeWire, Splitwire and IO Label and groups of these symbols.
27. Symbol alignment - Symbols that are close to each other are automatically aligned to enhance the canvas's neatness.
28. Input Port location switching (rotate symbol) - Alt-A,W,S,D are used to shift the orientation of the input ports (left, up, down and right) respectively, it has the effect of rotating the symbol.
29. Output Port location switching (rotate symbol) - Alt-Shift-A,W,S,D are used to shift the orientation of the output ports (left, up, down and right) respectively it has the effect of rotating the symbol.
30. Error highlighting (symbol and buswire) - When the buswidths of two connecting symbols do not match, both symbol and buswire highlight RED to indicate that there is an error
31. Catalog for inserting symbols - Catalogue of symbols for adding symbols.
32. Initial naming


* New symbols can be created by using the catalogue on the left by clicking on the desired symbol type.
* It is possible to swap between the two sheets by clicking on the appropriate one in the catalogue.
* Multiple symbols can be dragged together by drawing a box around them.
* Backspace key is used to delete symbols. Multiple symbols can be deleted as described in the previous example.
* Wire connections can be made by dragging from one port to the next. The symbols and the wire will turn red if the connection is invalid.
* Last action can be undone by the Alt + z keybind.




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


