# HLP21 GROUP 9 



Issie (Interactive Schematic Simulator with Integrated Editor) is an application for digital circuit design and simulation. It is targeted at students and hobbyists that want to get a grasp of Digital Electronics concepts in a simple and fun way. Issie is designed to be beginner-friendly and guide the users toward their goals via clear error messages and visual clues. However Issie has several bugs, the main bug being its schematic drawing library Draw2D which is implemented in Javascript. This is very slow and buggy, therefore thre main aim of the project will be to reimplement the parts of Draw2d used in Issie from scratch (the existing js code is irrelevant to this), with a pure F# implementation using the Elmish MVU (pure functional) web framework. 
[ref: https://intranet.ee.ic.ac.uk/t.clarke/hlp/project.html]

This markdown file contains a summary of all the features implemented in the group stage of our project.

Our interface documentation can be found under ./docs directory. A document is included for each of the three modules, that is Symbol, BusWire, and Sheet.

**For MAC users, use Option in replacement of Alt for keypresses.**

### Main Features: 
1. 27/27 issie symbols implemented - All 27 symbols in issie draw 2D are implemented.
2.. Symbol creation - When the AddSymbol message is received, CreateNewSymbol is called, and a new symbol is added to the Canvas
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





