# HLP21 GROUP 9 

This markdown file contains a summary of all the features implemented in the group stage of our project.

Our interface documentation can be found under ./docs directory. A document is included for each of the three modules, that is Symbol, BusWire, and Sheet.

Main Features: 
1. 27/27 issie symbols implemented - All 27 symbols in issie are implemented.
2. Buswidth inference - For symbols in which all ports buswidth is not specified, Buswidth inference infers the undefined port buswidth based on the defined widths(can be input or output), extended to work for Multiplexer, Demultiplexer, MergeWire, Splitwire and IO Label and groups of these symbols.
3. Symbol creation - When the AddSymbol mesage is received, CreateNewSymbol is called and a new symbol is added to the Canvas
4. Symbol selection - When a Symbol on the screen is clicked, it is slected and turns green
5. Symbol deselection - When a symbol is deseletcted, it returns back to its grey hue
6. Highlight ports interactively when moving - Small circles are drawn on symbol ports and increase in opacity (darkness) as cursor moves closer to them and decrease in opacity as cursor moves further away
7. Highlight ports interactively when conecting - As a wire hovers above a port before it is connected, the circle drawn around that port increases in size and a small blue circle is drawn around it to indicate to the user which port the wire is connecting to.
8. Drag a group of symbols - When a group of symbols are highlighted, a new boundng box is created for them which enbles the group of symbols to be dragged across the canvas by moving mouse
9. Draw lines during drag and drop connection - While drawing a line from one port to another, a dashed blue line is simultaneously drawn to show user the direction of the wire.
10. Connect Wires - when multiples wires go into the same port, the wires are connected together tomake the canvas look neeat.
11. Symbol deletion - when Del is pressed, DeleteSymbol message is sent to Symbol which removes the symbol from the canvas, then the symbols and its connections are deleted.
12. Symbol drag and drop on canvas - Symbols can be dragged and dropped on the canvas, by seleting and moving the mouse
13. Undo - When Option-Z is clicked, the last action performed will be undone on the canvas
14. Redo - When Option-Shift-Z is clicked, the last action undone will be redone on the canvas 
15. Zoom - zoom in and zoom out respectively using mouse - transforms the canvas accordingly so that all other mouse functions are not affected
16. Select region of wires/symbols on canvas - Drag cursor over multiple symbols and/or their conections to select them, they would turn green to indicate their selection.
17. Deselect region of wires/symbols on canvas - Click away from the selected symbol(s) and/or their connections to deslect them, they should revert back to the colors they were before selection.
18. Buswidth annotation - the buswidth of a wire is written alongside it 
19. Copy symbol/wire or a group of symbols and wires - Option-C copies selected symbol(s)/wire(s).
20. Paste symbol/ wire or a group of symbols and wires - Option-V pastes selected symbol(s)/wires(s) on the canvas.
21. Copy and paste to new sheet - Selected symbols/ group of their connections can be pasted to new canvas.
22. Drawing a Wire: - When the AddWire message is sent, a wire is created on the canvas to join the two Ports sent with the message.
23. Wire moving with Ports: - When a port connected to a wire is moved, the wire will move as well to stay connected to the moving Port.
24. Selecting a Wire: - Clicking in a Wire's BoundingBox selects it turns green.
25. Deselecting a Wire: - Unckicking on a wire deselects it and it reverts back to original color (green, red, black or purple) depending on the symbol and error message.
26. Dragging a Wire: - Individual Wire segments can be moved around to adjust the routing.
27. Deleting a Wire: - When then Delete message is received all Selected wires are deleted.
28. Wire rerouting: We implemented a port rotation feature, which allows us to rotate the positions of the input and output ports, when the ports are rotated, the wires adjust to automatically to new port positions.
29. Catalog for inserting symbols - An insertion catalog is created to 
30. Symbol alignment - Symbols that are close to each other are automatically aligned to enhance the neatness of the canvas.
31. Multiple Sheets - Multiple sheets are impelemnted for instance switchign between Sheets. Switching between sheets can be done via the Catalogue
32. Input Port location switching (rotate symbol) - Option-A,W,S,D are used to shift the orientation of the input ports (left, up, down and right) respectively it has the effect of rotating the symbol.
33. Output Port location swicthing (rotate symbol) - Option-Shift-A,W,S,D are used to shift the orientation of the output ports (left, up, down and right) respectively it has the effect of rotating the symbol.
34. Error highlihgting (symbol and buswire) - When the buswidths of two connecting symbols do not match, both symbol and buswire highlight RED to indicate that there is an error



