## File Structure

| Files     | Description |
| :---      | :--- |
| Helpers.fs   | Contains helper functions commonly used by all of the other modules |
| CommonTypes.fs  | Contains data types that are used within the Issie application |
| SymbolRenderers.fs  | Collection of functions that generates the ReactElement of Symbols|
| Symbol.fs  |  Contains the algorithms for `Symbol` and `Port` generation, manipulation and also interface functions to access symbols within Symbol.fs |
| BusWire.fs   | Contains the algorithms for wiring and buswidth inference. It has access to `Symbol` Model. |      
| Sheet.fs  |  Manages coordination of mouse actions and key presses on the Canvas. It has access to `Symbol` and `BusWire` Model. |    



