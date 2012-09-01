/* Author: Jenner Hanni
 * Project: Harry Porter Relay Computer
 * File: Address Bus
 * License: MIT http://opensource.org/licenses/MIT
*/

interface Addr_Bus();

  parameter ADDR_BUS_WIDTH = 8;

  wire [ADDR_BUS_WIDTH-1:0] address;

  modport reg_M     (output address);

  modport reg_XY    (inout address);

  modport reg_J     (output address);

  modport reg_PC    (inout address);

  modport reg_INC   (output address);

  modport INC16     (input address);

  modport memory    (input address);

endinterface
