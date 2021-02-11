/*--------------------
Original Author: Yuma Ueda
Contact Point: cyan@0x00a1e9.dev
ram.v
Created: 12/19/2019

Copyright 2019 Yuma Ueda

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--------------------*/


`include "defs.v"


module RAM (
    input wire [31:0] addr, write_data,
    input wire memwrite,
    input wire [1:0] storeops,
    output wire [31:0] read_data,
    input wire CLK, reset
    );

    reg [7:0] ram [`RAM_COL_MAX-1:0];

    always @(posedge CLK) begin
        if (reset) begin : rst
            integer i;
            for (i = 0; i < `RAM_COL_MAX; i = i + 1) begin
                ram[i] <= 0;
            end
        end
        else if (memwrite) begin
            do_store();
        end
    end

    assign read_data = {
        ram[addr],
        ram[addr+1],
        ram[addr+2],
        ram[addr+3]
    };

    task store_byte;
    begin
        ram[addr] <= write_data[7:0];
    end
    endtask

    task store_half_word;
    begin
        ram[addr]   <= write_data[15:8];
        ram[addr+1] <= write_data[7:0];
    end
    endtask

    task store_word;
    begin
        ram[addr]   <= write_data[31:24];
        ram[addr+1] <= write_data[23:16];
        ram[addr+2] <= write_data[15: 8];
        ram[addr+3] <= write_data[ 7: 0];
    end
    endtask

    task do_store;
    begin
        (* full_case *)
        case (storeops)
            `STORE_B: store_byte();
            `STORE_H: store_half_word();
            `STORE_W: store_word();
        endcase
    end
    endtask
endmodule
