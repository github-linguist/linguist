program main;

 mailbox#(bit) p2c_cmd = new;
 mailbox#(string) p2c_data = new;
 mailbox#(int) c2p_data = new;

 initial begin
   int fh = $fopen("input.txt", "r");
   string line;
   int count;
   while ($fgets(line, fh)) begin
     p2c_cmd.put(0);
     p2c_data.put(line);
   end
   p2c_cmd.put(1);
   c2p_data.get(count);
   $display( "COUNT: %0d", count );
 end

 initial begin
   bit done;
   int count;
   while (!done) begin
     p2c_cmd.get(done);
     if (done) begin
       c2p_data.put(count);
     end
     else begin
       string line;
       p2c_data.get(line);
       $display( "LINE: %s", line);
       count++;
     end
   end
 end

endprogram
