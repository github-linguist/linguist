program main;
  int values[$];

  initial begin
    values = '{ 1, 3, 7, 11 };
    foreach (values[i]) begin
       $display( "%0d --> %0d", i, values[i] );
    end
  end
endprogram
