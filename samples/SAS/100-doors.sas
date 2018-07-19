data _null_;
   open=1;
   close=0;
   array Door{100};
   do Pass = 1 to 100;
      do Current = Pass to 100 by Pass;
         if Door{Current} ne open
            then Door{Current} = open;
            else Door{Current} = close;
      end;
   end;
   NumberOfOpenDoors = sum(of Door{*});
   put "Number of Open Doors:  " NumberOfOpenDoors;
run;
