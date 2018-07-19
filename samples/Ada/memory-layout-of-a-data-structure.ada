type Bit is mod 2;
type Rs_232_Layout is record
   Carrier_Detect      : Bit;
   Received_Data       : Bit;
   Transmitted_Data    : Bit;
   Data_Terminal_ready : Bit;
   Signal_Ground       : Bit;
   Data_Set_Ready      : Bit;
   Request_To_Send     : Bit;
   Clear_To_Send       : Bit;
   Ring_Indicator      : Bit;
end record;

for Rs_232_Layout use record
   Carrier_Detect      at 0 range 0..0;
   Received_Data       at 0 range 1..1;
   Transmitted_Data    at 0 range 2..2;
   Data_Terminal_Ready at 0 range 3..3;
   Signal_Ground       at 0 range 4..4;
   Data_Set_Ready      at 0 range 5..5;
   Request_To_Send     at 0 range 6..6;
   Clear_To_Send       at 0 range 7..7;
   Ring_Indicator      at 0 range 8..8;
end record;
