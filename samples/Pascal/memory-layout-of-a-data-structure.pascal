program memoryLayout;

type
  T_RS232 = (
    carrier_detect,
    received_data,
    transmitted_data,
    data_terminal_ready,
    signal_ground,
    data_set_ready,
    request_to_send,
    clear_to_send,
    ring_indicator
  );

var
  Signal: bitpacked array[T_RS232] of boolean;

begin
  Signal[signal_ground] := true;
end.
