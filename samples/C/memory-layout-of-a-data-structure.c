struct RS232_data
{
  unsigned carrier_detect        : 1;
  unsigned received_data         : 1;
  unsigned transmitted_data      : 1;
  unsigned data_terminal_ready   : 1;
  unsigned signal_ground         : 1;
  unsigned data_set_ready        : 1;
  unsigned request_to_send       : 1;
  unsigned clear_to_send         : 1;
  unsigned ring_indicator        : 1;
};
