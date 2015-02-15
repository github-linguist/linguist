open ExtLib
class rs232_data = object
  val d = BitSet.create 9

  method carrier_detect      = BitSet.is_set d 0
  method received_data       = BitSet.is_set d 1
  method transmitted_data    = BitSet.is_set d 2
  method data_terminal_ready = BitSet.is_set d 3
  method signal_ground       = BitSet.is_set d 4
  method data_set_ready      = BitSet.is_set d 5
  method request_to_send     = BitSet.is_set d 6
  method clear_to_send       = BitSet.is_set d 7
  method ring_indicator      = BitSet.is_set d 8

  method set_carrier_detect      b = (if b then BitSet.set else BitSet.unset) d 0
  method set_received_data       b = (if b then BitSet.set else BitSet.unset) d 1
  method set_transmitted_data    b = (if b then BitSet.set else BitSet.unset) d 2
  method set_data_terminal_ready b = (if b then BitSet.set else BitSet.unset) d 3
  method set_signal_ground       b = (if b then BitSet.set else BitSet.unset) d 4
  method set_data_set_ready      b = (if b then BitSet.set else BitSet.unset) d 5
  method set_request_to_send     b = (if b then BitSet.set else BitSet.unset) d 6
  method set_clear_to_send       b = (if b then BitSet.set else BitSet.unset) d 7
  method set_ring_indicator      b = (if b then BitSet.set else BitSet.unset) d 8
end
;;
