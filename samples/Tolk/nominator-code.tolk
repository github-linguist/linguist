// Nominator pool

fun onExternalMessage(inMsg: slice) {
  // do nothing for external messages
}

fun onInternalMessage(msgValue: int, inMsgCell: cell, inMsg: slice) {
  var cs = inMsgCell.beginParse();
  var flags = cs.loadUint(4);  // int_msg_info$0 ihr_disabled:Bool bounce:Bool bounced:Bool
  var s_addr: slice = cs.loadAddress();
  var elector: slice = elector_address();
  var config: slice = config_address();

  var is_processed: int = 0;

  if ( equal_slices(s_addr, elector) ) {
    process_elector_response(flags, inMsg, msgValue);
    is_processed = -1;
  }

  if ( (~ is_processed) & equal_slices(s_addr, config) ) {
    process_config_response(flags, inMsg);
    is_processed = -1;
  }

  if (flags & 1) {
    // ignore all bounced messages
    return;
  }

  var storage: tuple = storage_tuple(load_data());
  var validator_address: slice = storage.car();

  if ( (~ is_processed) & equal_slices(s_addr, validator_address) ) {
    is_processed = process_validator_request(inMsg, msgValue, storage, s_addr);
  }

  if( ~ is_processed)  {
    is_processed = process_nominator_request(inMsg, msgValue, storage, s_addr);
  }
  assert(is_processed) throw 404;
}
