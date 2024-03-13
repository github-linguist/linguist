**free

  ctl-opt nomain;

  /include sms_h.rpgle


  // Send request to Twilio API, log and return response as DS
  dcl-proc sendSmsVerbose export;
    dcl-pi *N likeds(smsResponse);
      req     likeds(smsRequest);
    end-pi;
    
    dcl-ds resp    likeds(smsResponse);
    dcl-s  resp_rs sqltype(RESULT_SET_LOCATOR);

    exec SQL call send_sms(:req.phone_to, :req.phone_from, :req.msg, :req.account, :req.auth);
    exec SQL associate result set locator (:resp_rs) with procedure send_sms;
    exec SQL allocate c1 cursor for result set :resp_rs;
    exec SQL fetch next from c1 into :resp;
    exec SQL close c1;
    exec SQL insert into sms_log values(:resp); // log response

    return resp;
  end-proc;


  // Send request to Twilio API, log and return error code.
  // For simple requests where we don't care about the details immediately.
  // We just care whether it successfully sent the request
  dcl-proc sendSms export;
    dcl-pi *N    varchar(8); 
      phone_to   varchar(16);
      phone_from varchar(16);
      msg        varchar(1600);
      account    varchar(64);
      auth       varchar(64);
    end-pi;

    dcl-ds resp likeds(smsResponse);
    dcl-ds req  likeds(smsRequest);

    req.phone_to = phone_to;
    req.phone_from = phone_from;
    req.msg = msg;
    req.account = account;
    req.auth = auth;

    resp = sendSmsVerbose(req);
    return resp.error_code;
  end-proc;