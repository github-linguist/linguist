**free

/if not defined (SMS)
/define SMS

dcl-ds smsResponse qualified template;
  sid           varchar(64);
  date_created  varchar(64);
  date_updated  varchar(64);
  date_sent     varchar(64);
  account_sid   varchar(64);
  phone_to      varchar(32);
  phone_from    varchar(32);
  msg_srv_sid   varchar(64);
  body          varchar(1600);
  status        varchar(16);
  num_segments  varchar(8);
  num_media     varchar(8);
  direction     varchar(32);
  api_version   varchar(16);
  price         varchar(8);
  price_unit    varchar(4);
  error_code    varchar(8);
  error_message varchar(512);
  uri           varchar(256);
end-ds;


dcl-ds smsRequest qualified template;
  phone_to   varchar(16);
  phone_from varchar(16);
  msg        varchar(1600);
  account    varchar(64);
  auth       varchar(64);
end-ds;


// Send request DS to Twilio API, log and return entire response as DS
dcl-pr sendSmsVerbose likeds(smsResponse);
  req likeds(smsRequest);
end-pr;


// Send request to Twilio API, log and only return error code
// For simple requests where we don't care about the details immediately.
// We just care whether it successfully sent the request
dcl-pr sendSms varchar(8);
  phone_to     varchar(16);
  phone_from   varchar(16);
  msg          varchar(1600);
  account      varchar(64);
  auth         varchar(64);
end-pr;


/endif