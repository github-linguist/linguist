
#
# (C) Tenable, Inc.
#
# This script is released under one of the Tenable Script Licenses and may not
# be used from within scripts released under another license without the
# authorization from Tenable, Inc.
#
# @NOGPL@
#
# smtp_func.inc
# Revision: 1.36

# You need to include global_settings.inc and misc_func.inc
# NTLM

include("http_crypto_func.inc");
include("dump.inc");



function smtp_close(socket)
{
  send(socket:socket, data:'QUIT\r\n');
  smtp_recv_line(socket:socket);
  close(socket);
}


function smtp_auth(socket, method, user, pass)
{
  local_var data, match, hmac, hash, type1, type2, type3, msg_len;
  local_var flags, nonce, domain, hostname, user16, dom_len, user_len;
  local_var host_len, user_off, host_off, lm_resp_off, nt_resp_off;
  local_var cnonce, lm_resp, h, nt_resp;
  local_var smethod;
  local_var success;

  success = false;

  if ( method == 'CRAM-MD5' )
  {
    send(socket:socket, data:'AUTH CRAM-MD5\r\n');
    data = smtp_recv_line(socket:socket);
    if ( data =~ '334[- ]' )
    {
      match = eregmatch(string:data, pattern:"^.*334[- ](.*)$");
      hmac = hexstr(HMAC_MD5(data:base64_decode(str:match[1]), key:pass));

      hash = base64(str:strcat(user, ' ', hmac));
      send(socket:socket, data:hash + '\r\n');
      data = smtp_recv_line(socket:socket);

      if ( data =~ '235[- ]' )
        success = true;
    }
  }

  if ( method == 'PLAIN' )
  {
    hash = base64(str:strcat('\0', user, '\0', pass));
    send(socket:socket, data:strcat('AUTH PLAIN ', hash, '\r\n'));
    data = smtp_recv_line(socket:socket);

    if ( data =~ '235[- ]' ) 
      success = true;
  }

  if ( method == 'LOGIN' )
  {
    send(socket:socket, data:'AUTH LOGIN\r\n');
    data = smtp_recv_line(socket:socket);
    if ( data =~ '334[- ]VXNlcm5hbWU' )
    {
      send(socket:socket, data:strcat(base64(str:user), '\r\n'));
      data = smtp_recv_line(socket:socket);
      if ( data =~ '334[- ]UGFzc3dvcmQ' )
      {
        send(socket:socket, data:strcat(base64(str:pass), '\r\n'));
        data = smtp_recv_line(socket:socket);

        if ( data =~ '235[- ]' )
          success = true;
      }
    }
  }

  if ( method == 'NTLM' )
  {
    # For more details on SMTP NTLM Authentication, see
    # http://msdn.microsoft.com/en-us/library/cc246870.aspx
    # For more details on NTLM in general, read comments in http_request.inc

    send(socket:socket, data:'AUTH NTLM\r\n');
    data = smtp_recv_line(socket:socket);

    if ( data =~ '334[- ]ntlm supported' )
    {
      # Build and send NTLM Type 1 message
      type1 = strcat('NTLMSSP\x00\x01\x00\x00\x00\x07\x82\x08\x00',   
	             '\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0' );
      send(socket:socket, data:strcat(base64(str:type1), '\r\n'));
      data = smtp_recv_line(socket:socket);

      match = eregmatch(string:data, pattern:"^.*334[- ](.*)$");
      if ( !isnull(match) )
      {
        # Make sure we have a type 2 NTLMSSP response.
        type2 = base64_decode(str:match[1]);
        if ( substr(type2, 0, 7) != 'NTLMSSP\0' || ord(substr(type2, 8, 8)) != 2 )
        {
          log_smtp("SMTP Authentication method '", method, "' failed: ", data);
          return false;
        }

        # Capture the flags and challenge nonce.
        msg_len = LEword(blob:type2, pos:16);
        flags = LEword(blob:type2, pos:20);
        nonce = substr(type2, 24, 31);

        # Build the authentication response
        domain = "";
        hostname = this_host_name();
        user16 = user;
        domain = ascii2utf16LE(ascii:toupper(domain)); 
        hostname = ascii2utf16LE(ascii:hostname);
        user16 = ascii2utf16LE(ascii:user);
        dom_len = strlen(domain);
        user_len = strlen(user16);
        host_len = strlen(hostname);
        user_off = 64 + dom_len;
        host_off = user_off + user_len;
        lm_resp_off = host_off + host_len;
        nt_resp_off = lm_resp_off + 0x18;
        msg_len = nt_resp_off + 0x18;
   
        # Calculate responses
        cnonce = _rand64();
        lm_resp = strcat(cnonce, '\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0');
        h = substr(MD5(strcat(nonce, cnonce)), 0, 7);
        nt_resp = HTTP_NTLM_Response(password:ascii2utf16LE(ascii:pass), challenge:h);
        nt_resp = nt_resp[0];

        # Build Type 3 Message
        type3 = strcat( 'NTLMSSP\0', '\x03\x00\x00\x00',
              '\x18\x00', '\x18\x00', mkLEword(lm_resp_off), '\0\0', 
              '\x18\x00', '\x18\x00', mkLEword(nt_resp_off), '\0\0',
              mkLEword(dom_len), mkLEword(dom_len), 
              '\x40\x00', '\0\0', # domain offset
              mkLEword(user_len), mkLEword(user_len), 
              mkLEword(user_off), '\0\0',
              mkLEword(host_len), mkLEword(host_len), 
              mkLEword(host_off), '\0\0',
              '\0\0\0\0',
              '\0\0\0\0',
              '\x05\x82\x08\x00',
              domain,
              user16,
              hostname,
              lm_resp,
              nt_resp );

        send(socket:socket, data:strcat(base64(str:type3), '\r\n'));
        data = smtp_recv_line(socket:socket);

        if ( data =~ '235.*Authentication successful' ) 
          success = true;
      }
    }
  }

  if ( !success )
    log_smtp("SMTP Authentication method '", method, "' failed: ", data);

  return success;
}

function smtp_send_socket(socket, from, to, body)
{
  local_var buff;
  local_var dest;
  local_var dests;
  local_var timeout;

  timeout = 120;

  send(socket:socket, data:string("RSET\r\n"));
  buff = smtp_recv_line(socket:socket);

  # Here, we might test the return code
  if ( from !~ ' *<.*> *' ) 
    from = strcat('<', from, '>');

  send(socket:socket, data:string("MAIL FROM: ", from, "\r\n"));
  buff = smtp_recv_line(socket:socket);

  if ( !ereg(pattern:"^2[0-9][0-9] ", string:buff) ) 
  { 
   log_smtp("SMTP MAIL FROM failed: ", buff);
   return 0;
  }

  foreach dest (to)
  {  
    dest = dest["email"];
    if ( dest !~ ' *<.*> *' ) 
      dest = strcat('<', dest, '>');

    send(socket:socket, data:string("RCPT TO: ", dest, "\r\n"));
    buff = smtp_recv_line(socket:socket);

    if ( !ereg(pattern:"^2[0-9][0-9] ", string:buff) ) 
    { 
     log_smtp("SMTP RCPT TO failed: ", buff);
     return 0;
    }
  }

  send(socket:socket, data:string("DATA\r\n"));
  buff = smtp_recv_line(socket:socket);

  if ( !ereg(pattern:"^3[0-9][0-9] ", string:buff) )
  { 
    log_smtp("SMTP DATA failed: ", buff);
    return 0;
  }

  send(socket:socket, data:body);
  send(socket:socket, data:string("\r\n.\r\n"));
  buff = smtp_recv_line(socket:socket);

  if ( !ereg(pattern:"^2[0-9][0-9] ", string:buff) )
  { 
    log_smtp("SMTP DATA data failed: ", buff);
    return 0;
  }

  return 1;
}


function smtp_recv_line(socket, code, retry, last)
{
  local_var timeout;
  local_var ret, n, r, pat;
 
  timeout = 120;

  if ( isnull(code) )
    pat = "^[0-9][0-9][0-9]-";
  else
    pat = strcat("^", code, "-");

  ret = "";
  r = recv_line(socket:socket, length:4096, timeout:timeout);
  #
  n = 0;
  while ( !r && n ++ < retry )
    r = recv_line(socket:socket, length:4096, timeout:timeout);
  #
  n = 0;
  ret = r;
  if ( strlen(r) < 4 )
  { 
    log_smtp(debug:true, "smtp_recv_line: ", r);
    return r;
  }

  while ( ereg(pattern:pat, string:r) )
  {
    n = n + 1;
    r = recv_line(socket:socket, length:4096, timeout:timeout);
    if ( strlen(r) == 0 ) 
      break;

    if ( n > 512 )
      return NULL;

    if ( last ) 
      ret = r;
    else
      ret = strcat(ret, r);
  }

  log_smtp(debug:true, "smtp_recv_line: ", ret);
  return ret;
}


function smtp_recv_banner(socket)
{
  local_var b;

  b = smtp_recv_line(socket:socket, code:"220");

  return b;
}


#----------------------------------------------------------------#
# Function    : smtp_starttls                                    #
# Description : Sends a STARTTLS command to an open socket.      #
# Inputs      : 'socket' => an open socket. (required)           #
#               'dont_read_banner' => read the initial service   #
#                 banner unless set to true. (optional)          #
#               'encaps' => if specified, complete the SSL       #
#                 handshake using the type of encapsulation      #
#                 specified by 'encaps'. (optional)              #
#               'exit_on_fail' => if specified, exit on a        #
#                 failure rather than returning NULL.            #
# Return      : The socket if the connection could be upgraded   #
#               or NULL otherwise.                               #
# Notes       : Attempting to complete the SSL handshake will    #
#               always result in a failure if the function       #
#               'socket_negotiate_ssl()' is not defined.         #
# Usage       : if (smtp_starttls(socket:soc, encaps:encaps))    #
#               {                                                #
#                 send(socket:soc, data:...                      #
#----------------------------------------------------------------#
function smtp_starttls(socket, dont_read_banner, encaps, exit_on_fail)
{
  local_var data;

  if ( !dont_read_banner )
  {
    data = smtp_recv_banner(socket:socket);
    if ( !data )
    {
      log_smtp('SMTP receive banner failed');
      return NULL;
    }

    send(socket:socket, data:'HELO '+this_host()+'\r\n');
    data = smtp_recv_line(socket:socket);
    if(!ereg(pattern:"^[2-3][0-9][0-9]", string:data))
    {
      log_smtp("SMTP HELO failed: ", data);
      return NULL;
    }
  }

  send(socket:socket, data:'STARTTLS\r\n');
  data = smtp_recv_line(socket:socket);
  if ( strlen(data) < 4 ) 
  {
    log_smtp("The SMTP server did not send back a valid response to a STARTTLS command.");
    return NULL;
  }

  if ( substr(data, 0, 2) != "220" ) 
  {
    log_smtp("SMTP STARTTLS failed: ", data);
    return NULL;
  }

  if ( encaps )
  {
    if ( !defined_func("socket_negotiate_ssl") ) 
    {
      log_smtp("Failed to negotiate an SSL / TLS connection after sending a STARTTLS command.");
      return NULL;
    }

    return socket_negotiate_ssl(socket:socket, transport:encaps);
  }

  return socket;
}


