/***************************************************
  Builds and sends the actual piwik tracking request
  
  Copyright (c) 2015 John Hatch
  Licenced under the MIT licence: http://opensource.org/licenses/MIT
 ***************************************************/

// Source - https://github.com/johnhatch14/GMS-Extensions/blob/master/PiwikAnalyticsForGMS/GMS_PiwikAnalytics.gmx/scripts/_piwikSendReq.gml

//Build argument map
   var args = ds_map_create();
   
   //-----
   // Populate GET arguments to Piwik HTTP API
   // See full HTTP API reference at http://developer.piwik.org/api-reference/tracking-api
   //-----
   
   //Required args
   ds_map_add(args, "idsite", string(_Piwik_idsite));
   ds_map_add(args, "rec", "1");
   //ds_map_add(args, "send_image", "0");
   ds_map_add(args, "url", _piwikUrlEncode(_Piwik_baseurl));
   
   //
   ds_map_add(args, "apiv", "1");
   ds_map_add(args, "_id", _piwikUrlEncode(_Piwik_id));
   ds_map_add(args, "rand", _piwikUrlEncode( string(round(random(999999999)+game_id)) ));
   
   //Pass local time to API
   var ctz = date_get_timezone();
   date_set_timezone(timezone_local);
   var now = date_current_datetime();
   ds_map_add(args, "h", _piwikUrlEncode(string(date_get_hour(now))));
   ds_map_add(args, "m", _piwikUrlEncode(string(date_get_minute(now))));
   ds_map_add(args, "s", _piwikUrlEncode(string(date_get_second(now))));
   date_set_timezone(ctz);
   
   //Pass local display resolution
   ds_map_add(args, "res", _piwikUrlEncode(string(display_get_width())+'x'+string(display_get_height())));
   
   //Pass local language
   ds_map_add(args, "lang", _piwikUrlEncode(os_get_language()));
   
   //Pass stored values if they exist
   if (_Piwik_idvc != -1)
      ds_map_add(args, "_idvc", _piwikUrlEncode(string(_Piwik_idvc)));
   if (_Piwik_idts != -1)
      ds_map_add(args, "_idts", _piwikUrlEncode(string(_Piwik_idts)));
   if (_Piwik_viewts != -1)
      ds_map_add(args, "_viewts", _piwikUrlEncode(string(_Piwik_viewts)));
   
   //-----
   // Add any other arguments passed to script in the form "param=value" to the http arg map
   //-----
   var arg_keyval;
   for (var i=0; i<argument_count; i++)
   {
    arg_keyval = _piwikStringExplode(argument[i],'=');
    ds_map_add(args, arg_keyval[0], _piwikUrlEncode(string(arg_keyval[1])));
   }

//Build argument string
   var argstring = "";
   var prevkey = ds_map_find_first(args);
   argstring += (prevkey + "=" + args[? prevkey] + "&");
   repeat (ds_map_size(args)-1)
   {
    prevkey = ds_map_find_next(args, prevkey);
    argstring += (prevkey + "=" + args[? prevkey] + "&");
   }
ds_map_destroy(args);

//Append query string to ds_list of requests to be sent at End Step. 
ds_list_add(_PIWIK_REQS, "?" + argstring);