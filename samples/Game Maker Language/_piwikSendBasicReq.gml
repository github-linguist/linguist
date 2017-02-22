/***************************************************
  Builds and sends the actual piwik tracking request
  
  Copyright (c) 2015 John Hatch
  Licenced under the MIT licence: http://opensource.org/licenses/MIT
 ***************************************************/

// Source - https://github.com/johnhatch14/GMS-Extensions/blob/master/PiwikAnalyticsForGMS/GMS_PiwikAnalytics.gmx/scripts/_piwikSendBasicReq.gml

//Build argument map
   var args = ds_map_create();
   
   //-----
   // Populate GET arguments to Piwik HTTP API
   // See full HTTP API reference at http://developer.piwik.org/api-reference/tracking-api
   //-----
   
   //Required args
   ds_map_add(args, "idsite", string(_Piwik_idsite));
   ds_map_add(args, "rec", "1");
   ds_map_add(args, "url", _piwikUrlEncode(_Piwik_baseurl + "/" + room_get_name(room)));
   ds_map_add(args, "apiv", "1");
   ds_map_add(args, "_id", _piwikUrlEncode(_Piwik_id));
   ds_map_add(args, "rand", _piwikUrlEncode( string(round(random(999999999)+game_id)) ));
   //ds_map_add(args, "new_visit", "0");
   
   //Pass local time to API
   var ctz = date_get_timezone();
   date_set_timezone(timezone_local);
   var now = date_current_datetime();
   ds_map_add(args, "h", _piwikUrlEncode(string(date_get_hour(now))));
   ds_map_add(args, "m", _piwikUrlEncode(string(date_get_minute(now))));
   ds_map_add(args, "s", _piwikUrlEncode(string(date_get_second(now))));
   date_set_timezone(ctz);
   
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