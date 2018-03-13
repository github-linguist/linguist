// Source - https://github.com/johnhatch14/GMS-Extensions/blob/master/PiwikAnalyticsForGMS/GMS_PiwikAnalytics.gmx/scripts/_piwikCacheRequest.gml

var jsonMap, fh;

var otz = date_get_timezone();
date_set_timezone(timezone_utc);
var requestToCache = argument0 + "cdt="+_piwikUrlEncode(string(current_year)+"-"+
                                 string(current_month)+"-"+
                                 string(current_day)+" "+
                                 string(current_hour)+":"+
                                 string(current_minute)+":"+
                                 string(current_second));
date_set_timezone(otz);

if (file_exists(_Piwik_CacheFile))
{
 //Verify cache signature to make sure no unwanted (heaven-forbid, malicious) requests have been added.
 var curCacheSig = sha1_string_utf8(sha1_file(_Piwik_CacheFile) + "v5T7uAXnpQ3BGKq" + string(game_id+2563542));
 ini_open(_Piwik_IniFile);
 var storedSig = ini_read_string("cache", "sig", "NULL");
 ini_close();
 if (string_count(storedSig, curCacheSig) == 1)
 {
    fh = file_text_open_read(_Piwik_CacheFile);
    var cachedJson = base64_decode(file_text_read_string(fh));
    file_text_close(fh);
    
    jsonMap = json_decode(cachedJson);
    
    if (!ds_exists(jsonMap, ds_type_map))
       jsonMap = ds_map_create();
 }
 else
 {
  if (_PiwikDebugOutput)
     show_debug_message("Piwik Error: Signature mis-match. LOCAL REQUEST-CACHE HAS BEEN TAMPERED WITH! The ninjas are here somewhere...");
  jsonMap = ds_map_create();//Start with a fresh cache since the old one is corrupted. An ounce of lost analytics is worth a pound of security.
 }
}
else
{
 jsonMap = ds_map_create();
}

if (!is_undefined(jsonMap[? "requests"]))
{
 ds_list_add(jsonMap[? "requests"], requestToCache);
}
else
{
 var requestList = ds_list_create();
 ds_list_add(requestList, requestToCache);
 
 ds_map_add_list(jsonMap, "requests", requestList);
}

var newCachedJson = json_encode(jsonMap);
ds_map_destroy(jsonMap);

fh = file_text_open_write(_Piwik_CacheFile);
file_text_write_string(fh, base64_encode(newCachedJson));
file_text_close(fh);

var cacheSig = sha1_string_utf8(sha1_file(_Piwik_CacheFile) + "v5T7uAXnpQ3BGKq" + string(game_id+2563542));

ini_open(_Piwik_IniFile);
ini_write_string("cache", "sig", cacheSig);
ini_close();