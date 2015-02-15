package require http
set request [http::geturl "http://www.rosettacode.org"]
puts [http::data $request]
http::cleanup $request
