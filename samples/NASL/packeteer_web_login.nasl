# nnposter
# GPL

# Changes by Tenable:
# - Revised plugin title (3/30/2009)

if(!defined_func("MD5")) exit(0);


include("compat.inc");

if (description)
    {
    script_id(25570);
    script_version ("$Revision: 1.5 $");
    script_cvs_date("$Date: 2011/03/14 21:48:09 $");

    name["english"]="Packeteer Web Management Interface Authentication";
    script_name(english:name["english"]);

 script_set_attribute(attribute:"synopsis", value:
"It is possible to log onto the remote web application." );
 script_set_attribute(attribute:"description", value:
"Nessus was able to log onto the remote Packeteer web management
interface with the given credentials and has stored the authentication
cookie in the KB for use with other plugins." );
 script_set_attribute(attribute:"risk_factor", value:"None" );
 script_set_attribute(attribute:"solution", value:"n/a" );
 script_set_attribute(attribute:"plugin_publication_date", value: "2007/06/26");
script_set_attribute(attribute:"plugin_type", value:"remote");
script_end_attributes();


    summary["english"]="Logs into Packeteer web management interface";
    script_summary(english:summary["english"]);

    family["english"]="CGI abuses";
    script_family(english:family["english"]);

    script_category(ACT_GATHER_INFO);
    script_copyright(english:"This script is Copyright (C) 2006-2011 nnposter");
    script_dependencies("logins.nasl","packeteer_web_detect.nasl");
    script_require_keys("www/packeteer","http/password");
    script_require_ports("Services/www",80);
    exit(0);
    }

# Notes:
# - logins.nasl will not process the HTTP password preference if the HTTP 
#   username is left blank. To compensate for this behavior this script assumes
#   that a username tha consists of a single non-alphanumeric character is not
#   really meant to be used.
# - Does not work with http_keepalive_send_recv() for some reason.
#   Resorting to http_send_recv()

include("global_settings.inc");
include("http_func.inc");
#include("http_keepalive.inc");


if (!get_kb_item("www/packeteer")) exit(0);
if (!get_kb_item("http/password")) exit(0);


function hex2str2()
{
local_var xlat,hs,s,i,j;
hs=_FCT_ANON_ARGS[0];
s="";
for (i=0; i<256; ++i) xlat[substr(hex(i),2)]=raw_string(i);
for (j=0; j<strlen(hs)/2; ++j) s+=xlat[substr(hs,2*j,2*j+1)];
return s;
}


port=get_http_port(default:80);
if (!get_tcp_port_state(port) || !get_kb_item("www/"+port+"/packeteer")) exit(0);

#resp=http_keepalive_send_recv(port:port,data:http_get(item:"/login.htm",port:port));
resp=http_send_recv(port:port,data:http_get(item:"/login.htm",port:port));
if (!resp) exit(0);

match=eregmatch(
    pattern:'challenge *= *"([0-9A-Fa-f]{32})".{1,80}chapID *= *"([0-9]*)"',
    string:resp);
challenge=match[1];
chapid=match[2];
if (!challenge || !chapid) exit(0);

authsrc=raw_string(int(chapid))
    +get_kb_item("http/password")
    +hex2str2(challenge);
response=hexstr(MD5(authsrc));

username=get_kb_item("http/login");
if (!strlen(username) || username=~"^[^a-z0-9]$") username="";

url="/Login?"
    + "LOGIN.CHALLENGE="+challenge
    +"&LOGIN.CHAPID="+chapid
    +"&LOGIN.RESPONSE="+response
    +"&LOGIN.USERNAME="+username;

#resp=http_keepalive_send_recv(port:port,data:http_get(item:url,port:port));
resp=http_send_recv(port:port,data:http_get(item:url,port:port));
if (!resp) exit(0);

cookie=egrep(pattern:"^Set-Cookie: *[^a-z0-9]PSpcV310=[0-9a-f]{32}",
             string:resp,
             icase:TRUE);
if (!cookie) exit(0);
cookie=ereg_replace(string:cookie,pattern:"^Set-",replace:"",icase:TRUE);

set_kb_item(name:"/tmp/http/auth/"+port,value:cookie);
if (report_verbosity > 1) security_note(port);
