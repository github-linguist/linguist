#
# Josh Zlatin-Amishav (josh at ramat dot cc)
# GPLv2
#
# Changes by Tenable:
# - Revised plugin title (12/22/2008)
# - Revised description and added CPE (11/29/2012)

include("compat.inc");

if (description) {
  script_id(19693);
  script_bugtraq_id(12303);
  script_osvdb_id(13130);
  script_version("$Revision: 1.13 $");

  script_name(english:"CMSimple Guestbook Module index.php XSS");
 
 script_set_attribute(attribute:"synopsis", value:
"The remote web server is hosting a PHP application that is affected 
by a cross-site scripting vulnerability." );
 script_set_attribute(attribute:"description", value:
"The remote host is running CMSimple, a CMS written in PHP. 

The version of CMSimple installed on the remote host is prone to
cross-site scripting attacks due to its failure to sanitize
user-supplied input to both the search and guestbook modules." );
 script_set_attribute(attribute:"see_also", value:"http://securitytracker.com/alerts/2005/Jan/1012926.html" );
 script_set_attribute(attribute:"solution", value:
"Upgrade to version 2.4 Beta 5 or higher." );
 script_set_cvss_base_vector("CVSS2#AV:N/AC:M/Au:N/C:N/I:P/A:N");
 script_set_cvss_temporal_vector("CVSS2#E:H/RL:OF/RC:C");
 script_set_attribute(attribute:"exploitability_ease", value:"No exploit is required");
 script_set_attribute(attribute:"exploit_available", value:"true");
 script_cwe_id(20, 74, 79, 442, 629, 711, 712, 722, 725, 750, 751, 800, 801, 809, 811, 864, 900, 928, 931, 990);

 script_set_attribute(attribute:"plugin_publication_date", value: "2005/09/14");
 script_set_attribute(attribute:"vuln_publication_date", value: "2005/01/18");
 script_cvs_date("$Date: 2015/01/13 20:37:05 $");
script_set_attribute(attribute:"plugin_type", value:"remote");
script_set_attribute(attribute:"cpe", value:"cpe:/a:cmsimple:cmsimple");
script_end_attributes();

 
  summary["english"] = "Checks for XSS in guestbook module in index.php";
  script_summary(english:summary["english"]);
 
  script_category(ACT_DESTRUCTIVE_ATTACK);
  script_family(english:"CGI abuses : XSS");

  script_copyright(english:"(C) 2005-2015 Josh Zlatin-Amishav");

  script_dependencie("http_version.nasl", "cross_site_scripting.nasl");
  script_require_ports("Services/www", 80);
  script_exclude_keys("Settings/disable_cgi_scanning");
  script_require_keys("www/PHP");
  exit(0);
}

include("global_settings.inc");
include("http_func.inc");
include("http_keepalive.inc");
include("url_func.inc");

port = get_http_port(default:80);
if (!get_port_state(port)) exit(0);
if (!can_host_php(port:port)) exit(0);
if ( get_kb_item("www/"+port+"/generic_xss") ) exit(0);

xss = "<script>alert('" + SCRIPT_NAME + "');</script>";
exss = urlencode(str:xss);

foreach dir ( cgi_dirs() )
{
  req = http_get(
    item:string(
      dir, "/index.php?",
     "guestbook=", exss, 
     "&function=guestbook",
     "&action=save"
    ),
    port:port
  );
  res = http_keepalive_send_recv(port:port, data:req, bodyonly:TRUE);
  if (res == NULL) exit(0);

  # There's a problem if we see our XSS.
  if (
    xss >< res &&
    (
      egrep(string:res, pattern:'meta name="generator" content="CMSimple .+ cmsimple\\.dk') ||
      egrep(string:res, pattern:'href="http://www\\.cmsimple\\.dk/".+>Powered by CMSimple<') ||
      egrep(string:res, pattern:string('href="', dir, '/\\?&(sitemap|print)">'))
    )
  ) 
  {
    security_warning(port);
    set_kb_item(name: 'www/'+port+'/XSS', value: TRUE);
    exit(0);
  }
}
