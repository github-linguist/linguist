#TRUSTED 1ecad1a72af07d5c7dc5d7e14779bfee77ceb3b40a5d630e0e91d6e142b3d0853c45660b7098d75fe8817238e26e502c686bd381d0d458069d4732bb7020e11e6f69df6cd814e4aa29e5c665b10858937b89f18f817a6be3568875c3d526b9280112ec4170f2bfbb99ae543ae2dbe4621435213d9f89c03717f10a3bf5780e0e19afd42894130b23ab55f2dcecb17d340299be01b79500392a805428d6475e915457616319d094ad0016e84d4a7dad59fb5716aab91224ee8e435d6019c057d8feefd33a0b43927f1a0eb818c07e54a53cde7fc761a6f043b940d675ce8c1a10ca3b2a6970db0b7aa4bde9098b2d49b00ce627691ce30f531211befbd0b6efa06155854e368219697a5e324e0ce47bcfba9ebb3d4087596372b981c61019177b996522f60623390692b4a2da64a0ab5471beff7ec5a3b830c4c76ce4e0d6386f674eb6d7d2cb6a4639ef04963285c1acf821be6e7045e3872b9b67dd447e413c159c6cc11ae30d4a3ce58dff2c961e14956dc6fc99cc6f60612b2a49323b2651f37629954d033d627651d244865d787df0148fe0a6a06b473836486d9bb70a0cb65111dd75326c703af7790f82c886d57a60bda2a8ec0381fb437ce01998f265e0a548a6aae53c696e8165ba7e0963b6058454c01cefac65b9049a652bd9ce6edd22a234f7b204ce7114b964ee593481ca6add3574cb1f8e671828e1d6cafa17
#
# This script was written by Michel Arboi <mikhail@nessus.org>
#
# and modified by airwofl (a@flumps.org) 29/5/06
# slight fixes by Vlatko Kosturjak <kost@linux.hr>
#
# GPL
#
# Last modification: 2009-05-19
#
#
# Nmap can be found at :
# <http://www.insecure.org/nmap/>
#
#


if (! defined_func("get_preference") ) exit(0);
if ( ! defined_func("pread") || ! find_in_path("nmap") ) no_exec = 1;

if(description)
{
 script_id(14259);
 script_version ("2.6");
 if ( no_exec == 1 )
  script_name(english: "Nmap Importer (NASL wrapper)");
 else
  script_name(english: "Nmap (NASL wrapper)");;

 desc = "
This plugin runs nmap(1) to find open ports.
See the section 'plugins options' to configure it

";

 if ( no_exec ) desc += '\n\n*** This plugin will only import the Nmap results';

 script_description(english:desc);
 script_summary(english: "Performs portscan / RPC scan");
 script_category(ACT_SCANNER);
 script_copyright(english:"This script is Copyright (C) 2004-2009 Michel Arboi");
 script_family(english: "Port scanners");

 if (NASL_LEVEL >= 3210)
  script_dependencies("portscanners_stub.nasl", "portscanners_settings.nasl");
 else
  script_dependencies("ping_host.nasl", "portscanners_settings.nasl");

 if (NASL_LEVEL < 2181) exit(0);	# Cannot run

 if (! no_exec)
 {
 v = pread(cmd: "nmap", argv: make_list("nmap", "-V"));
 if (v != NULL)
 {
  ver = ereg_replace(pattern: ".*nmap version ([0-9.]+).*", string: v, replace: "\1");
  if (ver == v) ver = NULL;
 }


 if (ver =~ "^[3-9]\.")
 script_add_preference(name:"TCP scanning technique :", type:"radio", 
  value:"connect();SYN scan;FIN scan;Xmas Tree scan;SYN FIN scan;FIN SYN scan;Null scan;No TCP Scanning");
 else
 script_add_preference(name:"TCP scanning technique :", type:"radio", 
  value:"connect();SYN scan;FIN scan;Xmas Tree scan;Null scan;No TCP Scanning");

 script_add_preference(name:"UDP port scan", type:"checkbox", value: "no");
 # This option eats too much memory and hits rlimits
 if (NASL_LEVEL > 2200)
  script_add_preference(name:"Service scan", type:"checkbox", value: "no");
 script_add_preference(name:"RPC port scan", type:"checkbox", value: "no");
 # Too much memory
 if (NASL_LEVEL > 2200)
 {
  script_add_preference(name:"Identify the remote OS", type:"checkbox", value: "no");
 }
 if (ver =~ "^3\.[0-5]") script_add_preference(name:"Get Identd info", type:"checkbox", value: "no");
 script_add_preference(name: "Source port :", value: "", type: "entry");
 script_add_preference(name:"Timing policy :", type:"radio",
  value: "Auto (nessus specific!);Normal;Insane;Aggressive;Polite;Sneaky;Paranoid;Custom");
 script_add_preference(name: "Initial RTT timeout (ms) :", value: "", type: "entry");
 script_add_preference(name: "Min RTT Timeout (ms) :", value: "", type: "entry");
 script_add_preference(name: "Max RTT Timeout (ms) :", value: "", type: "entry");
 script_add_preference(name: "Ports scanned in parallel (max)", value: "", type: "entry");
 script_add_preference(name: "Ports scanned in parallel (min)", value: "", type: "entry");
 script_add_preference(name: "Host Timeout (ms) :", value: "", type: "entry");
 script_add_preference(name: "Minimum wait between probes (ms)", value: "", type: "entry");
 }
 script_add_preference(name: "File containing grepable results : ", value: "", type: "file");
 if (! no_exec)
 {
 script_add_preference(name: 'Do not scan targets not in the file', value: 'no', type: 'checkbox');
 if (ver =~ "^3\.")
 script_add_preference(name: "Data length : ", value: "", type: "entry");
 script_add_preference(name: "Run dangerous port scans even if safe checks are set", value:"no", type:"checkbox");
 if (ver =~ "^[4-9]\.")
 script_add_preference(name: "Number of port retries (max)", value: "", type: "entry");
 }
 exit(0);
}

#
if (NASL_LEVEL < 2181 || ! defined_func("get_preference"))
{
  set_kb_item(name: "/tmp/UnableToRun/14259", value: TRUE);
  display("Script #14259 (nmap.nasl) cannot run - upgrade libnasl\n");
  exit(0);
}

if ( ! defined_func("pread") || ! find_in_path("nmap") ) no_exec = 1;

tmpfile = NULL;

function on_exit()
{
  if (tmpfile) unlink(tmpfile);
}

function compute_rtt()
{
  local_var	p, i, min, max, s, t1, t2, ms, v1, v2;

  min = 10000; 
  max = 0;

  foreach p (make_list(80, 113))
   for (i = 0; i < 3; i ++)
   {
    t1 = gettimeofday();
    s = open_sock_tcp(p, timeout: 10, transport: ENCAPS_IP);
    t2 = gettimeofday();
    if (s) close(s);
    v1 = eregmatch(string: t1, pattern: "([0-9]+)\.([0-9]+)");
    v2 = eregmatch(string: t2, pattern: "([0-9]+)\.([0-9]+)");
    ms = (int(v2[1]) - int(v1[1])) * 1000 + (int(v2[2]) - int(v1[2])) / 1000;
    if (ms > 9000 && !s) break;
    if (min > ms) min = ms;
    if (max < ms) max = ms;
   }
  if (max < 3 * min) max = 3 * min;
  if (min >= 9000) min = 0;
  if (max && max < 6) max = 6;
  if (max >= 9000) max = 0;
  if (isnull(min) && isnull(max)) return NULL;
  return make_list(min, max);
}

if (! no_exec)
{
 safe_opt = script_get_preference("Run dangerous port scans even if safe checks are set");
 if ( safe_opt && "yes" >< safe_opt ) safe = 0;
 else safe = safe_checks();
}

ip = get_host_ip();
esc_ip = ""; l = strlen(ip);
for (i = 0; i < l; i ++) 
  if (ip[i] == '.') esc_ip = strcat(esc_ip, "\.");
  else esc_ip = strcat(esc_ip, ip[i]);

res = script_get_preference_file_content("File containing grepable results : ");

if (! egrep(pattern: "Host: +" + esc_ip + " ", string: res))
{
 if (no_exec) exit(0);
 if (get_kb_item("PortscannersSettings/run_only_if_needed")
     && get_kb_item("Host/full_scan")) exit(0);
 opt = script_get_preference('Do not scan targets not in the file');
 if ('yes' >< opt) exit(0);

 i = 0;
 argv[i++] = "nmap";
 argv[i++] = "-n";
 argv[i++] = "-P0";	# Nmap ping is not reliable
 argv[i++] = "-oG";
 if (defined_func("get_tmp_dir"))
 {
  tmpdir = get_tmp_dir();
  if (tmpdir) tmpfile = strcat(tmpdir, "/nmap-", get_host_ip(), "-", rand() );
 }
 if (tmpfile)
  argv[i++] = tmpfile;
 else
 argv[i++] = "-";

 p = script_get_preference("TCP scanning technique :");
 scan_tcp=0;
 scan_udp=0;
 # Force TCP scan in safe mode - other options could crash the IP stack
 # TCP scan is more aggressive against broken services than SYN scan,
 # but those services will probably be killed by fin_service or similar
 # tests.
 if (p != "No TCP Scanning")
 {
   scan_tcp = 1;
   if (safe) argv[i++] = "-sT";
   else if (p == "SYN scan" || p == "SYN FIN scan") argv[i++] = "-sS";
   else if (p == "FIN scan" || p == "FIN SYN scan") argv[i++] = "-sF";
   else if (p == "Xmas Tree scan") argv[i++] = "-sX";
   else if (p == "Null scan") argv[i++] = "-sN";
   else argv[i++] = "-sT";
   if (p == "FIN SYN scan" || p == "SYN FIN scan")
   {
     argv[i++] = "--scanflags";
     argv[i++] = "SYNFIN";
   }
 }

 # UDP & RPC scans or fingerprinting may kill a buggy IP stack
 if (! safe)
 {
  p = script_get_preference("UDP port scan");
  if ("yes" >< p)
  {
   # Newer versions of Nmap only print 25 open UDP ports 
   # and add 25 port for each -v.  I don't know exactly what "newer" versions 
   # are affected by this bug^W feature.
   argv[i++] = "-vvvvvvvvvvvvvvvvvvvvvvvvv";	# 26*25 ports
   argv[i++] = "-sU";
   scan_udp = 1;
  }
  p = script_get_preference("Service scan");
  if ("yes" >< p) argv[i++] = "-sV";
  p = script_get_preference("RPC port scan");
  if ("yes" >< p) argv[i++] = "-sR";
  p = script_get_preference("Identify the remote OS");
  if ("yes" >< p) argv[i++] = "-O";
 }
 p = script_get_preference("Get Identd info");
 if ("yes" >< p) argv[i++] = "-I";
 port_range = get_preference("port_range");
 if (port_range) # Null for command line tests only
 {
  argv[i++] = "-p";
  if (port_range == "default" )
  {
   n = 0;
   str = "";
   str2 = "";
   while ( port = scanner_get_port(n) )
   {
    if ( n > 0 )
    {
     str += "," + string(port);
     if (ver =~ "^[4-9]\.") str2 += "," + string(port);
    }
    else
    {
     if (ver =~ "^[4-9]\.")
     {
      str = "T:" + string(port);
      str2 = "U:" + string(port);
     }
     else str = string(port);
    }
    n ++;
   }
   if ( scan_tcp == 0 ) str = NULL;
   if ( scan_udp == 0 ) str2 = NULL;
  
   if ( isnull(str) && isnull(str2) ) exit(0); # ???
   argv[i++] = strcat ( str , str2 );
  }
  else
  {
   tmp_port_range = NULL;
   if ( ver =~ "^[4-9]\." )
   {
    if (scan_tcp) tmp_port_range = "T:" + port_range;
    if ( strlen(tmp_port_range) ) tmp_port_range += ",";
    if (scan_udp) tmp_port_range += "U:" + port_range;
   }
   else tmp_port_range = port_range;

   argv[i++] = tmp_port_range;
  }
 }
 
 p = script_get_preference("Source port :");
 if (p =~ '^[0-9]+$') { argv[i++] = "-g"; argv[i++] = p; }

 # We should check the values when running in "safe checks".
 custom_policy = 0;
 p = script_get_preference("Host Timeout (ms) :");
 if (p =~ '^[0-9]+$')
 {
   argv[i++] = "--host_timeout";
   argv[i++] = p;
   custom_policy ++;
 }
 p = script_get_preference("Min RTT Timeout (ms) :");
 if (p =~ '^[0-9]+$')
 {
   argv[i++] = "--min_rtt_timeout";
   argv[i++] = p;
   custom_policy ++;
 }
 p = script_get_preference("Max RTT Timeout (ms) :");
 if (p =~ '^[0-9]+$')
 {
   argv[i++] = "--max_rtt_timeout";
   argv[i++] = p;
   custom_policy ++;
 }
 p = script_get_preference("Initial RTT timeout (ms) :");
 if (p =~ '^[0-9]+$')
 {
   argv[i++] = "--initial_rtt_timeout";
   argv[i++] = p;
   custom_policy ++;
 }
 min = 1;
 p = script_get_preference("Ports scanned in parallel (min)");
 if (p =~ '^[0-9]+$')
 {
   argv[i++] = "--min_parallelism";
   argv[i++] = p;
   min = p;
   custom_policy ++;
 }
 p = script_get_preference("Ports scanned in parallel (max)");
 if (p =~ '^[0-9]+$')
 {
   argv[i++] = "--max_parallelism";
   if (p < min) p = min;
   argv[i++] = p;
   custom_policy ++;
 }

 p = script_get_preference("Number of port retries (max)");
 if (p =~ '^[0-9]+$')
 {
   argv[i++] = "--max-retries";
   argv[i++] = p;
   custom_policy ++;
 }

 p = script_get_preference("Minimum wait between probes (ms)");
 if (p =~ '^[0-9]+$')
 {
   argv[i++] = "--scan_delay";
   argv[i++] = p;
   custom_policy ++;
 }

 if (! custom_policy)
 {
   p = script_get_preference("Timing policy :");
   if ( ! p ) p = "Normal";
   if ("Auto" >< p)
   {
      rtt = compute_rtt();
      if (! isnull(rtt))
      {
        minrtt = rtt[0];
        maxrtt = rtt[1];
        if (minrtt)
        {
         argv[i++] = "--min_rtt_timeout";
         argv[i++] = minrtt;
        }
        if (maxrtt)
        {
         argv[i++] = "--max_rtt_timeout";
         argv[i++] = maxrtt;
        }
      }
      # otherwise, use "Normal" timing
    }
    else if (p != "Normal")
    {
     argv[i++] = "-T";
     # Disable aggresive timings in safe checks
     if (safe && ("Insane">< p || "Aggressive" >< p)) argv[i++] = "Normal";
     else argv[i++] = p;
    }
 }

 p = script_get_preference("Data length : ");
 if (p =~ '^[0-9]+$')
 {
   argv[i++] = "--data_length";
   argv[i++] = p;
   custom_policy ++;
 }

 argv[i++] = ip;

scanner_status(current: 0, total: 65535);

 res = pread(cmd: "nmap", argv: argv, cd: 1);
 if (tmpfile)
  res = fread(tmpfile);
# display(argv, "\n", res, "\n\n");
 if (! res) exit(0);	# error
}

if (egrep(string: res, pattern: '^# +Ports scanned: +TCP\\(65535;'))
  full_scan = 1;
else
 full_scan = 0;

res = egrep(pattern: "Host: +" + esc_ip + " ", string: res);
if (! res)
{
 # set_kb_item(name: "Host/ping_failed", value: "yes");
 exit(0);
}

res = ereg_replace(pattern: 'Host: +[0-9.]+ .*[ \t]+Ports: +',
	string: res, replace: "");
# Fields:
# port_nb/state/protocol/owner/port_name/rpc_name/service/
# Example:
# Host: 127.0.0.1 ()      Ports: 111/open/tcp/bin/rpcbind (rpcbind V2)/(rpcbind:100000*2-2)/2 (rpc #100000)/, 111/open/udp//rpcbind (rpcbind V2)/(rpcbind:100000*2-2)/2 (rpc #100000)/, 113/open/tcp/root/ident //Linux-identd/, 119/open/tcp/root/nntp //Leafnode NNTPd 1.9.49.rel/, 123/open/udp//ntp ///, 137/open/udp//netbios-ns //Samba nmbd (host: CASSEROLE workgroup: MAISON)/, 138/open/udp//netbios-dgm ///, 139/open/tcp/root/netbios-ssn //Samba smbd 3.X (workgroup: MAISON)/      Ignored State: closed (194)

scanned = 0; udp_scanned = 0; ident_scanned = 0;
foreach blob(split(res, sep: ',', keep:0))
{
  v = eregmatch(string: blob, icase: 1, 
  pattern: "^(Host: .*:)? *([0-9]+)/([^/]+)/([^/]+)/([^/]*)/([^/]*)/([^/]*)/([^/]*)/");
  if (! isnull(v))
  {
   port = v[2];
   status = v[3];
   proto = v[4];
   owner = v[5];
   svc = v[6];
   rpc = v[7];
   ver = v[8];
# display("port=", port, "\nstatus=", status, "\nproto=", proto, "\nsvc=", svc, "\nowner=", owner, "\nrpc=", rpc, "\nver=", ver, "\n\n");
   if ("open" >< status)	# nmap 3.70 says "open|filtered" on UDP
     scanner_add_port(proto: proto, port: port);
    if (owner)
    {
      security_note(port: port, proto: proto, data: "This service is owned by user "+owner);
      set_kb_item(name: "Ident/"+proto+"/"+port, value: owner);
      ident_scanned ++;
    }
    scanned ++;
    if (proto == "udp") udp_scanned ++;
    if (strlen(rpc) > 1)
    {
      r = ereg_replace(string: rpc, pattern: "\(([^:]+):.+\)", replace: "\1");
      if (! r) r = rpc;
      security_note(port: port, proto: proto, 
       data: "The RPC service "+r+" is running on this port
If you do not use it, disable it, as it is
a potential security risk");
    }
    if (ver)
    {
      ver = ereg_replace(pattern: "^([0-9-]+) +\((.+)\)$", string: ver,
	replace: "\2 V\1");
      security_note(port: port, proto: proto, data: "Nmap has identified this service as " + ver);
      set_kb_item(name: 'Nmap/'+proto+'/'+port+'/version', value: ver);
      if (string !~ "\?$")
       set_kb_item(name: 'Nmap/'+proto+'/'+port+'/svc', value: svc);
      # set_kb_item(name: "NmapSvc/"+port, value: svc);
    }
 }
}

v = eregmatch(string: res, pattern: 'OS: (.+)[ \t]+[A-Za-z]+:');
if (! isnull(v))
{
  security_note(port: 0, data: "Nmap found that this host is running "+v[1]);
  set_kb_item(name: "Host/OS/nmap", value: v[1]);
}

v = eregmatch(string: res, pattern: 'Seq Index: ([^\t]+)');
if (! isnull(v))
{
  idx = int(v[1]);
  if (idx == 9999999)
  {
    security_note(port: 0, data: "The TCP initial sequence number of the remote host look truly random. 
Excellent!");
    set_kb_item(name: "Host/tcp_seq", value: "random");
   }
  else if (idx == 0)
  {
    security_hole(port: 0, data: 
"The TCP sequence numbers of the remote host are
constant ! A cracker may use this flaw to spoof TCP connections
easily.

Solution : contact your vendor for a patch
Risk factor : High");
    set_kb_item(name: "Host/tcp_seq", value: "constant");
  }
  else if (idx == 1)
  {
    security_hole(port: 0, data: 
"The TCP sequence numbers of the remote host are
always incremented by 64000, so they can be
guessed rather easily. A cracker may use
this flaw to spoof TCP connections easily.

Solution : contact your vendor for a patch
Risk factor : High");
    set_kb_item(name: "Host/tcp_seq", value: "64000");
  }
  else if (idx == 10)
  {
    security_hole(port: 0, data: 
"The TCP sequence numbers of the remote host are
always incremented by 800, so they can be
guessed rather easily. A cracker may use
this flaw to spoof TCP connections easily.

Solution : contact your vendor for a patch
Risk factor : High");
    set_kb_item(name: "Host/tcp_seq", value: "800");
  }
  else if (idx < 75)
  {
    security_hole(port: 0, data: 
"The TCP sequence numbers of the remote host
depends on the time, so they can be
guessed rather easily. A cracker may use
this flaw to spoof TCP connections easily.

Solution : http://www.microsoft.com/technet/security/bulletin/ms99-046.asp
Risk factor : High ");
    set_kb_item(name: "Host/tcp_seq", value: "time");
  }
  else
  {
    security_note(port: 0, data: "The TCP initial sequence number of the remote host are incremented by random positive values. 
Good!");
    set_kb_item(name: "Host/tcp_seq", value: "random");
   }
  set_kb_item(name: "Host/tcp_seq_idx", value: v[1]);
}

v = eregmatch(string: res, pattern: 'IPID Seq: ([^\t]+)');
if (! isnull(v))
  security_note(port: 0, data: "the IP ID sequence generation is: " + v[1]);

if (scanned)
{
 set_kb_item(name: "Host/scanned", value: TRUE);
 set_kb_item(name: 'Host/scanners/nmap', value: TRUE);
}
if (udp_scanned) set_kb_item(name: "Host/udp_scanned", value: TRUE);
if (full_scan)
{
 if (ident_scanned) set_kb_item(name: "Host/ident_scanned", value: TRUE);
 set_kb_item(name: "Host/full_scan", value: TRUE);
}

scanner_status(current: 65535, total: 65535);