# jun/19/2022 23:33:22 by RouterOS 6.49.6
# software id = 9ASF-9GB5
#
                script: #| Welcome to RouterOS!
                        #|    1) Set a strong router password in the System > Users menu
                        #|    2) Upgrade the software in the System > Packages menu
                        #|    3) Enable firewall on untrusted networks
                        #| -----------------------------------------------------------------------------
                        #| RouterMode:
                        #|  * WAN port is protected by firewall and enabled DHCP client
                        #|  * Ethernet interfaces (except WAN port/s) are part of LAN bridge
                        #| LAN Configuration:
                        #|     IP address 192.168.88.1/24 is set on bridge (LAN port)
                        #|     DHCP Server: enabled;
                        #|     DNS: enabled;
                        #| WAN (gateway) Configuration:
                        #|     gateway:  ether1 ;
                        #|     ip4 firewall:  enabled;
                        #|     NAT:   enabled;
                        #|     DHCP Client: enabled;
                        #| Login
                        #|     admin user protected by password
                        
                        :global defconfMode;
                        :log info "Starting defconf script";
                        #-------------------------------------------------------------------------------
                        # Apply configuration.
                        # these commands are executed after installation or configuration reset
                        #-------------------------------------------------------------------------------
                        :if ($action = "apply") do={
                          # wait for interfaces
                          :local count 0;
                          :while ([/interface ethernet find] = "") do={
                            :if ($count = 30) do={
                              :log warning "DefConf: Unable to find ethernet interfaces";
                              /quit;
                            }
                            :delay 1s; :set count ($count +1); 
                          };
                         /interface list add name=WAN comment="defconf"
                         /interface list add name=LAN comment="defconf"
                         /interface bridge
                           add name=bridge disabled=no auto-mac=yes protocol-mode=rstp comment=defconf;
                         :local bMACIsSet 0;
                         :foreach k in=[/interface find where !(slave=yes   || name="ether1" || name~"bridge")] do={
                           :local tmpPortName [/interface get $k name];
                           :if ($bMACIsSet = 0) do={
                             :if ([/interface get $k type] = "ether") do={
                               /interface bridge set "bridge" auto-mac=no admin-mac=[/interface get $tmpPortName mac-address];
                               :set bMACIsSet 1;
                             }
                           }
                             :if (([/interface get $k type] != "ppp-out") && ([/interface get $k type] != "lte")) do={
                               /interface bridge port
                                 add bridge=bridge interface=$tmpPortName comment=defconf;
                             }
                           }
                           /ip pool add name="default-dhcp" ranges=192.168.88.10-192.168.88.254;
                           /ip dhcp-server
                             add name=defconf address-pool="default-dhcp" interface=bridge lease-time=10m disabled=no;
                           /ip dhcp-server network
                             add address=192.168.88.0/24 gateway=192.168.88.1 dns-server=192.168.88.1 comment="defconf";
                          /ip address add address=192.168.88.1/24 interface=bridge comment="defconf";
                         /ip dns {
                             set allow-remote-requests=yes
                             static add name=router.lan address=192.168.88.1 comment=defconf
                         }
                        
                           /ip dhcp-client add interface=ether1 disabled=no comment="defconf";
                         /interface list member add list=LAN interface=bridge comment="defconf"
                         /interface list member add list=WAN interface=ether1 comment="defconf"
                         /ip firewall nat add chain=srcnat out-interface-list=WAN ipsec-policy=out,none action=masquerade comment="defconf: masquerade"
                         /ip firewall {
                           filter add chain=input action=accept connection-state=established,related,untracked comment="defconf: accept established,related,untracked"
                           filter add chain=input action=drop connection-state=invalid comment="defconf: drop invalid"
                           filter add chain=input action=accept protocol=icmp comment="defconf: accept ICMP"
                           filter add chain=input action=accept dst-address=127.0.0.1 comment="defconf: accept to local loopback (for CAPsMAN)"
                           filter add chain=input action=drop in-interface-list=!LAN comment="defconf: drop all not coming from LAN"
                           filter add chain=forward action=accept ipsec-policy=in,ipsec comment="defconf: accept in ipsec policy"
                           filter add chain=forward action=accept ipsec-policy=out,ipsec comment="defconf: accept out ipsec policy"
                           filter add chain=forward action=fasttrack-connection connection-state=established,related comment="defconf: fasttrack"
                           filter add chain=forward action=accept connection-state=established,related,untracked comment="defconf: accept established,related, untracked"
                           filter add chain=forward action=drop connection-state=invalid comment="defconf: drop invalid"
                           filter add chain=forward action=drop connection-state=new connection-nat-state=!dstnat in-interface-list=WAN comment="defconf: drop all from WAN not DSTNATed"
                         }
                           /ip neighbor discovery-settings set discover-interface-list=LAN
                           /tool mac-server set allowed-interface-list=LAN
                           /tool mac-server mac-winbox set allowed-interface-list=LAN
                         :if (!($defconfPassword = "" || $defconfPassword = nil)) do={
                           /user set admin password=$defconfPassword
                         }
                        }
                        #-------------------------------------------------------------------------------
                        # Revert configuration.
                        # these commands are executed if user requests to remove default configuration
                        #-------------------------------------------------------------------------------
                        :if ($action = "revert") do={
                        /user set admin password=""
                         /system routerboard mode-button set enabled=no
                         /system routerboard mode-button set on-event=""
                         /system script remove [find comment~"defconf"]
                         /ip firewall filter remove [find comment~"defconf"]
                         /ip firewall nat remove [find comment~"defconf"]
                         /interface list member remove [find comment~"defconf"]
                         /interface detect-internet set detect-interface-list=none
                         /interface detect-internet set lan-interface-list=none
                         /interface detect-internet set wan-interface-list=none
                         /interface detect-internet set internet-interface-list=none
                         /interface list remove [find comment~"defconf"]
                         /tool mac-server set allowed-interface-list=all
                         /tool mac-server mac-winbox set allowed-interface-list=all
                         /ip neighbor discovery-settings set discover-interface-list=!dynamic
                           :local o [/ip dhcp-server network find comment="defconf"]
                           :if ([:len $o] != 0) do={ /ip dhcp-server network remove $o }
                           :local o [/ip dhcp-server find name="defconf" !disabled]
                           :if ([:len $o] != 0) do={ /ip dhcp-server remove $o }
                           /ip pool {
                             :local o [find name="default-dhcp" ranges=192.168.88.10-192.168.88.254]
                             :if ([:len $o] != 0) do={ remove $o }
                           }
                           :local o [/ip dhcp-client find comment="defconf"]
                           :if ([:len $o] != 0) do={ /ip dhcp-client remove $o }
                         /ip dns {
                           set allow-remote-requests=no
                           :local o [static find comment="defconf"]
                           :if ([:len $o] != 0) do={ static remove $o }
                         }
                         /ip address {
                           :local o [find comment="defconf"]
                           :if ([:len $o] != 0) do={ remove $o }
                         }
                         :foreach iface in=[/interface ethernet find] do={
                           /interface ethernet set $iface name=[get $iface default-name]
                         }
                         /interface bridge port remove [find comment="defconf"]
                         /interface bridge remove [find comment="defconf"]
                         /interface bonding remove [find comment="defconf"]
                         /interface wireless cap set enabled=no interfaces="" caps-man-addresses=""
                          /caps-man manager set enabled=no
                          /caps-man manager interface remove [find comment="defconf"]
                          /caps-man manager interface set [ find default=yes ] forbid=no
                          /caps-man provisioning remove [find comment="defconf"]
                          /caps-man configuration remove [find comment="defconf"]
                          /caps-man security remove [find comment="defconf"]
                        }
                        :log info Defconf_script_finished;
                        :set defconfMode;
                        
      caps-mode-script: #-------------------------------------------------------------------------------
                        # Note: script will not execute at all (will throw a syntax error) if
                        #       dhcp or wireless-fp packages are not installed
                        #-------------------------------------------------------------------------------
                        
                        #| CAP configuration
                        #|
                        #|   Wireless interfaces are set to be managed by CAPsMAN.
                        #|   All ethernet interfaces and CAPsMAN managed interfaces are bridged.
                        #|   DHCP client is set on bridge interface.
                        
                        # bridge port name
                        :global brName  "bridgeLocal";
                        :global logPref "defconf:";
                        
                        
                        :global action;
                        
                        :log info $action
                        
                        :if ($action = "apply") do={
                        
                          # wait for ethernet interfaces
                          :local count 0;
                          :while ([/interface ethernet find] = "") do={
                            :if ($count = 30) do={
                              :log warning "DefConf: Unable to find ethernet interfaces";
                              /quit;
                            }
                            :delay 1s; :set count ($count + 1);
                          }
                        
                          :local macSet 0;
                          :local tmpMac "";
                        
                          :foreach k in=[/interface ethernet find] do={
                            # first ethernet is found; add bridge and set mac address of the ethernet port
                            :if ($macSet = 0) do={
                              :set tmpMac [/interface ethernet get $k mac-address];
                              /interface bridge add name=$brName auto-mac=no admin-mac=$tmpMac comment="defconf";
                              :set macSet 1;
                            }
                            # add bridge ports
                            /interface bridge port add bridge=$brName interface=$k comment="defconf"
                          }
                        
                          # try to add dhcp client on bridge interface (may fail if already exist)
                          :do {
                            /ip dhcp-client add interface=$brName disabled=no comment="defconf"
                          } on-error={ :log warning "$logPref unable to add dhcp client";}
                        
                        
                          # try to configure caps (may fail if for example specified interfaces are missing)
                          :local interfacesList "";
                          :local bFirst 1;
                        
                          # wait for wireless interfaces
                          :while ([/interface wireless find] = "") do={
                            :if ($count = 30) do={
                              :log warning "DefConf: Unable to find wireless interfaces";
                              /quit;
                            }
                            :delay 1s; :set count ($count + 1);
                          }
                        
                          # delay just to make sure that all wireless interfaces are loaded
                          :delay 5s;
                          :foreach i in=[/interface wireless find] do={
                            if ($bFirst = 1) do={
                              :set interfacesList [/interface wireless get $i name];
                              :set bFirst 0;
                            } else={
                              :set interfacesList "$interfacesList,$[/interface wireless get $i name]";
                            }
                          }
                          :do {
                            /interface wireless cap
                              set enabled=yes interfaces=$interfacesList discovery-interfaces=$brName bridge=$brName
                          } on-error={ :log warning "$logPref unable to configure caps";}
                        
                        }
                        
                        :if ($action = "revert") do={
                          :do {
                            /interface wireless cap
                              set enabled=no interfaces="" discovery-interfaces="" bridge=none
                          } on-error={ :log warning "$logPref unable to unset caps";}
                        
                          :local o [/ip dhcp-client find comment="defconf"]
                          :if ([:len $o] != 0) do={ /ip dhcp-client remove $o }
                        
                          /interface bridge port remove [find comment="defconf"]
                          /interface bridge remove [find comment="defconf"]
                        
                        }
  wps-sync-mode-script: #-------------------------------------------------------------------------------
                        # Note: script will not execute at all (will throw a syntax error) if
                        #       dhcp or wireless-fp packages are not installed
                        #-------------------------------------------------------------------------------
                        
                        #| WPS Sync Configuration:
                        #|  * Wireless and ethernet interfaces bridged with enabled DHCP client
                        #|  * wlan1/2 access points and wlan3 wps sync repeater
                        #| Wireless: CAP enabled on wlan1/2
                        #| WPS Sync:
                        #|     mode:          repeater;
                        #| LAN Configuration:
                        #|     DHCP client: enabled;
                        
                        
                        # bridge port name
                        :global brName  "bridgeLocal";
                        :global logPref "defconf:";
                        :global ssid;
                        
                        :global action;
                        
                        :log info $action
                        
                        :if ($action = "apply") do={
                        
                          # wait for ethernet interfaces
                          :local count 0;
                          :while ([/interface ethernet find] = "") do={
                            :if ($count = 30) do={
                              :log warning "DefConf: Unable to find ethernet interfaces";
                              /quit;
                            }
                            :delay 1s; :set count ($count + 1);
                          }
                        
                          :local macSet 0;
                          :local tmpMac "";
                        
                          :foreach k in=[/interface ethernet find] do={
                            # first ethernet is found; add bridge and set mac address of the ethernet port
                            :if ($macSet = 0) do={
                              :set tmpMac [/interface ethernet get $k mac-address];
                              /interface bridge add name=$brName auto-mac=no admin-mac=$tmpMac comment="defconf";
                              :set macSet 1;
                            }
                            # add bridge ports
                            /interface bridge port add bridge=$brName interface=$k comment="defconf"
                          }
                        
                          # try to add dhcp client on bridge interface (may fail if already exist)
                          :do {
                            /ip dhcp-client add interface=$brName disabled=no comment="defconf"
                          } on-error={ :log warning "$logPref unable to add dhcp client";}
                        
                        
                          :local count 0;
                          :while ([/interface wireless print count-only] < 3) do={
                            :set count ($count +1);
                            :if ($count = 40) do={
                              :log warning "DefConf: Unable to find wireless interfaces";
                              /ip address add address=192.168.88.1/24 interface=ether1 comment="defconf";
                              /quit
                            }
                            :delay 1s;
                          };
                        
                          :foreach k in=[/interface wireless find] do={
                            # add bridge ports
                            /interface bridge port add bridge=$brName interface=$k comment="defconf"
                          }
                        
                          :local hwInfo [/interface wireless info hw-info [.. find where default-name="wlan3"] as-value];
                          #:if (($hwInfo->"locked-countries")~"russia") do={
                            /interface wireless set [find where default-name="wlan3"] channel-width=20/40mhz-XX band=5ghz-a/n/ac
                          #} else={
                          #  /interface wireless set [find where default-name="wlan3"] channel-width=20/40/80mhz-XXXX band=5ghz-a/n/ac
                          #}
                        
                          /interface wireless cap
                            set discovery-interfaces=bridgeLocal enabled=yes interfaces=wlan1,wlan2
                          /interface wireless
                            setup-repeater wlan3 duration=2m
                        }
                        
                        :if ($action = "revert") do={
                          :local o [/ip dhcp-client find comment="defconf"]
                          :if ([:len $o] != 0) do={ /ip dhcp-client remove $o }
                        
                          /interface bridge port remove [find comment="defconf"]
                          /interface bridge remove [find comment="defconf"]
                        
                          /interface wireless cap set enabled=no interfaces="" discovery-interfaces=""
                          /interface wireless reset-configuration wlan1
                          /interface wireless reset-configuration wlan2
                          /interface wireless reset-configuration wlan3
                        
                        
                        }
                        
                        :set brName;
                        :set logPref;
                        :set ssid;
         custom-script: 
