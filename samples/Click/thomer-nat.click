// This Click configuration implements a firewall and NAT, roughly based on the
// mazu-nat.click example.
//
// This example assumes there is one interface that is IP-aliased.  In this
// example, eth0 and eth0:0 have IP addresses 66.68.65.90 and 192.168.1.1,
// respectively.  There is a local network, 192.168.1.0/24, and an upstream
// gateway, 66.58.65.89.  Traffic from the local network is NATed.
//
// Connections can be initiated from the NAT box itself, also.
// 
// For bugs, suggestions, and, corrections, please email me.
//
// Author: Thomer M. Gil (click@thomer.com)

AddressInfo(
    eth0-in     192.168.1.1     192.168.1.0/24  00:0d:87:9d:1c:e9,
    eth0-ex     66.58.65.90                     00:0d:87:9d:1c:e9,
    gw-addr     66.58.65.89                     00:20:6f:14:54:c2
);


elementclass SniffGatewayDevice {
  $device |
  from :: FromDevice($device)
    -> t1 :: Tee
    -> output;
  input -> q :: Queue(1024)
    -> t2 :: PullTee
    -> to :: ToDevice($device);
  t1[1] -> ToHostSniffers;
  t2[1] -> ToHostSniffers($device);
  ScheduleInfo(from .1, to 1);
}


device :: SniffGatewayDevice(eth0);
arpq_in :: ARPQuerier(eth0-in) -> device;
ip_to_extern :: GetIPAddress(16)
        -> CheckIPHeader
        -> EtherEncap(0x800, eth0-ex, gw-addr)
        -> device;
ip_to_host :: EtherEncap(0x800, gw-addr, eth0-ex)
        -> ToHost;
ip_to_intern :: GetIPAddress(16)
        -> CheckIPHeader
        -> arpq_in;

                 
arp_class :: Classifier(
        12/0806 20/0001,        // [0] ARP requests
        12/0806 20/0002,        // [1] ARP replies to host
        12/0800);               // [2] IP packets

device -> arp_class;

// ARP crap
arp_class[0] -> ARPResponder(eth0-in, eth0-ex) -> device;
arp_class[1] -> arp_t :: Tee;
                arp_t[0] -> ToHost;
                arp_t[1] -> [1]arpq_in;


// IP packets
arp_class[2] -> Strip(14)
   -> CheckIPHeader
   -> ipclass :: IPClassifier(dst host eth0-ex,
                              dst host eth0-in,
                              src net eth0-in);

// Define pattern NAT
iprw :: IPRewriterPatterns(NAT eth0-ex 50000-65535 - -);

// Rewriting rules for UDP/TCP packets
// output[0] rewritten to go into the wild
// output[1] rewritten to come back from the wild or no match
rw :: IPRewriter(pattern NAT 0 1,
                 pass 1);

// Rewriting rules for ICMP packets
irw :: ICMPPingRewriter(eth0-ex, -);
irw[0] -> ip_to_extern;
irw[1] -> icmp_me_or_intern :: IPClassifier(dst host eth0-ex, -);
          icmp_me_or_intern[0] -> ip_to_host;
          icmp_me_or_intern[1] -> ip_to_intern;

// Rewriting rules for ICMP error packets
ierw :: ICMPRewriter(rw irw);
ierw[0] -> icmp_me_or_intern;
ierw[1] -> icmp_me_or_intern;


// Packets directed at eth0-ex.
// Send it through IPRewriter(pass).  If there was a mapping, it will be
// rewritten such that dst is eth0-in:net, otherwise dst will still be for
// eth0-ex.
ipclass[0] -> [1]rw;

// packets that were rewritten, heading into the wild world.
rw[0] -> ip_to_extern;

// packets that come back from the wild or are not part of an established
// connection.
rw[1] -> established_class :: IPClassifier(dst host eth0-ex,
                                           dst net eth0-in);

         // not established yet or returning packets for a connection that was
         // established from this host itself.
         established_class[0] ->
           firewall :: IPClassifier(dst tcp port ssh,
                                    dst tcp port smtp,
                                    dst tcp port domain,
                                    dst udp port domain,
                                    icmp type echo-reply,
                                    proto icmp,
                                    port > 4095,
                                    -);

                                    firewall[0] -> ip_to_host; // ssh
                                    firewall[1] -> ip_to_host; // smtp
                                    firewall[2] -> ip_to_host; // domain (t)
                                    firewall[3] -> ip_to_host; // domain (u)
                                    firewall[4] -> [0]irw;     // icmp reply
                                    firewall[5] -> [0]ierw;    // other icmp
                                    firewall[6] -> ip_to_host; // port > 4095, probably for connection
                                                               // originating from host itself
                                    firewall[7] -> Discard;    // don't allow incoming for port <= 4095

        // established connection
        established_class[1] -> ip_to_intern;

// To eth0-in.  Only accept from inside network.
ipclass[1] -> IPClassifier(src net eth0-in) -> ip_to_host;

// Packets from eth0-in:net either stay on local network or go to the wild.
// Those that go into the wild need to go through the appropriate rewriting
// element.  (Either UDP/TCP rewriter or ICMP rewriter.)
ipclass[2] -> inter_class :: IPClassifier(dst net eth0-in, -);
              inter_class[0] -> ip_to_intern;
              inter_class[1] -> ip_udp_class :: IPClassifier(tcp or udp,
                                                             icmp type echo);
                                ip_udp_class[0] -> [0]rw;
                                ip_udp_class[1] -> [0]irw;
