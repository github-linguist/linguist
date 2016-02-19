rates :: AvailableRates
elementclass sr2 {
  $sr2_ip, $sr2_nm, $wireless_mac, $gateway, $probes|


arp :: ARPTable();
lt :: LinkTable(IP $sr2_ip);


gw :: SR2GatewaySelector(ETHTYPE 0x062c,
		      IP $sr2_ip,
		      ETH $wireless_mac,
		      LT lt,
		      ARP arp,
		      PERIOD 15,
		      GW $gateway);


gw -> SR2SetChecksum -> [0] output;

set_gw :: SR2SetGateway(SEL gw);


es :: SR2ETTStat(ETHTYPE 0x0641, 
	      ETH $wireless_mac, 
	      IP $sr2_ip, 
	      PERIOD 30000,
	      TAU 300000,
	      ARP arp,
	      PROBES $probes,
	      ETT metric,
	      RT rates);


metric :: SR2ETTMetric(LT lt);


forwarder :: SR2Forwarder(ETHTYPE 0x0643, 
			      IP $sr2_ip, 
			      ETH $wireless_mac, 
			      ARP arp, 
			      LT lt);


querier :: SR2Querier(ETH $wireless_mac, 
		     SR forwarder,
		     LT lt, 
		     ROUTE_DAMPENING true,
		     TIME_BEFORE_SWITCH 5,
		     DEBUG true);


query_forwarder :: SR2MetricFlood(ETHTYPE 0x0644,
			       IP $sr2_ip, 
			       ETH $wireless_mac, 
			       LT lt, 
			       ARP arp,
			       DEBUG false);

query_responder :: SR2QueryResponder(ETHTYPE 0x0645,
				    IP $sr2_ip, 
				    ETH $wireless_mac, 
				    LT lt, 
				    ARP arp,
				    DEBUG true);


query_responder -> SR2SetChecksum -> [0] output;
query_forwarder -> SR2SetChecksum -> SR2Print(forwarding) -> [0] output;
query_forwarder [1] -> query_responder;

data_ck :: SR2SetChecksum() 

input [1] 
-> host_cl :: IPClassifier(dst net $sr2_ip mask $sr2_nm,
				-)
-> querier
-> data_ck;


host_cl [1] -> [0] set_gw [0] -> querier;

forwarder[0] 
  -> dt ::DecIPTTL
  -> data_ck
  -> [2] output;


dt[1] 
-> Print(ttl-error) 
-> ICMPError($sr2_ip, timeexceeded, 0) 
-> querier;


// queries
querier [1] -> [1] query_forwarder;
es -> SetTimestamp() -> [1] output;


forwarder[1] //ip packets to me
  -> SR2StripHeader()
  -> CheckIPHeader()
  -> from_gw_cl :: IPClassifier(src net $sr2_ip mask $sr2_nm,
				-)
  -> [3] output;

from_gw_cl [1] -> [1] set_gw [1] -> [3] output;

 input [0]
   -> ncl :: Classifier(
			12/0643 , //sr2_forwarder
			12/0644 , //sr2
			12/0645 , //replies
			12/0641 , //sr2_es
			12/062c , //sr2_gw
			);
 
 
 ncl[0] -> SR2CheckHeader() -> [0] forwarder;
 ncl[1] -> SR2CheckHeader() -> PrintSR(query) -> query_forwarder
 ncl[2] -> SR2CheckHeader() -> query_responder;
 ncl[3] -> es;
 ncl[4] -> SR2CheckHeader() -> gw;
 
}



Idle -> s :: sr2(2.0.0.1, 255.0.0.0, 00:00:00:00:00:01, false, "12 60 12 1500") -> Discard;
Idle -> [1] s;
s[1] -> Discard;
s[2] -> Discard;
s[3] -> Discard;
