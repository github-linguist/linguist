<?php
  $ipv4_record = dns_get_record("www.kame.net",DNS_A);
  $ipv6_record = dns_get_record("www.kame.net",DNS_AAAA);
  print "ipv4: " . $ipv4_record[0]["ip"] . "\n";
  print "ipv6: " . $ipv6_record[0]["ipv6"] . "\n";
?>
