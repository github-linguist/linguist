#!/bin/awk -f

BEGIN {
  # It is not possible to define output file names here because
  # FILENAME is not define in the BEGIN section
  n = "";
  printf "Generating data files ...";
  network_max_bandwidth_in_byte = 10000000;
  network_max_packet_per_second = 1000000;
  last3 = 0;
  last4 = 0;
  last5 = 0;
  last6 = 0;
}
{
  if ($1 ~ /Average/)
    { # Skip the Average values
      n = "";
      next;
    }

  if ($2 ~ /all/)
    { # This is the cpu info
      print $3 > FILENAME".cpu.user.dat";
#	  print $4 > FILENAME".cpu.nice.dat";
      print $5 > FILENAME".cpu.system.dat";
#     print $6 > FILENAME".cpu.iowait.dat";
      print $7 > FILENAME".cpu.idle.dat";
      print 100-$7 > FILENAME".cpu.busy.dat";
    }
  if ($2 ~ /eth0/)
    { # This is the eth0 network info
      if ($3 > network_max_packet_per_second)
	print last3 > FILENAME".net.rxpck.dat"; # Total number of packets received per second.
      else
	{
	  last3 = $3;
	  print $3 > FILENAME".net.rxpck.dat"; # Total number of packets received per second.
	}
      if ($4 > network_max_packet_per_second)
	print last4 > FILENAME".net.txpck.dat"; # Total number of packets transmitted per second.
      else
	{
	  last4 = $4;
	  print $4 > FILENAME".net.txpck.dat"; # Total number of packets transmitted per second.
	}
      if ($5 > network_max_bandwidth_in_byte)
	print last5 > FILENAME".net.rxbyt.dat"; # Total number of bytes received per second.
      else
	{
	  last5 = $5;
	  print $5 > FILENAME".net.rxbyt.dat"; # Total number of bytes received per second.
	}
      if ($6 > network_max_bandwidth_in_byte)
	print last6 > FILENAME".net.txbyt.dat"; # Total number of bytes transmitted per second.
      else
	{
	  last6 = $6;
	  print $6 > FILENAME".net.txbyt.dat"; # Total number of bytes transmitted per second.
	}
#     print $7 > FILENAME".net.rxcmp.dat"; # Number of compressed packets received per second (for cslip etc.).
#     print $8 > FILENAME".net.txcmp.dat"; # Number of compressed packets transmitted per second.
#     print $9 > FILENAME".net.rxmcst.dat"; # Number of multicast packets received per second.
    }

  # Detect which is the next info to be parsed
  if ($2 ~ /proc|cswch|tps|kbmemfree|totsck/)
    {
      n = $2;
    }

  # Only get lines with numbers (real data !)
  if ($2 ~ /[0-9]/)
    {
      if (n == "proc/s")
	{ # This is the proc/s info
	  print $2 > FILENAME".proc.dat";
#	  n = "";
	}
      if (n == "cswch/s")
	{ # This is the context switches per second info
	  print $2 > FILENAME".ctxsw.dat";
#	  n = "";
	}
      if (n == "tps")
	{ # This is the disk info
	  print $2 > FILENAME".disk.tps.dat"; # total transfers per second
	  print $3 > FILENAME".disk.rtps.dat"; # read requests per second
	  print $4 > FILENAME".disk.wtps.dat"; # write requests per second
	  print $5 > FILENAME".disk.brdps.dat"; # block reads per second
	  print $6 > FILENAME".disk.bwrps.dat"; # block writes per second
#	  n = "";
	}
      if (n == "kbmemfree")
	{ # This is the mem info
	  print $2 > FILENAME".mem.kbmemfree.dat"; # Amount of free memory available in kilobytes.
	  print $3 > FILENAME".mem.kbmemused.dat"; # Amount of used memory in kilobytes. This does not take into account memory used by the kernel itself.
	  print $4 > FILENAME".mem.memused.dat"; # Percentage of used memory.
#         It appears the kbmemshrd has been removed from the sysstat output - ntolia
#	  print $X > FILENAME".mem.kbmemshrd.dat"; # Amount of memory shared by the system in kilobytes.  Always zero with 2.4 kernels.
#	  print $5 > FILENAME".mem.kbbuffers.dat"; # Amount of memory used as buffers by the kernel in kilobytes.
	  print $6 > FILENAME".mem.kbcached.dat"; # Amount of memory used to cache data by the kernel in kilobytes.
#	  print $7 > FILENAME".mem.kbswpfree.dat"; # Amount of free swap space in kilobytes.
#	  print $8 > FILENAME".mem.kbswpused.dat"; # Amount of used swap space in kilobytes.
	  print $9 > FILENAME".mem.swpused.dat"; # Percentage of used swap space.
#	  n = "";
 	}
      if (n == "totsck")
	{ # This is the socket info
	  print $2 > FILENAME".sock.totsck.dat"; # Total number of used sockets.
	  print $3 > FILENAME".sock.tcpsck.dat"; # Number of TCP sockets currently in use.
#	  print $4 > FILENAME".sock.udpsck.dat"; # Number of UDP sockets currently in use.
#	  print $5 > FILENAME".sock.rawsck.dat"; # Number of RAW sockets currently in use.
#	  print $6 > FILENAME".sock.ip-frag.dat"; # Number of IP fragments currently in use.
#	  n = "";
 	}
    }
}
END {
  print " '" FILENAME "' done.";
}
