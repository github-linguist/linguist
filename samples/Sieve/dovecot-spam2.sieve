# This script was copied from https://wiki2.dovecot.org/Pigeonhole/Sieve/Examples
# on 2020-04-08.
require ["comparator-i;ascii-numeric","relational"];
if allof (
   not header :matches "x-spam-score" "-*",
   header :value "ge" :comparator "i;ascii-numeric" "x-spam-score" "10" )
{
  discard;
  stop;
}
