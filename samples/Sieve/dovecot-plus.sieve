# This script was copied from https://wiki2.dovecot.org/Pigeonhole/Sieve/Examples
# on 2020-04-08.
require ["fileinto", "envelope", "subaddress"];
if envelope :detail "to" "spam"{
  fileinto "Spam";
}
