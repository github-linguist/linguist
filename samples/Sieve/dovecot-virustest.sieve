# This script was copied from https://wiki2.dovecot.org/Pigeonhole/Sieve/Examples
# on 2020-04-08.
require "virustest";
require "fileinto";
require "relational";
require "comparator-i;ascii-numeric";

/* Not scanned ? */
if virustest :value "eq" :comparator "i;ascii-numeric" "0" {
  fileinto "Unscanned";

/* Infected with high probability (value range in 1-5) */
} if virustest :value "eq" :comparator "i;ascii-numeric" "4" {
  /* Quarantine it in special folder (still somewhat dangerous) */
  fileinto "Quarantine";

/* Definitely infected */
} elsif virustest :value "eq" :comparator "i;ascii-numeric" "5" {
  /* Just get rid of it */
  discard;
}
