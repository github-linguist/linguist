# This script was copied from https://wiki2.dovecot.org/Pigeonhole/Sieve/Examples
# on 2020-04-08.
require ["variables", "envelope", "fileinto", "subaddress"];

if envelope :is :user "to" "sales" {
  if envelope :matches :detail "to" "*" {
    /* Save name in ${name} in all lowercase except for the first letter.
     * Joe, joe, jOe thus all become 'Joe'.
     */
    set :lower :upperfirst "name" "${1}";
  }

  if string :is "${name}" "" {
    /* Default case if no detail is specified */
    fileinto "sales";
  } else {
    /* For sales+joe@ this will become users/Joe */
    fileinto "users/${name}";
  }
}
