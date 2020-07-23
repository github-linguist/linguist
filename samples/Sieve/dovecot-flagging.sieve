# This script was copied from https://wiki2.dovecot.org/Pigeonhole/Sieve/Examples
# on 2020-04-08.
require ["variables","date","fileinto","mailbox"];

# Extract date info
if currentdate :matches "year" "*" { set "year" "${1}"; }
if currentdate :matches "month" "*" { set "month" "${1}"; }

# Archive Dovecot mailing list items by year and month.
# Create folder when it does not exist.
if header :is "list-id" "dovecot.dovecot.org" {
  fileinto :create "INBOX.Lists.${year}.${month}.dovecot";
}
