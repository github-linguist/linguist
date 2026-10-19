# Refresh handlers after installing a package.
# trigger RefreshHandlers on Package__c (after insert)
trigger_dir="${TRIGGER_DIR:-/etc/package-triggers}"
printf '%s\n' 'public with sharing class PackageHandler'

for handler in "$trigger_dir"/*.sh; do
    [ -f "$handler" ] || continue
    sh "$handler" "$@"
done
