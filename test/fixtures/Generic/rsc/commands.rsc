:log info "Starting scheduled maintenance";
:put [/system identity get name];
/system backup save name=scheduled-backup;
