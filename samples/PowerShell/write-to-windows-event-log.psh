# Create Event Log object
$EventLog=new-object System.Diagnostics.EventLog("Application")
#Declare Event Source; must be 'registered' with Windows
$EventLog.Source="Application" # It is possible to register a new source (see Note2)
# Setup the Event Types; you don't have to use them all, but I'm including all the possibilities for reference
$infoEvent=[System.Diagnostics.EventLogEntryType]::Information
$errorEvent=[System.Diagnostics.EventLogEntryType]::Error
$warningEvent=[System.Diagnostics.EventLogEntryType]::Warning
$successAuditEvent=[System.Diagnostics.EventLogEntryType]::SuccessAudit
$failureAuditEvent=[System.Diagnostics.EventLogEntryType]::FailureAudit

# Write the event in the format "Event test",EventType,EventID
$EventLog.WriteEntry("My Test Event",$infoevent,70)
