require 'win32/eventlog'
logger = Win32::EventLog.new
logger.report_event(:event_type => Win32::EventLog::INFO, :data => "a test event log entry")
