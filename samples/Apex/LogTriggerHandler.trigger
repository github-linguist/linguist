/**
 * @description Trigger handler for persisting Platform Event based
 * log messages.
 *
 * @group Trigger Recipes
 * @see Log
 */
public with sharing class LogTriggerHandler extends TriggerHandler {
    List<Log__e> incomingRecords = new List<Log__e>();

    /**
     * @description constructor accepting a list of log__e records
     */
    public LogTriggerHandler() {
        this.incomingRecords = (List<Log__e>) Trigger.new;
    }

    /**
     * @description code to be executed in the afterInsert context
     */
    override public void afterInsert() {
        List<LogEvent__c> events = new List<LogEvent__c>();

        for (Log__e event : this.incomingRecords) {
            events.add(
                new LogEvent__c(
                    Log_Data__c = event.Log_Message__c,
                    Quiddity__c = event.Quiddity__c,
                    Request_Id__c = event.Request_Id__c,
                    Severity__c = event.Severity__c
                )
            );
        }

        List<Database.SaveResult> res = Database.insert(events, false);
        for (Database.SaveResult saveRes : res) {
            if (!saveRes.isSuccess()) {
                System.debug(
                    LoggingLevel.ERROR,
                    'Failed to save log message: ' + saveRes
                );
            }
        }
    }
}