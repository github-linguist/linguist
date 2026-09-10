TRIGGER AuditEvents
ON Audit_Entry__c (
    AFTER INSERT,
    AFTER UNDELETE
) {
    AuditDispatcher.publish(Trigger.new);
}
