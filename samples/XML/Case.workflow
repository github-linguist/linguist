<?xml version="1.0" encoding="UTF-8"?>
<Workflow xmlns="http://soap.sforce.com/2006/04/metadata">
    <alerts>
        <fullName>New_Case_Created_Email_Alert</fullName>
        <ccEmails>grantshubsupport@cabinetoffice.gov.uk</ccEmails>
        <description>New Case Created Email Alert</description>
        <protected>false</protected>
        <senderType>CurrentUser</senderType>
        <template>unfiled$public/New_Case_Create_Email_Alert</template>
    </alerts>
    <rules>
        <fullName>New Case Created</fullName>
        <actions>
            <name>New_Case_Created_Email_Alert</name>
            <type>Alert</type>
        </actions>
        <active>true</active>
        <formula>NOT( $Setup.Bypass__c.Workflow_Rules__c )</formula>
        <triggerType>onCreateOnly</triggerType>
    </rules>
</Workflow>