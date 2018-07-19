$XMLdata=@"
<?xml version="1.0" encoding="utf-8"?>
<unattend xmlns="urn:schemas-microsoft-com:unattend">
    <servicing>
        <package action="configure">
            <assemblyIdentity name="Microsoft-Windows-Foundation-Package" version="${strFullVersion}" processorArchitecture="amd64" publicKeyToken="31bf3856ad364e35" language="" />
            <selection name="RemoteServerAdministrationTools" state="true" />
            <selection name="RemoteServerAdministrationTools-Roles-AD" state="true" />
            <selection name="RemoteServerAdministrationTools-Roles-AD-DS" state="true" />
            <selection name="RemoteServerAdministrationTools-Roles-AD-DS-SnapIns" state="true" />
            <selection name="RemoteServerAdministrationTools-Features-StorageManager" state="true" />
            <selection name="RemoteServerAdministrationTools-Features-Wsrm" state="true" />
        </package>
    </servicing>
    <cpi:offlineImage cpi:source="wim:d:/2008r2wim/install.wim#Windows Server 2008 R2 SERVERSTANDARD" xmlns:cpi="urn:schemas-microsoft-com:cpi" />
</unattend>
"@
