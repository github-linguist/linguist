<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE ResourceType SYSTEM "http://www.holobloc.com/xml/LibraryElement.dtd" >
<ResourceType Name="_CBCore0" Comment="" >
<Identification Standard="61499-2" />
<VersionInfo Organization="UOA" Version="0.1" Author="hpea485" Date="2019-00-29" />
<CompilerInfo header="" classdef="">
</CompilerInfo>

<FBNetwork>
  <FB Name="cb3rx" Type="ArgoRx" x="568.75" y="2143.75">
    <Parameter Name="ChanId" Value="3" />
  </FB>
  <FB Name="cb2rx" Type="ArgoRx" x="568.75" y="1575">
    <Parameter Name="ChanId" Value="2" />
  </FB>
  <FB Name="cb1rx" Type="ArgoRx" x="568.75" y="1006.25">
    <Parameter Name="ChanId" Value="1" />
  </FB>
  <FB Name="Reference" Type="SifbCBPrintStatus" x="2479.16666666667" y="1629.6875" />
  <EventConnections><Connection Source="cb3rx.DataPresent" Destination="Reference.StatusUpdate" />
<Connection Source="cb2rx.DataPresent" Destination="Reference.StatusUpdate" />
<Connection Source="cb1rx.DataPresent" Destination="Reference.StatusUpdate" /></EventConnections>
  <DataConnections><Connection Source="cb3rx.Data" Destination="Reference.St3" />
<Connection Source="cb2rx.Data" Destination="Reference.St2" />
<Connection Source="cb1rx.Data" Destination="Reference.St1" /></DataConnections>
</FBNetwork>
</ResourceType>
