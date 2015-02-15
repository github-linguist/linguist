<cfdirectory action="list" directory="C:\temp" filter="*.html" name="dirListing">
<cfoutput query="dirListing">
  #dirListing.name# (#dirListing.type#)<br>
</cfoutput>
