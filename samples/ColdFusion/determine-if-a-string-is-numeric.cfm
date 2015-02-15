<cfset TestValue=34>
  TestValue: <cfoutput>#TestValue#</cfoutput><br>
<cfif isNumeric(TestValue)>
  is Numeric.
<cfelse>
  is NOT Numeric.
</cfif>

<cfset TestValue="NAS">
  TestValue: <cfoutput>#TestValue#</cfoutput><br>
<cfif isNumeric(TestValue)>
  is Numeric.
<cfelse>
  is NOT Numeric.
</cfif>
