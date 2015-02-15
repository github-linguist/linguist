<cfloop from = "2008" to = "2121" index = "i">
    <cfset myDate = createDate(i, 12, 25) />
    <cfif dayOfWeek(myDate) eq 1>
        December 25th falls on a Sunday in <cfoutput>#i#</cfoutput><br />
    </cfif>
</cfloop>
