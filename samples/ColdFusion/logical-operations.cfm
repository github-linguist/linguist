<cffunction name = "logic" hint = "Performs basic logical operations">
  <cfargument name = "a" required = "yes" type = "boolean" />
  <cfargument name = "a" required = "yes" type = "boolean" />
  <cfoutput>
    'A' AND 'B' is #a AND b#< br />
    'A' OR  'B' is #a OR  b#< br />
    NOT 'A'     is #!a#
  </cfoutput>
</cffunction>
