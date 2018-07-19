<cfif fileExists(expandPath("input.txt"))>
  <cffile action="read" file="#expandPath('input.txt')#" variable="inputContents">
  <cffile action="write" file="#expandPath('output.txt')#" output="#inputContents#">
</cfif>
