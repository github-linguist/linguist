<cfcomponent>
	
	<cffunction name="init" access="public" returntype="any">
		<cfargument name="arg1" type="any" required="true">
		<cfset this.myVariable = arguments.arg1>

		<cfreturn this>
	</cffunction>

	<cffunction name="testFunc" access="private" returntype="void">
		<cfargument name="arg1" type="any" required="false">
		
		<cfif structKeyExists(arguments, "arg1")>
			<cfset writeoutput("Argument exists")>
		</cfif>
	</cffunction>
	
</cfcomponent>