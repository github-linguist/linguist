<cfscript>
  value = 0;
  do
  {
    value += 1;
    writeOutput( value );
  } while( value % 6 != 0 );			
</cfscript>
