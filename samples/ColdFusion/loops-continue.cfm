<cfscript>
  for( i = 1; i <= 10; i++ )
  {
    writeOutput( i );
    if( 0 == i % 5 )
    {
      writeOutput( "< br />" );
      continue;
    }
    writeOutput( "," );
  }
</cfscript>
