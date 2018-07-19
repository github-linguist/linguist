<cfscript>
  localFile = getFileInfo(expandpath("input.txt"));
  rootFile = getFileInfo("/input.txt");
</cfscript>

<cfoutput>
  Size of input.txt is #localFile.size# bytes.
  Size of /input.txt is #rootFile.size# bytes.
</cfoutput>
