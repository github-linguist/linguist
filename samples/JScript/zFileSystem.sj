function startService(serviceName)
{
  Sys.OleObject("WScript.Shell").Run("\"sc.exe\" start "+serviceName,1,true);
  Log.Message(serviceName + " was started");
}

function stopService(serviceName)
{
  Sys.OleObject("WScript.Shell").Run("\"sc.exe\" stop "+serviceName,1,true);
  Log.Message(serviceName + " was stopped");
}

function TXTFile_open(pathToTXTFile)
{
  if(aqFile.Exists(pathToTXTFile))
    return aqFile.OpenTextFile(pathToTXTFile, aqFile.faReadWrite, aqFile.ctANSI);
  else
    Log.Error(pathToTXTFile+" does not exist.");
} 

function TXTFile_close(openTXTFile)
{
  openTXTFile.Close(); 
} 

function TXTFile_readAll(pathToTXTFile)
{
  if(aqFile.Exists(pathToTXTFile))
    return aqFile.ReadWholeTextFile(pathToTXTFile, aqFile.ctUTF8);
  else
    Log.Error(pathToTXTFile+" does not exist.");
}

function TXTFile_readSingeLine(openTXTfile, lineNumberToBeRead)
{
  var totalLinesInFile = openTXTfile.LinesCount;
  if(lineNumberToBeRead > totalLinesInFile)
  {
    Log.Message("Line trying to be read: "+lineNumberToBeRead);
    Log.Message("Total Lines in file: "+totalLinesInFile);
    Log.Error("The line attempting to be read is out of bounds");
    return;      
  }
  else
  {
    openTXTfile.Cursor = 0;
    for(var counter=0; counter<lineNumberToBeRead; counter++)
    {
      openTXTfile.SkipLine(1);
    }    
    return openTXTfile.ReadLine();
  }      
}
 
