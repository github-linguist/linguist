<Query Kind="Program">
  <GACReference>System.Management.Automation, Version=3.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35</GACReference>
  <Namespace>System</Namespace>
  <Namespace>System.Dynamic</Namespace>
  <Namespace>System.Management.Automation</Namespace>
  <Namespace>System.Runtime.Serialization.Formatters</Namespace>
  <Namespace>System.Xml.Linq</Namespace>
</Query>

void Main()
{
	/*
	 * chart process memory
	 * Authored by zyonet
	 * No waranties provided what soever, use at your own risk or benefit ;D
	 * MIT license
	 * https://raw.githubusercontent.com/zyonet/PowerLinqPadScripts/master/LinqPad5/chart-process-memory.linq
	 */
	
	//To Test:  open this file in LinqPad 5 (https://www.linqpad.net)
	//refence System.Management.Automation dll in GAC
	//and set language to C# in vscode for the current editor
	
	var ps = PowerShell.Create();
	var _1m = 1024 * 1024;
	
	var script = @"get-process | Select Name,WS | Sort-Object -Descending WS | Where-Object {$_.WS -gt 10 * 1024 * 1024}";
	ps.AddScript(script);
	var res = ps.Invoke();
	
	var processes = res.Select( x => new {
		Name = (string) x.Properties["Name"].Value,
		WSInMb = (long) x.Properties["WS"].Value / _1m
	});
	
	//now chart it
	Util.Chart(processes
	.Where( p => p.WSInMb >= 100), //replace with your filter 
	(p) => p.Name, (p) => p.WSInMb,
	Util.SeriesType.Pie)
	.Dump("Processes Where WS > 100M (unit = 1Mb)");
}

// Define other methods and classes here
