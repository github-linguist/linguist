// https://raw.githubusercontent.com/beefytech/Beef/5d5186f8127af4c670f76d7af7df735d3544a16d/IDE/dist/lib/gen/src/Program.bf
#pragma warning disable 168

using System;
using System;
using System.Interop;
using System.IO;
using System.Threading;
using System.Diagnostics;
using System.Collections;

namespace gen
{
	class Program
	{
		static String[] sNames = new .[]("comdlg32", "gdi32", "kernel32", "ole32", "netapi32", "ntdll", "rpcrt4", "shell32", "user32", "version", "winmm") ~ delete _;

		public static int Main(String[] args)
		{
			bool isFirst = true;

			for (var name in sNames)
			{
				for (int pass < 2)
				{
					String resultStr = scope String();
					
					//
					{
						ProcessStartInfo startInfo = scope .();
						startInfo.SetFileName(@"c:\bin\dumpbin.exe");
						if (pass == 0)
							startInfo.SetArguments(scope $"/exports c:\\windows\\system32\\{name}.dll");
						else
							startInfo.SetArguments(scope $"/exports c:\\windows\\SysWow64\\{name}.dll");
						startInfo.RedirectStandardOutput = true;
						startInfo.UseShellExecute = false;
						startInfo.RedirectStandardOutput = true;
						startInfo.CreateNoWindow = false;
						SpawnedProcess process = scope SpawnedProcess();
						if (process.Start(startInfo) case .Err)
							continue;

						FileStream fileStream = scope FileStream();
						process.AttachStandardOutput(fileStream);
						StreamReader streamReader = scope StreamReader(fileStream, null, false, 4096);
						streamReader.ReadToEnd(resultStr).IgnoreError();

						if (process.ExitCode != 0)
							Runtime.FatalError(scope $"Failed to read lib '{name}'");
					}

					var outStr = scope String();
					outStr.Append("EXPORTS\n");

					for (var line in resultStr.Split('\n'))
					{
						if ((line.Length > 26) && (line.StartsWith("      ")) && (line[25] == ' ') && (line[26] != '['))
						{
							var exportStr = line.Substring(26);

							int fwdIdx = exportStr.IndexOf("(forwarded to");
							if (fwdIdx != -1)
							{
								outStr.Append(exportStr.Substring(0, fwdIdx - 1));
								outStr.Append(" = ");
								outStr.Append(exportStr.Substring(fwdIdx + 14, exportStr.Length - fwdIdx - 15));
							}
							else
							{
								outStr.Append(exportStr);

								switch (exportStr)
								{
								case "DllMain",
									"DllGetClassObject",
									"DllCanUnloadNow",
									"DllRegisterServer",
									"DllUnregisterServer",
									"DllInstall":
									outStr.Append(" PRIVATE");
								}
							}


							outStr.Append("\n");
						}
					}

					File.WriteAllText(scope $"{name}.txt", outStr);

					//
					{
						ProcessStartInfo startInfo = scope .();
						startInfo.SetFileName(@"c:\bin\lib.exe");
						if (pass == 0)
							startInfo.SetArguments(scope $"/def:{name}.txt /machine:x64 /out:..\\x64\\{name}.lib");
						else
							startInfo.SetArguments(scope $"/def:{name}.txt /machine:x64 /out:..\\x86\\{name}.lib");
						startInfo.CreateNoWindow = false;
						SpawnedProcess process = scope SpawnedProcess();
						if (process.Start(startInfo) case .Err)
							continue;

						process.WaitFor();
						if (process.ExitCode != 0)
							Runtime.FatalError(scope $"Failed to generate lib '{name}'");
					}
				}
			}

			return 0;
		}
	}
}
