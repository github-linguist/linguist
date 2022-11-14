using System;
using System.IO;
using System.Net;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

/*
 * Based on HttpServer.cs from Benjamin N. Summerton, thx - https://gist.github.com/define-private-public/d05bc52dd0bed1c4699d49e2737e80e7
 *
 * Compile how-to:
 * 	 - e.g.: c:\windows\microsoft.net\framework64\v4.0.30319\csc.exe /out:.\httpserver.exe .\SimpleHttpServer.cs
 *
 * Upload how-to:
 *   - Access via browser on your listening port
 *   - PowerShell: iwr -Uri http://<listenhost>:<port>/filename.dll -OutFile filename.dll
 *
 * Download how-to:
 *   - Access via browser on your listening port
 *   - PowerShell 3+: Invoke-RestMethod -Method POST -Body ([System.IO.File]::ReadAllBytes('c:\filename.dll)) http://<listenhost>:<port>/?fn=filename.dll
 *
 * */

class SimpleHttpServer
{
	public static HttpListener listener;
	public static string url = "";
	public const string pageStart = @"
		<!doctype>
		<html>
		<head>
			<title>HTTP Server</title>
			<script>function upload(el) {{
				fr = new FileReader();
				fr.readAsArrayBuffer(el.files[0]);
				fname = el.value.split('\\\\').slice(-1)[0];
				fr.onload = () => {{
					fetch('{0}?fn='+fname, {{
						method: 'POST',
						body: fr.result
						}}).then(() => {{
							document.querySelector('p').innerHTML = fname + ' most likely uploded successfully';
						}}).catch(() => {{
							document.querySelector('p').innerHTML = 'upload possibly failed';
						}})
					}}
				}}
			</script>
		</head>
		<body>
			<ul>";
	public const string pageEnd = @"
			</ul>
			<hr>
			<form>
				Upload file: <input type=""file"" onchange=""upload(this)"">
			</form>
			<p></p>
		</body>
		</html>";


	public static async Task HandleIncomingConnections()
	{
		string localDir = Directory.GetCurrentDirectory();

		while (true)
		{
			string html = String.Format(pageStart, url);
			HttpListenerContext ctx = await listener.GetContextAsync();
			HttpListenerRequest req = ctx.Request;
			HttpListenerResponse resp = ctx.Response;
			byte[] data = new byte[0];
			string queryFname = req.QueryString.Get("fn");
			double mb = Math.Pow(2,20);

			Console.Write(req.HttpMethod + " " + req.Url.ToString() + " " + req.RemoteEndPoint.ToString());

			if (req.Url.AbsolutePath.Length > 1)
			{
				string downFname = req.Url.AbsolutePath.Substring(1);
				downFname = System.Uri.UnescapeDataString(downFname);
				try
				{
					data = File.ReadAllBytes(localDir+"\\"+downFname);
					Console.Write(" '" + downFname + "' sent");
				}
				catch (FileNotFoundException)
				{
					resp.StatusCode = 404;
					Console.Write(" '" + downFname + "' not found");
				}
				resp.ContentType = "application/octet-stream";
				resp.Headers.Add("Content-Disposition", "attachment; filename=\"" + downFname + "\"");
			}
			else if (req.HttpMethod == "POST" && queryFname != null)
			{
				using (FileStream destination = new FileStream(queryFname, FileMode.Create, FileAccess.Write))
				{
					req.InputStream.CopyTo(destination);
				}
				Console.Write(" '" + queryFname + "' saved");
			}
			else
			{
				DirectoryInfo directoryInfo = new DirectoryInfo(localDir);
				foreach (FileInfo fi in directoryInfo.EnumerateFiles("*"))
				{
					html += String.Format("<li>\n<a href=\"/{0}\" target=\"_blank\">{0} (Size: {1:.000}MB)</a>\n</li>", fi, fi.Length/mb);
				}
				html += pageEnd;
				data = Encoding.UTF8.GetBytes(html);
				resp.ContentType = "text/html";
				resp.ContentEncoding = Encoding.UTF8;
			}

			resp.ContentLength64 = data.LongLength;
			await resp.OutputStream.WriteAsync(data, 0, data.Length);
			resp.Close();
			req = null;
			resp = null;
			ctx = null;
			data = new byte[0]; 
			GC.Collect(); // otherwise it takes stupidly long time to free memory after downloads
			Console.Write("\n");
		}
	}

	public static void Main(string[] args)
	{
		if (args.Length != 2)
		{
			Console.WriteLine(String.Format("Usage: {0} <listen host/IP> <listen port>", Environment.GetCommandLineArgs()[0]));
			return;
		}
		url = "http://" + args[0] + ":" + args[1] + "/";
		listener = new HttpListener();
		listener.Prefixes.Add(url);
		listener.Start();
		Console.WriteLine("Listening for connections on {0}", url);
		Task task = HandleIncomingConnections();
		task.GetAwaiter().GetResult();
		listener.Close();
	}
}

