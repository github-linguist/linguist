B4J=true
Group=Network
ModulesStructureVersion=1
Type=Class
Version=8.45
@EndOfDesignText@
Sub Class_Globals
	
End Sub

Public Sub Initialize
	
End Sub

Public Sub CancelRequest (URL As String, Job As HttpJob)
	If HttpUtils2Service.hc.IsInitialized = False Then Return
#if B4J or B4A
	Dim Jo As JavaObject = HttpUtils2Service.hc
	Dim OkHttpClient As JavaObject = Jo.GetField("client")
	Dim RunningCalls As List = OkHttpClient.RunMethodJO("dispatcher", Null).RunMethod("runningCalls", Null)
	For Each call As JavaObject In RunningCalls
		Dim req As JavaObject = call.RunMethod("request", Null)
		Dim s As String = req.RunMethod("url", Null)
		If s = URL Then
			call.RunMethod("cancel", Null)
			Return
		End If
	Next
	For Each j As HttpJob In HttpUtils2Service.TaskIdToJob.Values
		If j = Job Then 
			If j.Out.IsInitialized Then j.Out.Close
			Return
		End If
	Next
#else if B4i
	Dim no As NativeObject = HttpUtils2Service.hc
	Dim session As NativeObject = no.GetField("session")
	no = Me
	no.RunMethod("cancelDownload::", Array(session, Job.req))
#end if
	
End Sub

#if OBJC
- (void) cancelDownload:(NSURLSession*)session :(B4IHttpRequest*)req{
	[session getTasksWithCompletionHandler:^(NSArray *dataTasks, NSArray *uploadTasks, NSArray *downloadTasks) {
		//NSLog(@"data: %@, \nupload: %@, \ndownload: %@", dataTasks, uploadTasks, downloadTasks);
		@try {
			for (NSURLSessionDownloadTask* task in downloadTasks) {
				if ([task.originalRequest.URL isEqual:((NSURLRequest*)req.object).URL]) {
					[task cancel];
				}
			}
		}
		 @catch (NSException *exception) {
		 	 NSLog(@"%@", exception);
		 }
		
	}];
}
#End If