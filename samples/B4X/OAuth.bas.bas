B4J=true
Group=Network
ModulesStructureVersion=1
Type=Class
Version=8.5
@EndOfDesignText@
#Event: SignedIn (Result As PLMResult)
Sub Class_Globals
	Private su As StringUtils
	#if B4A
	Private LastIntent As Intent
	#end if
	#if B4J
	Private serversock As ServerSocket
	Private fx As JFX
	Private port As Int = 51067
	Private astream As AsyncStreams
	#End if
	Private mCallback As Object
	Private mEventName As String
	Private packageName As String 'ignore
	Private CurrentlySignedInServer As PLMServer
End Sub

Public Sub Initialize (Callback As Object, EventName As String)
	mCallback = Callback
	mEventName = EventName
	#if B4A
	packageName = Application.PackageName
	#Else If B4i
	packageName = GetPackageName
	#End If
End Sub

Public Sub RegisterApp (Server As PLMServer) As ResumableSub
	Dim j As HttpJob
	j.Initialize("", Me)
	Dim sb As StringBuilder
	sb.Initialize
	sb.Append("client_name=").Append(su.EncodeUrl(Constants.AppName, "UTF8"))
	sb.Append("&redirect_uris=").Append(su.EncodeUrl(GetRedirectUri, "UTF8"))
	sb.Append("&scopes=read+write+follow+push")
	sb.Append("&website=").Append("https://www.b4x.com")
	Dim res As PLMResult
	Try
		j.PostString(Server.URL & "/api/v1/apps", sb.ToString)
	Catch
		Log(LastException)
		Return B4XPages.MainPage.CreatePLMResult(False, LastException)
	End Try
	Wait For (j) JobDone (j As HttpJob)
	If j.Success Then
		Try
			Dim m As Map = B4XPages.MainPage.TextUtils1.JsonParseMap(j.GetString)
			If m.IsInitialized Then
				Server.AppClientId = m.Get("client_id")
				Server.AppClientSecret = m.Get("client_secret")
				Log("server client id and secret set.")
				B4XPages.MainPage.PersistUserAndServers
				res = B4XPages.MainPage.CreatePLMResult2(True, "")
			End If
		Catch
			res = B4XPages.MainPage.CreatePLMResult(False, LastException)
			Log(LastException)
		End Try
	Else
		res = B4XPages.MainPage.CreatePLMResult2(False, j.ErrorMessage)
	End If
	j.Release
	If Server.AppClientSecret = "" Then res = B4XPages.MainPage.CreatePLMResult2(False, "client secret empty")
	Return res
End Sub

Public Sub SignIn (User As PLMUser, Server As PLMServer)
	Dim link As String = BuildLink(Server.URL & "/oauth/authorize", _
		 CreateMap("client_id": Server.AppClientId, _
		"redirect_uri": GetRedirectUri, _
		"response_type": "code", "scope": "read write follow push"))
	B4XPages.MainPage.ShowExternalLink(link)
	CurrentlySignedInServer = Server
	#if B4J
	PrepareServer
	#end if
End Sub

Private Sub BuildLink(Url As String, Params As Map) As String
	Dim sb As StringBuilder
	sb.Initialize
	sb.Append(Url)
	If Params.Size > 0 Then
		sb.Append("?")
		For Each k As String In Params.Keys
			sb.Append(su.EncodeUrl(k, "utf8")).Append("=").Append(su.EncodeUrl(Params.Get(k), "utf8"))
			sb.Append("&")
		Next
		sb.Remove(sb.Length - 1, sb.Length)
	End If
	Return sb.ToString
End Sub

#if B4J
Private Sub PrepareServer
	If serversock.IsInitialized Then serversock.Close
	If astream.IsInitialized Then astream.Close
	Do While True
		Try
			serversock.Initialize(port, "server")
			serversock.Listen
			Exit
		Catch
			port = port + 1
			Log(LastException)
		End Try
	Loop
	Wait For server_NewConnection (Successful As Boolean, NewSocket As Socket)
	If Successful Then
		astream.Initialize(NewSocket.InputStream, NewSocket.OutputStream, "astream")
		Dim Response As StringBuilder
		Response.Initialize
		Do While Response.ToString.Contains("Host:") = False
			Wait For AStream_NewData (Buffer() As Byte)
			Response.Append(BytesToString(Buffer, 0, Buffer.Length, "UTF8"))
		Loop
		astream.Write(("HTTP/1.0 200" & Chr(13) & Chr(10)).GetBytes("UTF8"))
		Sleep(50)
		astream.Close
		serversock.Close
		ParseBrowserUrl(Regex.Split2("$",Regex.MULTILINE, Response.ToString)(0))
	End If
	
End Sub
#else if B4A
Public Sub CallFromResume(Intent As Intent)
	If IsNewOAuth2Intent(Intent) Then
		LastIntent = Intent
		ParseBrowserUrl(Intent.GetData)
	End If
End Sub

Private Sub IsNewOAuth2Intent(Intent As Intent) As Boolean
	Return Intent.IsInitialized And Intent <> LastIntent And Intent.Action = Intent.ACTION_VIEW And _
		Intent.GetData <> Null And Intent.GetData.StartsWith(Application.PackageName)
End Sub
#else if B4I
Public Sub CallFromOpenUrl (url As String)
	If url.StartsWith(packageName & ":/oath") Then
		ParseBrowserUrl(url)
	End If
	Sleep(0)
	Dim no As NativeObject = B4XPages.MainPage.safari
	no = no.GetField("safari")
	If no.IsInitialized Then
		no.RunMethod("dismissViewControllerAnimated:completion:", Array(True, Null))
	End If
End Sub

Private Sub GetPackageName As String
	Dim no As NativeObject
	no = no.Initialize("NSBundle").RunMethod("mainBundle", Null)
	Dim name As Object = no.RunMethod("objectForInfoDictionaryKey:", Array("CFBundleIdentifier"))
	Return name
End Sub

#end if

Private Sub ParseBrowserUrl(Response As String)
	Dim m As Matcher = Regex.Matcher("code=([^&\s]+)", Response)
	If m.Find Then
		Dim code As String = m.Group(1)
		GetTokenFromAuthorizationCode(code)
	Else
		Log("Error parsing server response: " & Response)
		RaiseEvent(B4XPages.MainPage.CreatePLMResult2(False, "Error parsing server response: " & Response))
	End If
End Sub

Private Sub GetTokenFromAuthorizationCode (Code As String)
	Dim user As PLMUser = B4XPages.MainPage.User
	Dim server As PLMServer = CurrentlySignedInServer
	Log(Code)
	Log("Getting access token from authorization code...")
	Dim j As HttpJob
	j.Initialize("", Me)
	Dim postString As String = $"code=${Code}&client_id=${server.AppClientId}&grant_type=authorization_code&redirect_uri=${su.EncodeUrl(GetRedirectUri, "UTF8")}"$
	postString = postString & $"&client_secret=${server.AppClientSecret}&scope=read+write+follow+push"$
	j.PostString(server.URL & "/oauth/token", postString)
	Wait For (j) JobDone(j As HttpJob)
	If j.Success Then
		Dim m As Map = B4XPages.MainPage.TextUtils1.JsonParseMap(j.GetString)
		If m.IsInitialized Then
			user.AccessToken = m.Get("access_token")
			user.MeURL = m.Get("me")
			Wait For (VerifyUser (CurrentlySignedInServer)) Complete (Result As PLMResult)
			j.Release
			RaiseEvent(Result)
		Else
			RaiseEvent(B4XPages.MainPage.CreatePLMResult2(False, "Failed to parse server response: " & j.GetString))
		End If
	Else
		RaiseEvent(B4XPages.MainPage.CreatePLMResult2(False, j.ErrorMessage))
	End If
	j.Release
End Sub

Private Sub RaiseEvent(Result As PLMResult)
	CallSubDelayed2(mCallback, mEventName & "_SignedIn", Result)
End Sub


Private Sub GetRedirectUri As String
	#if B4J
	Return "http://127.0.0.1:" & port
	#Else
		Return packageName & ":/oath"
	#End If
End Sub

Public Sub VerifyUser (Server As PLMServer) As ResumableSub
	Dim user As PLMUser = B4XPages.MainPage.User
	Dim j As HttpJob
	j.Initialize("", Me)
	j.Download(Server.URL & "/api/v1/accounts/verify_credentials")
	j.GetRequest.SetHeader("Authorization", "Bearer " & user.AccessToken)
	Dim res As PLMResult
	Wait For (j) JobDone(j As HttpJob)
	If j.Success Then
		Dim m As Map = B4XPages.MainPage.TextUtils1.JsonParseMap(j.GetString)
		If m.IsInitialized Then
			user.DisplayName = m.Get("display_name")
			If user.DisplayName = "" Then user.DisplayName = m.Get("username")
			user.Avatar = m.Get("avatar")
			user.Id = m.Get("id")
			user.Note = m.Get("note")
			user.Acct = m.Get("acct")
			res = B4XPages.MainPage.CreatePLMResult2(True, "")
		Else
			res = B4XPages.MainPage.CreatePLMResult2(False, "Failed to parse server response: " & j.GetString)
		End If
	Else
		res = B4XPages.MainPage.CreatePLMResult2(False, j.ErrorMessage)
		Log(j.ErrorMessage)
	End If
	j.Release
	Return res
End Sub

Public Sub AddAuthorization (job As HttpJob)
	Dim user As PLMUser = B4XPages.MainPage.User
	If user.SignedIn Then
		job.GetRequest.SetHeader("Authorization", "Bearer " & user.AccessToken)
	End If
End Sub