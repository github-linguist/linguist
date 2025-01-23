Scriptname vSCM_MetaQuestScript extends Quest  
{Do initialization and track variables for scripts}

;--=== Imports ===--

Import Utility
Import Game

;--=== Properties ===--

Actor Property PlayerRef Auto

Float Property ModVersion Auto Hidden

String Property ModName = "Smarter Combat Music" Auto Hidden

Message Property vSCM_ModLoadedMSG Auto
Message Property vSCM_ModUpdatedMSG Auto

;--=== Variables ===--

Float _CurrentVersion
String _sCurrentVersion

Bool _Running

Float _ScriptLatency
Float _StartTime
Float _EndTime

;--=== Events ===--

Event OnInit()
	If ModVersion == 0
		DoUpkeep(True)
	EndIf
EndEvent

Event OnReset()
	Debug.Trace("SCM: Metaquest event: OnReset")
EndEvent

Event OnGameReloaded()
	Debug.Trace("SCM: Metaquest event: OnGameReloaded")
EndEvent

;--=== Functions ===--

Function DoUpkeep(Bool DelayedStart = True)
	;FIXME: CHANGE THIS WHEN UPDATING!
	_CurrentVersion = 0.01
	_sCurrentVersion = GetVersionString(_CurrentVersion)
	String sErrorMessage
	If DelayedStart
		Wait(RandomFloat(2,4))
	EndIf
	Debug.Trace("SCM: " + ModName)
	Debug.Trace("SCM: Performing upkeep...")
	Debug.Trace("SCM: Loaded version is " + GetVersionString(ModVersion) + ", Current version is " + _sCurrentVersion)
	If ModVersion == 0
		Debug.Trace("SCM: Newly installed, doing initialization...")
		DoInit()
		If ModVersion == _CurrentVersion
			Debug.Trace("SCM: Initialization succeeded.")
		Else
			Debug.Trace("SCM: WARNING! Initialization had a problem!")
		EndIf
	ElseIf ModVersion < _CurrentVersion
		Debug.Trace("SCM: Installed version is older. Starting the upgrade...")
		DoUpgrade()
		If ModVersion != _CurrentVersion
			Debug.Trace("SCM: WARNING! Upgrade failed!")
			Debug.MessageBox("WARNING! " + ModName + " upgrade failed for some reason. You should report this to the mod author.")
		EndIf
		Debug.Trace("SCM: Upgraded to " + _CurrentVersion)
		vSCM_ModUpdatedMSG.Show(_CurrentVersion)
	Else
		Debug.Trace("SCM: Loaded, no updates.")
		;CheckForOrphans()
	EndIf
	CheckForExtras()
	UpdateConfig()
	Debug.Trace("SCM: Upkeep complete!")
EndFunction

Function DoInit()
	Debug.Trace("SCM: Initializing...")
	_Running = True
	ModVersion = _CurrentVersion
	vSCM_ModLoadedMSG.Show(_CurrentVersion)
EndFunction

Function DoUpgrade()
	_Running = False
	If ModVersion < 0.01
		Debug.Trace("SCM: Upgrading to 0.01...")
		ModVersion = 0.01
	EndIf
	_Running = True
	Debug.Trace("SCM: Upgrade complete!")
EndFunction

Function UpdateConfig()
	Debug.Trace("SCM: Updating configuration...")

	Debug.Trace("SCM: Updated configuration values, some scripts may update in the background!")
EndFunction

String Function GetVersionString(Float fVersion)
	Int Major = Math.Floor(fVersion) as Int
	Int Minor = ((fVersion - (Major as Float)) * 100.0) as Int
	If Minor < 10
		Return Major + ".0" + Minor
	Else
		Return Major + "." + Minor
	EndIf
EndFunction

Function CheckForExtras()
EndFunction
