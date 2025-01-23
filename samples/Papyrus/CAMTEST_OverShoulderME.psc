Scriptname CAMTEST_OverShoulderME extends activemagiceffect  
{Play with camera effects}

;--=== Imports ===--

Import Utility
Import Game

;--=== Properties ===--

Actor Property PlayerRef Auto
ActorBase Property CAMTEST_CameraActor Auto

;--=== Variables ===--

Actor Player

Actor Camera

Actor Target

Float PosX
Float PosY
Float PosZ
Float SpeedMult

ObjectReference Mist
ObjectReference Fog

;--=== Events ===--

Event OnInit()
	Player = PlayerRef
EndEvent

Event onEffectStart(Actor akTarget, Actor akCaster)
	Camera = Player.PlaceActorAtMe(CAMTEST_CameraActor)
	Camera.EnableAI(False)
	Camera.SetScale(0.1)
	Camera.TranslateTo(Player.X + 40,Player.Y,Player.Z,0,0,0,800,30)
	DisablePlayerControls(abMovement = true, abFighting = true, abCamSwitch = true,  abLooking = true, abSneaking = true, abMenu = true, abActivate = true, abJournalTabs = false)
	SetPlayerAIDriven(True)
	ForceThirdPerson()
	SetHUDCartMode()
	SetInChargen(True, True, False)
	SetCameraTarget(Camera)
	ForceFirstPerson()
	Wait(1)
	Camera.SplineTranslateTo(Player.X + 4000,Player.Y,Player.Z + 1000,15,0,Camera.GetHeadingAngle(Player) + Camera.GetAngleZ(),1800,800,100)
;	Camera.SetLookAt(Player)
	Wait(10)
	Camera.SplineTranslateTo(Player.X + 1000,Player.Y - 500,Player.Z + 500,25,0,Camera.GetHeadingAngle(Player) + Camera.GetAngleZ(),1800,800,100)
	Wait(10)
	SetHUDCartMode(False)
	SetCameraTarget(Player)
	SetInChargen(False, False, False)
	EnablePlayerControls()
	SetPlayerAIDriven(False)
EndEvent

Event onUpdate()
EndEvent

Event onEffectFinish(Actor akTarget, Actor akCaster)
EndEvent

;--=== Functions ===--

