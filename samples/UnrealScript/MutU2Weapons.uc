/*
 * Copyright (c) 2008, 2013 Dainius "GreatEmerald" Masiliūnas
 *
 *  Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
//-----------------------------------------------------------------------------
// MutU2Weapons.uc
// Mutator class for replacing weapons
// GreatEmerald, 2008
//-----------------------------------------------------------------------------

class MutU2Weapons extends Mutator
    config(U2Weapons);

var() config string ReplacedWeaponClassNames0;
var() config string ReplacedWeaponClassNames1, ReplacedWeaponClassNames2, ReplacedWeaponClassNames3;
var() config string ReplacedWeaponClassNames4, ReplacedWeaponClassNames5, ReplacedWeaponClassNames6;
var() config string ReplacedWeaponClassNames7, ReplacedWeaponClassNames8, ReplacedWeaponClassNames9;
var() config string ReplacedWeaponClassNames10, ReplacedWeaponClassNames11, ReplacedWeaponClassNames12;
var() config bool bConfigUseU2Weapon0, bConfigUseU2Weapon1, bConfigUseU2Weapon2, bConfigUseU2Weapon3;
var() config bool bConfigUseU2Weapon4, bConfigUseU2Weapon5, bConfigUseU2Weapon6, bConfigUseU2Weapon7;
var() config bool bConfigUseU2Weapon8, bConfigUseU2Weapon9, bConfigUseU2Weapon10, bConfigUseU2Weapon11;
var() config bool bConfigUseU2Weapon12;
//var byte bUseU2Weapon[13];
//var class<Weapon> ReplacedWeaponClasses[13];
//var class<WeaponPickup> ReplacedWeaponPickupClasses[13];
//var class<Ammo> ReplacedAmmoPickupClasses[13];
var class<Weapon> U2WeaponClasses[13]; //GE: For default properties ONLY!
//var string U2WeaponPickupClassNames[13];
var string U2AmmoPickupClassNames[13]; //GE: For default properties ONLY!
var byte bIsVehicle[13], bNotVehicle[13]; //GE: For default properties ONLY!
var localized string U2WeaponDisplayText[33], U2WeaponDescText[33];
//var localized string GUISelectOptions[4];
//var config int FirePowerMode;
var config bool bExperimental;
var config bool bUseFieldGenerator;
var config bool bUseProximitySensor;
var config bool bIntegrateShieldReward;
var int IterationNum; //GE: Weapons.Length
const DamageMultiplier=1.818182;
var config int DamagePercentage;
var config bool bUseXMPFeel;
var config string FlashbangModeString;
struct WeaponInfo
{
    var class<Weapon> ReplacedWeaponClass; //GE: Generated from ReplacedWeaponClassName. This is what we replace.
    //var class<WeaponPickup> ReplacedWeaponPickupClass; //GE: UNUSED?!
    var class<Ammo> ReplacedAmmoPickupClass; //GE: Generated from ReplacedWeaponClassName.

    var class<Weapon> WeaponClass; //GE: This is the weapon we are going to put inside the world.
    var string WeaponPickupClassName; //GE: Generated from WeponClass.
    var string AmmoPickupClassName; //GE: Generated from WeaponClass.
    var bool bEnabled; //GE: Structs can't be defaultproperty'd, thus we still require bConfigUseU2WeaponX
    var bool bIsVehicle; //GE: This indicates that the weapon spawns a vehicle (deployable turrets). These only work in vehicle gametypes, duh.
    var bool bNotVehicle; //GE: Opposite of bIsVehicle, that is, only works in non-vehicle gametypes. Think shotgun.
};
var WeaponInfo Weapons[13];

/*
 * GE: Here we initialise the mutator. First of all, structs can't use defaultproperties (until UE3) and thus config, so here we need to initialise the structs.
 */
function PostBeginPlay()
{
    local int FireMode, x;
    //local string ReplacedWeaponPickupClassName;

    //IterationNum = ArrayCount(ReplacedWeaponClasses) - int(Level.Game.bAllowVehicles); //GE: He he, neat way to get the required number.
    IterationNum = ArrayCount(Weapons);

    for (x = 0; x < IterationNum; x++)
    {
        Weapons[x].bEnabled = bool(GetPropertyText("bConfigUseU2Weapon"$x)); //GE: GetPropertyText() is needed to use variables in an array-like fashion.
        //bUseU2Weapon[x] = byte(bool(GetPropertyText("bConfigUseU2Weapon"$x)));
        Weapons[x].ReplacedWeaponClass = class<Weapon>(DynamicLoadObject(GetPropertyText("ReplacedWeaponClassNames"$x),class'Class'));
        //ReplacedWeaponClasses[x] = class<Weapon>(DynamicLoadObject(GetPropertyText("ReplacedWeaponClassNames"$x),class'Class'));
        //ReplacedWeaponPickupClassName = string(ReplacedWeaponClasses[x].default.PickupClass);
        for(FireMode = 0; FireMode<2; FireMode++)
        {
            if( (Weapons[x].ReplacedWeaponClass.default.FireModeClass[FireMode] != None)
             && (Weapons[x].ReplacedWeaponClass.default.FireModeClass[FireMode].default.AmmoClass != None)
             && (Weapons[x].ReplacedWeaponClass.default.FireModeClass[FireMode].default.AmmoClass.default.PickupClass != None) )
            {
                Weapons[x].ReplacedAmmoPickupClass = class<Ammo>(Weapons[x].ReplacedWeaponClass.default.FireModeClass[FireMode].default.AmmoClass.default.PickupClass);
                break;
            }
        }
        Weapons[x].WeaponClass = U2WeaponClasses[x];
        Weapons[x].WeaponPickupClassName = string(Weapons[x].WeaponClass.default.PickupClass);
        Weapons[x].AmmoPickupClassName = U2AmmoPickupClassNames[x];
        Weapons[x].bIsVehicle = bool(bIsVehicle[x]);
        Weapons[x].bNotVehicle = bool(bNotVehicle[x]);
    }
    Super.PostBeginPlay();
}

/*
 * GE: Utility function for checking if we can replace the item with the weapon/ammo of choice.
 */
function bool ValidReplacement(int x)
{
    if (Level.Game.bAllowVehicles)
        return (Weapons[x].bEnabled && !Weapons[x].bNotVehicle );
    return (Weapons[x].bEnabled && !Weapons[x].bIsVehicle);
}

/*
 * GE: Here we replace things.
 */
function bool CheckReplacement( Actor Other, out byte bSuperRelevant )
{
    local int x, i;
    local WeaponLocker L;

    bSuperRelevant = 0;
    if (xWeaponBase(Other) != None)
    {
        for (x = 0; x < IterationNum; x++)
        {
            if (ValidReplacement(x) && xWeaponBase(Other).WeaponType == Weapons[x].ReplacedWeaponClass)
            {
                xWeaponBase(Other).WeaponType = Weapons[x].WeaponClass;
                return false;
            }
        }
        return true;
    }
    if (Weapon(Other) != None)
    {
        if ( Other.IsA('BallLauncher') )
                return true;
        for (x = 0; x < IterationNum; x++)
            if (ValidReplacement(x) && Other.Class == Weapons[x].ReplacedWeaponClass)
                return false;
        return true;
    }
    /*if (WeaponPickup(Other) != None) //GE: This should never happen.
    {
        for (x = 0; x < IterationNum; x++)
        {
            if (Weapons[x].bEnabled && Other.Class == Weapons[x].ReplacedWeaponPickupClass)
            {
                ReplaceWith(Other, U2WeaponPickupClassNames[x]);
                return false;
            }
        }
    } */
    if (Ammo(Other) != None)
    {
        for (x = 0; x < IterationNum; x++)
        {
            if (ValidReplacement(x) && Other.Class == Weapons[x].ReplacedAmmoPickupClass)
            {
                ReplaceWith(Other, Weapons[x].AmmoPickupClassName);
                return false;
            }
        }
        return true;
    }
    if (WeaponLocker(Other) != None)
    {
        L = WeaponLocker(Other);
        for (x = 0; x < IterationNum; x++)
            if (ValidReplacement(x))
                for (i = 0; i < L.Weapons.Length; i++)
                    if (L.Weapons[i].WeaponClass == Weapons[x].ReplacedWeaponClass)
                        L.Weapons[i].WeaponClass = Weapons[x].WeaponClass;
        return true;
    }

  //STARTING WEAPON
    if( xPawn(Other) != None )
		xPawn(Other).RequiredEquipment[0] = string(Weapons[1].WeaponClass);
    if( xPawn(Other) != None && bUseFieldGenerator == True && Level.Game.bAllowVehicles)
        xPawn(Other).RequiredEquipment[2] = "U2Weapons.U2WeaponFieldGenerator";
    if( xPawn(Other) != None && bUseProximitySensor == True && Level.Game.bAllowVehicles)
        xPawn(Other).RequiredEquipment[3] = "U2Weapons.U2ProximitySensorDeploy";

    //GE: Special handling - Shield Reward integration
    if (bIntegrateShieldReward && Other.IsA('ShieldReward'))
    {
        ShieldPack(Other).SetStaticMesh(StaticMesh'XMPWorldItemsM.items.Pickup_TD_001');
        ShieldPack(Other).Skins[0] = Shader'U2343T.Pickups.Energy_Pickup_B_FX_01';
        ShieldPack(Other).RepSkin = Shader'U2343T.Pickups.Energy_Pickup_B_FX_01';
        ShieldPack(Other).SetDrawScale(0.35);
        ShieldPack(Other).SetCollisionSize(27.0, 4.0);
        ShieldPack(Other).PickupMessage = "You got an Energy Pickup.";
        ShieldPack(Other).PickupSound = Sound'U2A.Powerups.EnergyPowerUp';
    }

        return Super.CheckReplacement(Other,bSuperRelevant);
}

/*
 * GE: This is for further replacement, I think...
 */
function string GetInventoryClassOverride(string InventoryClassName)
{
    local int x;

    for (x = 0; x < IterationNum; x++)
    {
        if (InventoryClassName ~= string(Weapons[x].ReplacedWeaponClass) && ValidReplacement(x))
            return string(Weapons[x].WeaponClass);
    }

    return Super.GetInventoryClassOverride(InventoryClassName);
}

/*
 * GE: Configuration options.
 */
static function FillPlayInfo(PlayInfo PlayInfo)
{
    local array<CacheManager.WeaponRecord> Recs;
    local string WeaponOptions;
    local int i;

    Super.FillPlayInfo(PlayInfo);

    class'CacheManager'.static.GetWeaponList(Recs);
    for (i = 0; i < Recs.Length; i++)
    {
        if (WeaponOptions != "")
            WeaponOptions $= ";";

        WeaponOptions $= Recs[i].ClassName $ ";" $ Recs[i].FriendlyName;
    }

    PlayInfo.AddSetting(default.RulesGroup, "bConfigUseU2Weapon0", default.U2WeaponDisplayText[0], 0, 3, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "ReplacedWeaponClassNames0", default.U2WeaponDisplayText[1], 0, 4, "Select", WeaponOptions);
    PlayInfo.AddSetting(default.RulesGroup, "bConfigUseU2Weapon1", default.U2WeaponDisplayText[2], 0, 5, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "ReplacedWeaponClassNames1", default.U2WeaponDisplayText[3], 0, 6, "Select", WeaponOptions);
    PlayInfo.AddSetting(default.RulesGroup, "bConfigUseU2Weapon2", default.U2WeaponDisplayText[6], 0, 7, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "ReplacedWeaponClassNames2", default.U2WeaponDisplayText[7], 0, 8, "Select", WeaponOptions);
    PlayInfo.AddSetting(default.RulesGroup, "bConfigUseU2Weapon3", default.U2WeaponDisplayText[8], 0, 9, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "ReplacedWeaponClassNames3", default.U2WeaponDisplayText[9], 0, 10, "Select", WeaponOptions);
    PlayInfo.AddSetting(default.RulesGroup, "bConfigUseU2Weapon4", default.U2WeaponDisplayText[10], 0, 11, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "ReplacedWeaponClassNames4", default.U2WeaponDisplayText[11], 0, 12, "Select", WeaponOptions);
    PlayInfo.AddSetting(default.RulesGroup, "bConfigUseU2Weapon5", default.U2WeaponDisplayText[12], 0, 13, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "ReplacedWeaponClassNames5", default.U2WeaponDisplayText[13], 0, 14, "Select", WeaponOptions);
    PlayInfo.AddSetting(default.RulesGroup, "bConfigUseU2Weapon6", default.U2WeaponDisplayText[14], 0, 15, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "ReplacedWeaponClassNames6", default.U2WeaponDisplayText[15], 0, 16, "Select", WeaponOptions);
    PlayInfo.AddSetting(default.RulesGroup, "bConfigUseU2Weapon7", default.U2WeaponDisplayText[16], 0, 17, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "ReplacedWeaponClassNames7", default.U2WeaponDisplayText[17], 0, 18, "Select", WeaponOptions);
    PlayInfo.AddSetting(default.RulesGroup, "bConfigUseU2Weapon8", default.U2WeaponDisplayText[18], 0, 19, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "ReplacedWeaponClassNames8", default.U2WeaponDisplayText[19], 0, 20, "Select", WeaponOptions);
    PlayInfo.AddSetting(default.RulesGroup, "bConfigUseU2Weapon9", default.U2WeaponDisplayText[20], 0, 21, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "ReplacedWeaponClassNames9", default.U2WeaponDisplayText[21], 0, 22, "Select", WeaponOptions);
    PlayInfo.AddSetting(default.RulesGroup, "bConfigUseU2Weapon10", default.U2WeaponDisplayText[22], 0, 23, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "ReplacedWeaponClassNames10", default.U2WeaponDisplayText[23], 0, 24, "Select", WeaponOptions);
    PlayInfo.AddSetting(default.RulesGroup, "bConfigUseU2Weapon11", default.U2WeaponDisplayText[24], 0, 25, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "ReplacedWeaponClassNames11", default.U2WeaponDisplayText[25], 0, 26, "Select", WeaponOptions);
    PlayInfo.AddSetting(default.RulesGroup, "bConfigUseU2Weapon12", default.U2WeaponDisplayText[26], 0, 27, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "ReplacedWeaponClassNames12", default.U2WeaponDisplayText[27], 0, 28, "Select", WeaponOptions);
    PlayInfo.AddSetting(default.RulesGroup, "bUseXMPFeel", default.U2WeaponDisplayText[4], 0, 29, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "DamagePercentage", default.U2WeaponDisplayText[30], 0, 30, "Text", "3;0:100");
    PlayInfo.AddSetting(default.RulesGroup, "bExperimental", default.U2WeaponDisplayText[5], 0, 31, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "bUseFieldGenerator", default.U2WeaponDisplayText[28], 0, 32, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "bUseProximitySensor", default.U2WeaponDisplayText[29], 0, 33, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "bIntegrateShieldReward", default.U2WeaponDisplayText[31], 0, 34, "Check");
    PlayInfo.AddSetting(default.RulesGroup, "FlashbangModeString", default.U2WeaponDisplayText[32], 0, 35, "Select", "FM_None;None;FM_Directional;Direction-based;FM_DistanceBased;Distance-based");
}

/*
 * GE: Configuration tooltips.
 */
static event string GetDescriptionText(string PropName)
{
    if (PropName == "bConfigUseU2Weapon0")
        return default.U2WeaponDescText[0];
    if (PropName == "ReplacedWeaponClassNames0")
        return default.U2WeaponDescText[1];
    if (PropName == "bConfigUseU2Weapon1")
        return default.U2WeaponDescText[2];
    if (PropName == "ReplacedWeaponClassNames1")
        return default.U2WeaponDescText[3];
    if (PropName == "bConfigUseU2Weapon2")
        return default.U2WeaponDescText[6];
    if (PropName == "ReplacedWeaponClassNames2")
        return default.U2WeaponDescText[7];
    if (PropName == "bConfigUseU2Weapon3")
        return default.U2WeaponDescText[8];
    if (PropName == "ReplacedWeaponClassNames3")
        return default.U2WeaponDescText[9];
    if (PropName == "bConfigUseU2Weapon4")
        return default.U2WeaponDescText[10];
    if (PropName == "ReplacedWeaponClassNames4")
        return default.U2WeaponDescText[11];
    if (PropName == "bConfigUseU2Weapon5")
        return default.U2WeaponDescText[12];
    if (PropName == "ReplacedWeaponClassNames5")
        return default.U2WeaponDescText[13];
    if (PropName == "bConfigUseU2Weapon6")
        return default.U2WeaponDescText[14];
    if (PropName == "ReplacedWeaponClassNames6")
        return default.U2WeaponDescText[15];
    if (PropName == "bConfigUseU2Weapon7")
        return default.U2WeaponDescText[16];
    if (PropName == "ReplacedWeaponClassNames7")
        return default.U2WeaponDescText[17];
    if (PropName == "bConfigUseU2Weapon8")
        return default.U2WeaponDescText[18];
    if (PropName == "ReplacedWeaponClassNames8")
        return default.U2WeaponDescText[19];
    if (PropName == "bConfigUseU2Weapon9")
        return default.U2WeaponDescText[20];
    if (PropName == "ReplacedWeaponClassNames9")
        return default.U2WeaponDescText[21];
    if (PropName == "bConfigUseU2Weapon10")
        return default.U2WeaponDescText[22];
    if (PropName == "ReplacedWeaponClassNames10")
        return default.U2WeaponDescText[23];
    if (PropName == "bConfigUseU2Weapon11")
        return default.U2WeaponDescText[24];
    if (PropName == "ReplacedWeaponClassNames11")
        return default.U2WeaponDescText[25];
    if (PropName == "bConfigUseU2Weapon12")
        return default.U2WeaponDescText[26];
    if (PropName == "ReplacedWeaponClassNames12")
        return default.U2WeaponDescText[27];
    if (PropName == "bUseXMPFeel")
        return default.U2WeaponDescText[4];
    if (PropName == "bExperimental")
        return default.U2WeaponDescText[5];
    if (PropName == "bUseFieldGenerator")
        return default.U2WeaponDescText[28];
    if (PropName == "bUseProximitySensor")
        return default.U2WeaponDescText[29];
    if (PropName == "DamagePercentage")
        return default.U2WeaponDescText[30];
    if (PropName == "bIntegrateShieldReward")
        return default.U2WeaponDescText[31];
    if (PropName == "FlashbangModeString")
        return default.U2WeaponDescText[32];

    return Super.GetDescriptionText(PropName);
}

/*
 * GE: Here we set all the properties for different play styles.
 */
event PreBeginPlay()
{
      //local int x;
      //local class<Weapon> Weapons;
      local float k; //GE: Multiplier.

      Super.PreBeginPlay();

      k=float(DamagePercentage)/100.0;
      //log("MutU2Weapons: k is"@k);
      //Sets various settings that match different games

      /*if (FirePowerMode == 1) { //Original XMP - use the UTXMP original values
        k=1;
      }
      else */if (!bUseXMPFeel && DamagePercentage != class'MutU2Weapons'.default.DamagePercentage) { //Original U2; compensate for float division errors
        Class'U2Weapons.U2AssaultRifleFire'.default.DamageMin *= DamageMultiplier * k;
        Class'U2Weapons.U2AssaultRifleFire'.default.DamageMax *= DamageMultiplier * k;
        Class'U2Weapons.U2AssaultRifleProjAlt'.default.Damage *= DamageMultiplier * k;
        Class'U2Weapons.U2ProjectileASExplAlt'.default.Damage *= DamageMultiplier * k;
        Class'U2Weapons.U2ProjectileAltEnergyRifle'.default.Damage *= DamageMultiplier * k;
        Class'U2Weapons.U2ProjectileEnergyRifle'.default.Damage *= DamageMultiplier * k;
        Class'U2Weapons.U2FireFlameThrower'.default.DamageMin *= DamageMultiplier * k;
        Class'U2Weapons.U2FireFlameThrower'.default.DamageMax *= DamageMultiplier * k;
        Class'U2Weapons.U2ProjectileFragGrenade'.default.Damage *= DamageMultiplier * k;
        Class'U2Weapons.U2ProjectileIncendiaryGrenade'.default.Damage *= DamageMultiplier * k;
        Class'U2Weapons.U2ProjectileConcussionGrenade'.default.Damage *= DamageMultiplier * k;
        Class'U2Weapons.U2ProjectileRocketDrunken'.default.Damage *= DamageMultiplier * k;
        Class'U2Weapons.U2ProjectileRocketSeeking'.default.Damage *= DamageMultiplier * k;
        Class'U2Weapons.U2ProjectileRocket'.default.Damage *= DamageMultiplier * k;
        Class'U2Weapons.U2ProjectileAltShotgun'.default.Damage *= DamageMultiplier * k;
        Class'U2Weapons.U2FireShotgun'.default.DamageMin *= DamageMultiplier * k;
        Class'U2Weapons.U2FireShotgun'.default.DamageMax *= DamageMultiplier * k;
        Class'U2Weapons.U2FireSniper'.default.DamageMin *= DamageMultiplier * k;
        Class'U2Weapons.U2FireSniper'.default.DamageMax *= DamageMultiplier * k;
        Class'U2Weapons.U2FirePistol'.default.DamageMin *= DamageMultiplier * k;
        Class'U2Weapons.U2FirePistol'.default.DamageMax *= DamageMultiplier * k;
        Class'U2Weapons.U2FireAltPistol'.default.DamageMin *= DamageMultiplier * k;
        Class'U2Weapons.U2FireAltPistol'.default.DamageMax *= DamageMultiplier * k;
        Class'U2Weapons.U2ProjectileEMPGrenade'.default.Damage *= DamageMultiplier * k;
        Class'U2Weapons.U2ProjectileToxicGrenade'.default.Damage *= DamageMultiplier * k;
        Class'U2Weapons.U2HurterProxy_Gas'.default.myDamage *= DamageMultiplier * k;
        Class'U2Weapons.U2HurterProxy_Fire'.default.myDamage *= DamageMultiplier * k;
      }
      //Dampened U2 is already the default - no need for a rewrite?
      else if (bUseXMPFeel) { //General XMP
        Class'U2Weapons.U2AssaultRifleFire'.default.Spread = 6.0;
        Class'U2Weapons.U2AssaultRifleAmmoInv'.default.MaxAmmo = 300;
        Class'U2Weapons.U2AssaultRifleProjAlt'.default.Speed = 5000;
        Class'U2Weapons.U2AssaultRifleProjAlt'.default.MomentumTransfer = 8000;
        Class'U2Weapons.U2WeaponEnergyRifle'.default.ClipSize = 30;
        Class'U2Weapons.U2FireAltEnergyRifle'.default.FireLastReloadTime = 2.265000;
        Class'U2Weapons.U2ProjectileAltEnergyRifle'.default.Speed = 1400.000000;
        Class'U2Weapons.U2ProjectileAltEnergyRifle'.default.DamageRadius = 290.000000;
        Class'U2Weapons.U2ProjectileAltEnergyRifle'.default.LifeSpan = 6.0;
        Class'U2Weapons.U2ProjectileEnergyRifle'.default.ShakeRadius = 1024.0;
        Class'U2Weapons.U2ProjectileEnergyRifle'.default.ShakeMagnitude = 1.0;
        Class'U2Weapons.U2ProjectileEnergyRifle'.default.Speed = 2500.0;
        Class'U2Weapons.U2ProjectileEnergyRifle'.default.MaxSpeed = 5000.0;
        Class'U2Weapons.U2ProjectileEnergyRifle'.default.LifeSpan = 6.0;
        Class'U2Weapons.U2AmmoEnergyRifle'.default.MaxAmmo = 150;
        Class'U2Weapons.U2FireAltFlameThrower'.default.FireRate = 0.25;
        Class'U2Weapons.U2ProjectileAltFlameThrower'.default.MaxSpeed = 500.0;
        Class'U2Weapons.U2ProjectileAltFlameThrower'.default.LifeSpan = 22.0;
        Class'U2Weapons.U2ProjectileAltFlameThrower'.default.MomentumTransfer = 500.0;
        Class'U2Weapons.U2WeaponFlameThrower'.default.ClipSize = 80;
        Class'U2Weapons.U2WeaponFlameThrower'.default.ReloadTime = 2.0;
        Class'U2Weapons.U2AmmoFlameThrower'.default.MaxAmmo = 480;
        Class'U2Weapons.U2ProjectileFragGrenade'.default.MomentumTransfer = 9000;
        Class'U2Weapons.U2ProjectileFragGrenade'.default.MaxSpeed = 2600.0;
        Class'U2Weapons.U2ProjectileFragGrenade'.default.Speed = 2600.0;
        Class'U2Weapons.U2ProjectileSmokeGrenade'.default.MaxSpeed = 2600.0;
        Class'U2Weapons.U2ProjectileSmokeGrenade'.default.Speed = 2600.0;
        Class'U2Weapons.U2ProjectileIncendiaryGrenade'.default.DamageRadius = 256.0;
        Class'U2Weapons.U2ProjectileIncendiaryGrenade'.default.MomentumTransfer = 9000.0;
        Class'U2Weapons.U2ProjectileConcussionGrenade'.default.DamageRadius = 180.0;
        Class'U2Weapons.U2FireGrenade'.default.FireRate = 1.33;
        Class'U2Weapons.U2WeaponRocketLauncher'.default.ReloadTime = 2.0;
        Class'U2Weapons.U2AmmoRocketLauncher'.default.MaxAmmo = 25;
        Class'U2Weapons.U2ProjectileRocketDrunken'.default.Speed = 1200.0;
        Class'U2Weapons.U2ProjectileRocketSeeking'.default.Speed = 1200.0;
        Class'U2Weapons.U2ProjectileRocket'.default.Speed = 2133.0;//3200 is too much
        Class'U2Weapons.U2ProjectileRocket'.default.MaxSpeed = 2133.0;
        Class'U2Weapons.U2ProjectileRocket'.default.DamageRadius = 384.0;
        Class'U2Weapons.U2WeaponShotgun'.default.ReloadTime = 2.21;
        Class'U2Weapons.U2WeaponShotgun'.default.ClipSize = 6;
        Class'U2Weapons.U2AmmoShotgun'.default.MaxAmmo = 42;
        Class'U2Weapons.U2WeaponSniper'.default.ClipSize = 3;
        Class'U2Weapons.U2FireSniper'.default.FireRate = 1.0;
        Class'U2Weapons.U2AmmoSniper'.default.MaxAmmo = 18;
        Class'U2Weapons.U2FirePistol'.default.FireLastReloadTime = 2.4;
        Class'U2Weapons.U2FireAltPistol'.default.FireLastReloadTime = 2.4;
        Class'U2Weapons.U2AmmoPistol'.default.MaxAmmo = 45;
        Class'U2Weapons.U2DamTypePistol'.default.VehicleDamageScaling = 0.30;
        Class'U2Weapons.U2DamTypeAltPistol'.default.VehicleDamageScaling = 0.30;

        Class'U2Weapons.U2AssaultRifleFire'.default.DamageMin = 20*k;
        Class'U2Weapons.U2AssaultRifleFire'.default.DamageMax = 20*k;
        Class'U2Weapons.U2AssaultRifleProjAlt'.default.Damage = 175*k;
        Class'U2Weapons.U2ProjectileASExplAlt'.default.Damage = 64.0*k;
        Class'U2Weapons.U2ProjectileAltEnergyRifle'.default.Damage = 120.0*k;
        Class'U2Weapons.U2ProjectileEnergyRifle'.default.Damage = 13.0*k;
        Class'U2Weapons.U2FireFlameThrower'.default.DamageMin = 15*k;
        Class'U2Weapons.U2FireFlameThrower'.default.DamageMax = 15*k;
        Class'U2Weapons.U2ProjectileFragGrenade'.default.Damage = 200.0*k;
        Class'U2Weapons.U2ProjectileIncendiaryGrenade'.default.Damage = 50.0*k;
        Class'U2Weapons.U2ProjectileConcussionGrenade'.default.Damage = 15.0*k;
        Class'U2Weapons.U2ProjectileRocketDrunken'.default.Damage = 70.0*k;
        Class'U2Weapons.U2ProjectileRocketSeeking'.default.Damage = 70.0*k;
        Class'U2Weapons.U2ProjectileRocket'.default.Damage = 190.0*k;
        Class'U2Weapons.U2ProjectileAltShotgun'.default.Damage = 22.0*k;
        Class'U2Weapons.U2FireShotgun'.default.DamageMin = 16*k;
        Class'U2Weapons.U2FireShotgun'.default.DamageMax = 16*k;
        Class'U2Weapons.U2FireSniper'.default.DamageMin = 75*k;
        Class'U2Weapons.U2FireSniper'.default.DamageMax = 75*k;
        Class'U2Weapons.U2FirePistol'.default.DamageMin = 40*k;
        Class'U2Weapons.U2FirePistol'.default.DamageMax = 40*k;
        Class'U2Weapons.U2FireAltPistol'.default.DamageMin = 65*k;
        Class'U2Weapons.U2FireAltPistol'.default.DamageMax = 65*k;
        Class'U2Weapons.U2ProjectileEMPGrenade'.default.Damage = 140.0*k;
        Class'U2Weapons.U2ProjectileToxicGrenade'.default.Damage = 100.0*k;
        Class'U2Weapons.U2HurterProxy_Gas'.default.myDamage = 30.0*k;
        Class'U2Weapons.U2HurterProxy_Fire'.default.myDamage = 80.0*k;
      }

      //
      //-----------------------------------------------------------
      //
      //Experimental options - lets you Unuse EMPimp projectile and fire from two CAR barrels
      if ((bExperimental) && (!bUseXMPFeel)) { //General U2
        Class'U2Weapons.U2ProjectileAltEnergyRifle'.default.LifeSpan = 2.0;
        Class'U2Weapons.U2ProjectileAltEnergyRifle'.default.Damage = 210.0*k;
      }
      if (bExperimental) { //CAR - nothing's setting it, FireMode independent
        Class'U2Weapons.U2AssaultRifleFire'.default.bModeExclusive = false;
        Class'U2Weapons.U2AssaultRifleAltFire'.default.bModeExclusive = false;
      }

      class'U2ProjectileConcussionGrenade'.static.SetFlashbangMode(FlashbangModeString);
}

defaultproperties
{
     ReplacedWeaponClassNames0="XWeapons.Minigun"
     ReplacedWeaponClassNames1="XWeapons.AssaultRifle"
     ReplacedWeaponClassNames2="XWeapons.BioRifle"
     ReplacedWeaponClassNames3="XWeapons.ShockRifle"
     ReplacedWeaponClassNames4="Onslaught.ONSGrenadeLauncher"
     ReplacedWeaponClassNames5="XWeapons.RocketLauncher"
     ReplacedWeaponClassNames6="XWeapons.FlakCannon"
     ReplacedWeaponClassNames7="XWeapons.SniperRifle"
     ReplacedWeaponClassNames8="UTClassic.ClassicSniperRifle"
     ReplacedWeaponClassNames9="Onslaught.ONSMineLayer"
     ReplacedWeaponClassNames10="XWeapons.Redeemer"
     ReplacedWeaponClassNames11="XWeapons.Painter"
     ReplacedWeaponClassNames12="XWeapons.LinkGun"
     bConfigUseU2Weapon0=True
     bConfigUseU2Weapon1=True
     bConfigUseU2Weapon2=True
     bConfigUseU2Weapon3=True
     bConfigUseU2Weapon4=True
     bConfigUseU2Weapon5=True
     bConfigUseU2Weapon6=True
     bConfigUseU2Weapon7=True
     bConfigUseU2Weapon8=True
     bConfigUseU2Weapon9=True
     bConfigUseU2Weapon10=True
     bConfigUseU2Weapon11=True
     bConfigUseU2Weapon12=True
     bUseFieldGenerator=True
     bUseProximitySensor=True
     U2WeaponClasses(0)=Class'U2Weapons.U2AssaultRifleInv'
     U2WeaponClasses(1)=Class'U2Weapons.U2WeaponEnergyRifle'
     U2WeaponClasses(2)=Class'U2Weapons.U2WeaponFlameThrower'
     U2WeaponClasses(3)=Class'U2Weapons.U2WeaponPistol'
     U2WeaponClasses(4)=Class'U2Weapons.U2AutoTurretDeploy'
     U2WeaponClasses(5)=Class'U2Weapons.U2WeaponRocketLauncher'
     U2WeaponClasses(6)=Class'U2Weapons.U2WeaponGrenadeLauncher'
     U2WeaponClasses(7)=Class'U2Weapons.U2WeaponSniper'
     U2WeaponClasses(8)=Class'U2Weapons.U2WeaponSniper'
     U2WeaponClasses(9)=Class'U2Weapons.U2WeaponRocketTurret'
     U2WeaponClasses(10)=Class'U2Weapons.U2WeaponLandMine'
     U2WeaponClasses(11)=Class'U2Weapons.U2WeaponLaserTripMine'
     U2WeaponClasses(12)=Class'U2Weapons.U2WeaponShotgun' //GE: Has to be in !Level.Game.bAllowVehicles
     bIsVehicle(0)=0
     bIsVehicle(1)=0
     bIsVehicle(2)=0
     bIsVehicle(3)=0
     bIsVehicle(4)=1
     bIsVehicle(5)=0
     bIsVehicle(6)=0
     bIsVehicle(7)=0
     bIsVehicle(8)=0
     bIsVehicle(9)=1
     bIsVehicle(10)=1
     bIsVehicle(11)=1
     bIsVehicle(12)=0
     bNotVehicle(12)=1
     U2AmmoPickupClassNames(0)="U2Weapons.U2AssaultRifleAmmoPickup"
     U2AmmoPickupClassNames(1)="U2Weapons.U2PickupAmmoEnergyRifle"
     U2AmmoPickupClassNames(2)="U2Weapons.U2PickupAmmoFlameThrower"
     U2AmmoPickupClassNames(3)="U2Weapons.U2PickupAmmoPistol"
     U2AmmoPickupClassNames(4)="U2Weapons.U2PickupAutoTurret"
     U2AmmoPickupClassNames(5)="U2Weapons.U2PickupAmmoRocket"
     U2AmmoPickupClassNames(6)="U2Weapons.U2PickupAmmoGrenade"
     U2AmmoPickupClassNames(7)="U2Weapons.U2PickupAmmoSniper"
     U2AmmoPickupClassNames(8)="U2Weapons.U2PickupAmmoSniper"
     U2AmmoPickupClassNames(9)="U2Weapons.U2PickupRocketTurret"
     U2AmmoPickupClassNames(10)="U2Weapons.U2PickupLandMine"
     U2AmmoPickupClassNames(11)="U2Weapons.U2PickupLaserTripMine"
     U2AmmoPickupClassNames(12)="U2Weapons.U2PickupAmmoShotgun"
     U2WeaponDisplayText(0)="Include the Combat Assault Rifle"
     U2WeaponDisplayText(1)="Replace this with the M32 Duster CAR"
     U2WeaponDisplayText(2)="Include the Shock Lance"
     U2WeaponDisplayText(3)="Replace this with the Shock Lance"
     U2WeaponDisplayText(4)="Use XMP-style fire power?"
     U2WeaponDisplayText(5)="Use alternative options?"
     U2WeaponDisplayText(6)="Include the Flamethrower"
     U2WeaponDisplayText(7)="Replace this with the Vulcan"
     U2WeaponDisplayText(8)="Include the Magnum Pistol"
     U2WeaponDisplayText(9)="Replace this with the Magnum Pistol"
     U2WeaponDisplayText(10)="Include the Auto Turret"
     U2WeaponDisplayText(11)="Replace this with the Auto Turret"
     U2WeaponDisplayText(12)="Include the Shark Rocket Launcher"
     U2WeaponDisplayText(13)="Replace this with the Shark"
     U2WeaponDisplayText(14)="Include the Grenade Launcher"
     U2WeaponDisplayText(15)="Replace this with the Hydra"
     U2WeaponDisplayText(16)="Include the Widowmaker Sniper (1)"
     U2WeaponDisplayText(17)="Replace this with the Widowmaker Rifle"
     U2WeaponDisplayText(18)="Include the Widowmaker Sniper (2)"
     U2WeaponDisplayText(19)="Replace this with the Widowmaker Rifle too"
     U2WeaponDisplayText(20)="Include the Rocket Turret"
     U2WeaponDisplayText(21)="Replace this with the Rocket Turret"
     U2WeaponDisplayText(22)="Include the Land Mine"
     U2WeaponDisplayText(23)="Replace this with the Land Mine"
     U2WeaponDisplayText(24)="Include the Laser Trip Mine"
     U2WeaponDisplayText(25)="Replace this with the Laser Trip Mine"
     U2WeaponDisplayText(26)="Include the Crowd Pleaser Shotgun"
     U2WeaponDisplayText(27)="Replace this with the Crowd Pleaser"
     U2WeaponDisplayText(28)="Include the Field Generator"
     U2WeaponDisplayText(29)="Include the Proximity Sensor"
     U2WeaponDisplayText(30)="Firepower damping percentage"
     U2WeaponDisplayText(31)="Integrate with Shield Reward"
     U2WeaponDisplayText(32)="Concussion grenade behaviour"
     U2WeaponDescText(0)="Include the M32 Duster Combat Assault Rifle in the game, i.e. enable it?"
     U2WeaponDescText(1)="What weapon should be replaced with the CAR. By default it's the Minigun."
     U2WeaponDescText(2)="Enable the Shock Lance Energy Rifle?"
     U2WeaponDescText(3)="What weapon should be replaced with the Energy Rifle. By default it's the Assault Rifle. NOTE: Changing this value is not recommended."
     U2WeaponDescText(4)="If enabled, this option will make all weapon firepower the same as in Unreal II XMP; if not, the firepower is consistent with Unreal II SP."
     U2WeaponDescText(5)="If enabled, will make the Shock Lance use another, limited, secondary fire mode and allow Combat Assault Rifle use Unreal: Return to Na Pali fire style (shooting out of 2 barrels)."
     U2WeaponDescText(6)="Enable the Vulcan Flamethrower?"
     U2WeaponDescText(7)="What weapon should be replaced with the Flamethrower. By default it's the Bio Rifle."
     U2WeaponDescText(8)="Enable the Magnum Pistol?"
     U2WeaponDescText(9)="What weapon should be replaced with the Magnum Pistol. By default it's the Shock Rifle."
     U2WeaponDescText(10)="Enable the Automatic Turret?"
     U2WeaponDescText(11)="What weapon should be replaced with the Auto Turret. By default it's the Onslaught Grenade Launcher."
     U2WeaponDescText(12)="Enable the Shark Rocket Launcher?"
     U2WeaponDescText(13)="What weapon should be replaced with the Shark Rocket Launcher. By default it's the Rocket Launcher."
     U2WeaponDescText(14)="Enable the Hydra Grenade Launcher?"
     U2WeaponDescText(15)="What weapon should be replaced with the Hydra Grenade Launcher. By default it's the Flak Cannon."
     U2WeaponDescText(16)="Should the Lightning Gun be replaced with the Widowmaker Sniper Rifle?"
     U2WeaponDescText(17)="What weapon should be replaced with the Widowmaker Sniper Rifle. By default it's the Lightning Gun here."
     U2WeaponDescText(18)="Should the Classic Sniper Rifle be replaced with the Widowmaker Sniper Rifle?"
     U2WeaponDescText(19)="What weapon should be replaced with the Widowmaker Sniper Rifle. By default it's the Classic Sniper Rifle here."
     U2WeaponDescText(20)="Enable the Rocket Turret delpoyable?"
     U2WeaponDescText(21)="What weapon should be replaced with the Rocket Turret deployable. By default it's the Mine Layer."
     U2WeaponDescText(22)="Enable the Land Mine?"
     U2WeaponDescText(23)="What weapon should be replaced with the Land Mine. By default it's the Redeemer."
     U2WeaponDescText(24)="Enable the Laser Trip Mine?"
     U2WeaponDescText(25)="What weapon should be replaced with the Laser Trip Mine. By default it's the Ion Painter."
     U2WeaponDescText(26)="Enable the Crowd Pleaser Shotgun? It won't replace the Link Gun in matches with vehicles."
     U2WeaponDescText(27)="What weapon should be replaced with the Crowd Pleaser Shotgun. By default it's the Link Gun. It does not replace it in vehicle matches."
     U2WeaponDescText(28)="Enable the Field Generator? If enabled, you start with one."
     U2WeaponDescText(29)="Enable the Proximity Sensor? If enabled, you start with one."
     U2WeaponDescText(30)="This number controls how powerful all weapons are. By deafult the firepower is set to 55% of the original in order to compensate for the fact that players in UT2004 don't have shields or damage filtering."
     U2WeaponDescText(31)="If checked, the Shield Reward mutator produces Unreal II shield pickups."
     U2WeaponDescText(32)="Choose between no white overlay, overlay depending on the player's view (XMP style) and overlay depending on the distance from the player (default, foolproof)."
     //FirePowerMode=4
     DamagePercentage=55
     bUseXMPFeel=False
     bIntegrateShieldReward=True
     FlashbangModeString="FM_DistanceBased"
     GroupName="Arena"
     FriendlyName="Unreal II or XMP Weapons"
     Description="Add the Unreal II weapons to other gametypes. Fully customisable, you can choose between Unreal II and XMP weapon behaviour."
}
