#
# License: GPL 2.0
#
# Install: Include this code into an aircraft to make it damagable. (remember to add it to the -set file)
#          for damage to be recognised, the property /payload/armament/msg must be 1
#
# Authors: Nikolai V. Chr., Pinto and Richard (with improvement by Onox)
#
#


############################ Config ########################################################################################
var full_damage_dist_m = 1.25;# Can vary from aircraft to aircraft depending on how many failure modes it has.
                           # Many modes (like Viggen) ought to have lower number like zero.
                           # Few modes (like F-14) ought to have larger number such as 3.
                           # For assets this should be average radius of the asset.
var use_hitpoints_instead_of_failure_modes_bool = 0;# mainly used by assets that don't have failure modes.
var hp_max = 80;# given a direct hit, how much pounds of warhead is needed to kill. Only used if hitpoints is enabled.
var hitable_by_air_munitions = 1;   # if anti-air can do damage
var hitable_by_cannon = 1;          # if cannon can do damage
#var hitable_by_ground_munitions = 1;# if anti-ground/marine can do damage
var is_fleet = 0;  # Is really 7 ships, 3 of which has offensive missiles.
var rwr_to_screen=0; # for aircraft that do not yet have proper RWR
var tacview_supported=1; # For aircraft with tacview support
var m28_auto=0; # only used by automats
var mlw_max=2.25; # 
var auto_flare_caller = 0; # If damage.nas should detect flare releases, or if function is called from somewhere in aircraft
############################################################################################################################

var TRUE  = 1;
var FALSE = 0;

var hp = hp_max;
setprop("sam/damage", math.max(0,100*hp/hp_max));#used in HUD

var shells = {
    # [id,damage,(name)]
    #
    # 0.20 means a direct hit will disable 20% of the failure modes on average.
    # or, 0.20 also means a direct hit can do 20 hitpoints damage.
    #
    "M70 rocket":        [0,0.250], #135mm
    "S-5 rocket":        [1,0.200], # 55mm
    "M55 shell":         [2,0.100], # 30mm
    "KCA shell":         [3,0.100], # 30mm
    "GSh-30":            [4,0.100], # 30mm mig29/su27
    "GAU-8/A":           [5,0.100], # 30mm
    "Mk3Z":              [6,0.100], # 30mm Jaguar
    "BK27":              [7,0.070], # 27mm
    "GSh-23":            [8,0.065], # 23mm
    "M61A1 shell":       [9,0.050], # 20mm F14, F15, F16
    "50 BMG":            [10,0.015], # 12.7mm (non-explosive)    
    "7.62":              [11,0.005], # 7.62mm (non-explosive)
    "Hydra-70":          [12,0.250], # F-16/A-6 LAU-68 and LAU-61
    "SNEB":              [13,0.250], # Jaguar   
    "DEFA 554":          [14,0.100], # 30mm Mirage
    "20mm APDS":         [15,0.050], # CIWS
    "LAU-10":            [16,0.225], # 127mm
};    

# lbs of warheads is explosive+fragmentation+fuse, so total warhead mass.

var warheads = {
    # [id,lbs,anti surface,cluster,(name)]
    "AGM-65B":           [ 0,  126.00,1,0],
    "AGM-84":            [ 1,  488.00,1,0],
    "AGM-88":            [ 2,  146.00,1,0],
    "MK-82SE":           [ 3,  192.00,1,0],# snake eye
    "AGM-119":           [ 4,  264.50,1,0],
    "AGM-154A":          [ 5,  493.00,1,0],
    "AGM-158":           [ 6, 1000.00,1,0],
    "ALARM":             [ 7,  450.00,1,0],
    "AM 39 Exocet":      [ 8,  364.00,1,0], 
    "AS 37 Martel":      [ 9,  330.00,1,0],# Also : AJ 168 Martel 
    "AS30L":             [10,  529.00,1,0],
    "BL755":             [11,  100.00,1,1],# 800lb bomblet warhead. Mix of armour piecing and HE. 100 due to need to be able to kill buk-m2.    
    "CBU-87":            [12,  100.00,1,1],# bomblet warhead. Mix of armour piecing and HE. 100 due to need to be able to kill buk-m2.    
    "CBU-105":           [13,  100.00,1,1],# bomblet warhead. Mix of armour piecing and HE. 100 due to need to be able to kill buk-m2.    
    "AS 37 Armat":       [14,  330.00,1,0],
    "FAB-100":           [15,   92.59,1,0],
    "FAB-250":           [16,  202.85,1,0],
    "FAB-500":           [17,  564.38,1,0],
    "GBU-12":            [18,  190.00,1,0],
    "GBU-24":            [19,  945.00,1,0],
    "GBU-31":            [20,  945.00,1,0],
    "GBU-54":            [21,  190.00,1,0],
    "GBU-10":            [22, 2000.00,1,0],
    "GBU-16":            [23,  450.00,1,0],
    "HVAR":              [24,    7.50,1,0],#P51
    "KAB-500":           [25,  564.38,1,0],
    "Kh-25MP":           [26,  197.53,1,0],
    "Kh-66":             [27,  244.71,1,0],
    "LAU-68":            [28,   10.00,1,0],
    "M71":               [29,  200.00,1,0],
    "M71R":              [30,  200.00,1,0],
    "M90":               [31,   10.00,1,1],# bomblet warhead. x3 of real mass.
    "MK-82":             [32,  192.00,1,0],
    "MK-83":             [33,  445.00,1,0],
    "MK-83HD":           [34,  445.00,1,0],
    "MK-84":             [35,  945.00,1,0],
    "OFAB-100":          [36,   92.59,1,0],
    "RB-04E":            [37,  661.00,1,0],
    "RB-15F":            [38,  440.92,1,0],
    "RB-75":             [39,  126.00,1,0],
    "RN-14T":            [40,  800.00,1,0], #fictional, thermobaeric replacement for the RN-24 nuclear bomb
    "RN-18T":            [41, 1200.00,1,0], #fictional, thermobaeric replacement for the RN-28 nuclear bomb
    "RS-2US":            [42,   28.66,1,0],
    "S-21":              [43,  245.00,1,0],
    "S-24":              [44,  271.00,1,0],
    "SCALP EG":          [45,  992.00,1,0],# aka. Storm Shadow
    "Sea Eagle":         [46,  505.00,1,0],
    "MK-82HD":           [47,  192.00,1,0],
    "MK-20":             [48,  100.00,1,1],#aka CBU-100 # bomblet warhead. 247 x 0.4lb
    "ZB-250":            [49,  236.99,1,0],
    "ZB-500":            [50,  473.99,1,0],
    "AGM-45":            [51,  149.00,1,0],#shrike
    "AIM-120B":          [52,   44.00,0,0],
    "AIM-54":            [53,  135.00,0,0],
    "AGM-78":            [54,  215.00,1,0],
    "AIM-7F":            [55,   88.00,0,0],
    "AGM-62":            [56, 2000.00,1,0],
    "AIM-9L":            [57,   20.80,0,0],
    "AGM-65D":           [58,  126.00,1,0],
    "AIM-132":           [59,   22.05,0,0],
    "d-8":               [60,   20.80,0,0],#deprecated
    "KN-06":             [61,  315.00,0,0],
    "9M317":             [62,  145.00,0,0],
    "d9":                [63,   27.00,0,0],#deprecated 
    "R.550 Magic":       [64,   26.45,0,0],# also called majic
    "d-0":               [65,   30.00,0,0],#deprecated
    "R.550 Magic 2":     [66,   27.00,0,0],
    "R.530":             [67,   55.00,0,0],
    "d-a":               [68,   30.00,0,0],#deprecated
    "AIM-9M":            [69,   20.80,0,0],
    "R-73 RMD-1":        [70,   16.31,0,0],# automat Mig29/su27
    "Meteor":            [71,   55.00,0,0],
    "MICA-EM":           [72,   30.00,0,0], 
    "MICA-IR":           [73,   30.00,0,0], 
    "R-13M":             [74,   16.31,0,0],
    "R-27R1":            [75,   85.98,0,0],
    "R-27T1":            [76,   85.98,0,0],
    "R-3R":              [77,   16.31,0,0],
    "R-3S":              [78,   16.31,0,0],
    "R-55":              [79,   20.06,0,0],
    "R-60":              [80,    6.60,0,0],
    "R-60M":             [81,    7.70,0,0],
    "R-73E":             [82,   16.31,0,0],
    "R-77":              [83,   49.60,0,0],
    "R74":               [84,   16.00,0,0],
    "RB-05A":            [85,  353.00,1,0],
    "RB-24":             [86,   20.80,0,0],
    "RB-24J":            [87,   20.80,0,0],
    "RB-71":             [88,   88.00,0,0],
    "RB-74":             [89,   20.80,0,0],
    "RB-99":             [90,   44.00,0,0],
    "Super 530D":        [91,   66.00,0,0],
    "48N6":              [92,  330.00,0,0],# 48N6 from S-300pmu
    "pilot":             [93,    0.00,1,0],# ejected pilot
    "BETAB-500ShP":      [94, 1160.00,1,0],
    "Flare":             [95,    0.00,0,0],
};

var id2warhead = [];
var launched = {};# callsign: elapsed-sec
var approached = {};# callsign: uniqueID
var heavy_smoke = [61,62,92];

var k = keys(warheads);

for(var myid = 0;myid<size(k);myid+=1) {
  foreach(key ; k) {
    var wh = warheads[key];
    if (wh[0] == myid) {
      append(wh, key);
      append(id2warhead, wh);
      break;
    }
  }
  if (size(id2warhead) != myid+1) {
    printf("warheads corrupt at %d", myid);
    return;
  }
}

var id2shell = [];

var k = keys(shells);

for(var myid = 0;myid<size(k);myid+=1) {
  foreach(key ; k) {
    var wh = shells[key];
    if (wh[0] == myid) {
      append(wh, key);
      append(id2shell, wh);
      break;
    }
  }
  if (size(id2shell) != myid+1) {
    printf("shells corrupt at %d", myid);
    return;
  }
}

#==================================================================
#                       Notification processing
#==================================================================

#
# Create emesary recipient for handling other craft's missile positioins.
var DamageRecipient =
{
    new: func(_ident)
    {
        var new_class = emesary.Recipient.new(_ident);

        new_class.Receive = func(notification)
        {
            if (!notification.FromIncomingBridge) {
              return emesary.Transmitter.ReceiptStatus_NotProcessed;
            }
#
#
# This will be where movement and damage notifications are received. 
# This can replace MP chat for damage notifications 
# and allow missile visibility globally (i.e. all suitable equipped models) have the possibility
# to receive notifications from all other suitably equipped models.
            if (notification.NotificationType == "ArmamentInFlightNotification" or notification.NotificationType == "ObjectInFlightNotification") {
#                print("recv(d1): ",notification.NotificationType, " ", notification.Ident, 
#                      " UniqueIdentity=",notification.UniqueIdentity,
#                      " Kind=",notification.Kind,
#                      " SecondaryKind=",notification.SecondaryKind,
#                      " lat=",notification.Position.lat(),
#                      " lon=",notification.Position.lon(),
#                      " alt=",notification.Position.alt(),
#                      " u_fps=",notification.u_fps,
#                      " Heading=",notification.Heading,
#                      " Pitch=",notification.Pitch,
#                      " IsDistinct=",notification.IsDistinct,
#                      " Callsign=",notification.Callsign,
#                      " RemoteCallsign=",notification.RemoteCallsign,
#                      " Flags=",notification.Flags,
#                      " Radar=",bits.test(notification.Flags, 0),
#                      " Thrust=",bits.test(notification.Flags, 1));
                #
                # todo:
                #   animate missiles
                #
                if (notification.NotificationType == "ObjectInFlightNotification") {
                  notification.Pitch = 0;
                  notification.Heading = 0;
                  notification.u_fps = 0;
                  notification.Flags = 0;
                  notification.RemoteCallsign = "";
                }
                if(getprop("payload/armament/msg") == 0 and notification.RemoteCallsign != notification.Callsign) {
                  return emesary.Transmitter.ReceiptStatus_NotProcessed;
                }
                                
                var elapsed = getprop("sim/time/elapsed-sec");
                var ownPos = geo.aircraft_position();
                var bearing = ownPos.course_to(notification.Position);
                var radarOn = bits.test(notification.Flags, 0);
                var thrustOn = bits.test(notification.Flags, 1);
                var index = notification.SecondaryKind-21;
                var typ = id2warhead[index];
                
                if (notification.Kind == MOVE) {
                  if (thrustOn or index == 93 or index == 95) {
                    # visualize missile smoke trail
                    
                      var smoke = 1;
                      if (index == 93) {
                        smoke = 0;
                      } elsif (index == 95) {
                        smoke = 3;
                        if (notification.Position.distance_to(ownPos)*M2NM > 5) {
                          return emesary.Transmitter.ReceiptStatus_OK;
                        }
                      } else {
                        foreach(var black;heavy_smoke) {
                          if (index == black) {
                            smoke = 2;
                            break;
                          }
                        }
                      }
                      dynamics["noti_"~notification.Callsign~"_"~notification.UniqueIdentity] = [systime(), notification.Position.lat(), notification.Position.lon(), notification.Position.alt(), notification.u_fps, notification.Heading, notification.Pitch,smoke];
                                     
                  } else {
                    # the +1.5 is the update time that missiles send notifications out in
                    dynamics["noti_"~notification.Callsign~"_"~notification.UniqueIdentity] = [systime()-(time_before_delete-1.6), notification.Position.lat(), notification.Position.lon(), notification.Position.alt(), notification.u_fps, notification.Heading, notification.Pitch,-1]
                  }
                } elsif (notification.Kind == DESTROY) {
                  dynamics["noti_"~notification.Callsign~"_"~notification.UniqueIdentity] = [systime()-(time_before_delete-1.6), notification.Position.lat(), notification.Position.lon(), notification.Position.alt(), notification.u_fps, notification.Heading, notification.Pitch,-1]
                }
                
                if (tacview_supported and getprop("sim/multiplay/txhost") == "mpserver.opredflag.com") {
                  if (tacview.starttime) {
                    var tacID = left(md5(notification.Callsign~notification.UniqueIdentity),6);
                    if (notification.Kind == DESTROY) {
                      thread.lock(tacview.mutexWrite);
                      tacview.write("#" ~ (systime() - tacview.starttime)~"\n");
                      tacview.write(tacID~",Visible=0\n-"~tacID~"\n");
                      thread.unlock(tacview.mutexWrite);
                    } else {
                      var typp = typ[4]=="pilot"?"Parachutist":typ[4];
                      var extra = typp=="Parachutist"?"|0|0|0":"";
                      var extra2 = typ[2]==0?",Type=Weapon+Missile":",Type=Weapon+Bomb";
                      extra2 = typp=="Parachutist"?"":extra2;
                      var color = radarOn?",Color=Red":",Color=Yellow";
                      thread.lock(tacview.mutexWrite);
                      tacview.write("#" ~ (systime() - tacview.starttime)~"\n");
                      tacview.write(tacID~",T="~notification.Position.lon()~"|"~notification.Position.lat()~"|"~notification.Position.alt()~extra~",Name="~typp~color~extra2~"\n");
                      thread.unlock(tacview.mutexWrite);
                    }
                  }
                }
                
                if (notification.Kind == DESTROY) {
                  return emesary.Transmitter.ReceiptStatus_OK;
                }
                
                if (index == 95 or index == 93) {
                  return emesary.Transmitter.ReceiptStatus_OK;
                }
                
                # Missile launch warning:
                if (thrustOn) {
                  var launch = launched[notification.Callsign~notification.UniqueIdentity];
                  if (launch == nil or elapsed - launch > 300) {
                    launch = elapsed;
                    launched[notification.Callsign~notification.UniqueIdentity] = launch;
                    if (notification.Position.direct_distance_to(ownPos)*M2NM < mlw_max) {
                      setprop("payload/armament/MLW-bearing", bearing);
                      setprop("payload/armament/MLW-launcher", notification.Callsign);
                      setprop("payload/armament/MLW-count", getprop("payload/armament/MLW-count")+1);
                      var out = sprintf("Missile Launch Warning from %03d degrees.", bearing);
                      if (rwr_to_screen) screen.log.write(out, 1,1,0);# temporary till someone models a RWR in RIO seat
                      print(out);
                      damageLog.push(sprintf("Missile Launch Warning from %03d degrees from %s.", bearing, notification.Callsign));
                    }
                  }
                }
                
                # Missile approach warning:
                var callsign = getprop("sim/multiplay/callsign");
                callsign = size(callsign) < 8 ? callsign : left(callsign,7);
                if (notification.RemoteCallsign != callsign) return emesary.Transmitter.ReceiptStatus_OK;
                if (!radarOn) return emesary.Transmitter.ReceiptStatus_OK;# this should be little more complex later
                var heading = getprop("orientation/heading-deg");
                var clock = geo.normdeg(bearing - heading);
                setprop("payload/armament/MAW-bearing", bearing);
                setprop("payload/armament/MAW-active", 1);# resets every 1 seconds
                MAW_elapsed = elapsed;
                var appr = approached[notification.Callsign~notification.UniqueIdentity];
                if (appr == nil or elapsed - appr > 450) {
                  printf("Missile Approach Warning from %03d degrees.", bearing);
                  damageLog.push(sprintf("Missile Approach Warning from %03d degrees from %s.", bearing, notification.Callsign));
                  if (rwr_to_screen) screen.log.write(sprintf("Missile Approach Warning from %03d degrees.", bearing), 1,1,0);# temporary till someone models a RWR in RIO seat
                  approached[notification.Callsign~notification.UniqueIdentity] = elapsed;
                  if (m28_auto) mig28.engagedBy(notification.Callsign);
                }
                return emesary.Transmitter.ReceiptStatus_OK;
            }
            if (notification.NotificationType == "ArmamentNotification") {
#                if (notification.FromIncomingBridge) {
#                    print("recv(d2): ",notification.NotificationType, " ", notification.Ident,
#                          " Kind=",notification.Kind,
#                          " SecondaryKind=",notification.SecondaryKind,
#                          " RelativeAltitude=",notification.RelativeAltitude,
#                          " Distance=",notification.Distance,
#                          " Bearing=",notification.Bearing,
#                          " Inc-bridge=",notification.FromIncomingBridge,
#                          " RemoteCallsign=",notification.RemoteCallsign);
#                    debug.dump(notification);
                    #
                    #
                    var callsign = getprop("sim/multiplay/callsign");
                    callsign = size(callsign) < 8 ? callsign : left(callsign,7);
                    if (notification.RemoteCallsign == callsign and getprop("payload/armament/msg") == 1) {
                        #damage enabled and were getting hit
                        if (m28_auto) mig28.engagedBy(notification.Callsign);
                        if (notification.SecondaryKind < 0 and hitable_by_cannon) {
                            # cannon hit
                            var probability = id2shell[-1*notification.SecondaryKind-1][1];
                            var typ = id2shell[-1*notification.SecondaryKind-1][2];
                            var hit_count = notification.Distance;
                            if (hit_count != nil) {
                                var damaged_sys = 0;
                                for (var i = 1; i <= hit_count; i = i + 1) {
                                  var failed = fail_systems(probability);
                                  damaged_sys = damaged_sys + failed;
                                }
                                printf("Took %.1f%% x %2d damage from %s! %s systems was hit.", probability*100, hit_count, typ, damaged_sys);
                                damageLog.push(sprintf("%s hit you with %d %s.", notification.Callsign, hit_count, typ));
                                nearby_explosion();
                            }
                        } elsif (notification.SecondaryKind > 20) {
                            # its a warhead
                            var dist     = notification.Distance;
                            var wh = id2warhead[notification.SecondaryKind - 21];
                            var type = wh[4];#test code
                            if (wh[3] == 1) {
                                # cluster munition
                                var lbs = wh[1];
                                var maxDist = maxDamageDistFromWarhead(lbs);
                                var distance = math.max(0,rand()*5-full_damage_dist_m);#being 0 to 5 meters from a bomblet on average.
                                var diff = math.max(0, maxDist-distance);
                                diff = diff * diff;
                                var probability = diff / (maxDist*maxDist);
                                if (use_hitpoints_instead_of_failure_modes_bool) {
                                  var hpDist = maxDamageDistFromWarhead(hp_max);
                                  probability = (maxDist/hpDist)*probability;
                                }
                                var failed = fail_systems(probability, hp_max);
                                var percent = 100 * probability;
                                printf("Took %.1f%% damage from %s clusterbomb at %0.1f meters from bomblet. %s systems was hit", percent,type,distance,failed);
                                damageLog.push(sprintf("%s hit you with %s bomblet, %.1f meters distance.", notification.Callsign, type, dist));
                                nearby_explosion();
                                return;
                            }

                            var distance = math.max(dist-full_damage_dist_m, 0);
              
                            var maxDist = 0;# distance where the explosion dont hurt us anymore
                            var lbs = 0;
                            
                            if (wh[2] == 1) {
                              lbs = wh[1];
                              maxDist = maxDamageDistFromWarhead(lbs);#3*sqrt(lbs)
                            } elsif (hitable_by_air_munitions and wh[2] == 0) {
                              lbs = wh[1];
                              maxDist = maxDamageDistFromWarhead(lbs);
                            } else {
                              return;
                            }
                            
                            var diff = maxDist-distance;
                            if (diff < 0) {
                              diff = 0;
                            }
                            diff = diff * diff;
                                          
                            var probability = diff / (maxDist*maxDist);
                            
                            if (use_hitpoints_instead_of_failure_modes_bool) {
                              var hpDist = maxDamageDistFromWarhead(hp_max);
                              probability = (maxDist/hpDist)*probability;
                            }

                            var failed = fail_systems(probability, hp_max);
                            var percent = 100 * probability;
                            printf("Took %.1f%% damage from %s at %0.1f meters. %s systems was hit", percent,type,dist,failed);
                            damageLog.push(sprintf("%s hit you with %s, %.1f meters distance.", notification.Callsign, type, dist));
                            nearby_explosion();
                            
                            ####
                            # I don't remember all the considerations that went into our original warhead damage model.
                            # But looking at the formula it looks like they all do 100% damage at 0 meter hit,
                            # and warhead size is only used to determine the decrease of damage with distance increase.
                            # It sorta gets the job done though, so I am hesitant to advocate that warheads above a certain
                            # size should give 100% damage for some distance, and that warheads smaller than certain size should
                            # not give 100% damage even on direct hit.
                            # Anyway, for hitpoint based assets, this is now the case. Maybe we should consider to also do something
                            # similar for failure mode based aircraft. ~Nikolai
                            ####
                            
                            ## example 1: ##
                            # 300 lbs warhead, 50 meters distance
                            # maxDist=52
                            # diff = 52-50 = 2
                            # diff^2 = 4
                            # prob = 4/2700 = 0.15%
                            
                            ## example 2: ##
                            # 300 lbs warhead, 25 meters distance
                            # maxDist=52
                            # diff = 52-25 = 27
                            # diff^2 = 729
                            # prob = 729/2700 = 27%
                        } 
                    }
#                }
                return emesary.Transmitter.ReceiptStatus_OK;
            }
            if (notification.NotificationType == "StaticNotification") {
                if(getprop("payload/armament/msg") == 0) {
                  return emesary.Transmitter.ReceiptStatus_NotProcessed;
                }
                if (notification.Kind == CREATE and getprop("payload/armament/enable-craters") == 1 and statics["obj_"~notification.UniqueIdentity] == nil) {
                    if (notification.SecondaryKind == 0) {# TODO: make a hash with all the models
                        var crater_model = getprop("payload/armament/models") ~ "crater_small.xml";
                        var static = geo.put_model(crater_model, notification.Position.lat(), notification.Position.lon(), notification.Position.alt(), notification.Heading);
                        if (static != nil) {
                            statics["obj_"~notification.UniqueIdentity] = [static, notification.Position.lat(), notification.Position.lon(), notification.Position.alt(), notification.Heading, notification.SecondaryKind];
                            #static is a PropertyNode inside /models
                        }
                    } elsif (notification.SecondaryKind == 1) {
                        var crater_model = getprop("payload/armament/models") ~ "crater_big.xml";
                        var static = geo.put_model(crater_model, notification.Position.lat(), notification.Position.lon(), notification.Position.alt(), notification.Heading);
                        if (static != nil) {
                            statics["obj_"~notification.UniqueIdentity] = [static, notification.Position.lat(), notification.Position.lon(), notification.Position.alt(), notification.Heading, notification.SecondaryKind];
                        }
                    } elsif (notification.SecondaryKind == 2) {
                        var crater_model = getprop("payload/armament/models") ~ "bomb_hit_smoke.xml";
                        var static = geo.put_model(crater_model, notification.Position.lat(), notification.Position.lon(), notification.Position.alt(), notification.Heading);
                        if (static != nil) {
                            statics["obj_"~notification.UniqueIdentity] = [static, notification.Position.lat(), notification.Position.lon(), notification.Position.alt(), notification.Heading, notification.SecondaryKind];
                        }
                    }
                } elsif (notification.Kind == REQUEST_ALL and getprop("payload/armament/enable-craters") == 1) {
                  # someone has requested all statics, lets send them out
                  var kes = keys(statics);
                  printf(notification.Callsign~" has requested all statics, sending %d to him/her.",size(kes));
                  foreach(ke;kes) {
                    var static = statics[ke];
                    var msg = notifications.StaticNotification.new("stat", num(substr(ke,4)), CREATE, static[5]);
                    msg.Position.set_latlon(static[1],static[2],static[3]);
                    msg.IsDistinct = 0;
                    msg.Heading = static[4];
                    notifications.hitBridgedTransmitter.NotifyAll(msg);
                  }
                }
                return emesary.Transmitter.ReceiptStatus_OK;
            }
            return emesary.Transmitter.ReceiptStatus_NotProcessed;
        };
        return new_class;
    }
};

damage_recipient = DamageRecipient.new("DamageRecipient");
emesary.GlobalTransmitter.Register(damage_recipient);

#==================================================================
#                       Notification Kinds
#==================================================================
# Static variables for notification.Kind:
var CREATE = 1;
var MOVE = 2;
var DESTROY = 3;
var IMPACT = 4;
var REQUEST_ALL = 5;

#==================================================================
#                       Flying missiles over MP
#==================================================================

var statics = {};
var dynamics = {};
var dynamic3d = [];
var deadreckon_updatetime = 0.1;# 1/15 of missile send rate
var time_before_delete = 2.5;# time since last notification before deleting

var dynamic_loop = func {
  # This keeps track of MP flying missiles/parachutes/flares and manages ModelManager.
  var new_dynamic3d = [];
  var stime = systime();
  foreach (dynamic3d_entry ; dynamic3d) {
    var dyna = dynamics[dynamic3d_entry[0]];
    if (dyna != nil and stime-dyna[0] > time_before_delete) {
      # OLD, DELETE ALL
      delete(dynamics, dynamic3d_entry[0]);
      reckon_delete(dynamic3d_entry);
    } elsif (dyna != nil and dynamic3d_entry[1] < dyna[0]) {
      # REFRESH INCOMING
      # update pos and attitude
      append(new_dynamic3d, reckon_update(dyna, dynamic3d_entry, stime));
      delete(dynamics, dynamic3d_entry[0]);
    } elsif (dyna == nil and stime-dynamic3d_entry[1] < time_before_delete) {
      # BETWEEN UPDATES
      # deadreckon
      reckon_move(dynamic3d_entry, stime);
      append(new_dynamic3d, dynamic3d_entry);
    } else {
      # OLD, DELETE ALL
      reckon_delete(dynamic3d_entry);
    }
  }
  dynamic3d = new_dynamic3d;
  var kees = keys(dynamics);
  foreach(kee; kees) {
    var dyna = dynamics[kee];
    if (stime-dyna[0] < time_before_delete) {
      var new_entry = reckon_create(kee, dyna, stime);
      if (new_entry !=nil) {
        append(dynamic3d, new_entry);
      }
    }
    delete(dynamics, kee);
  }
  settimer(dynamic_loop,deadreckon_updatetime);
}

var ModelManager = {
    # This shows missiles/parachutes/flares flying and their smoke trail.
    new: func (path,lat,lon,alt_ft,heading,pitch,para) {
        var m = {parents:[ModelManager]};
        var n = props.globals.getNode("models", 1);
        var i = 0;
        for (i = 0; 1==1; i += 1) {
          if (n.getChild("model", i, 0) == nil) {
            break;
          }
        }
        m.model = n.getChild("model", i, 1);
        
        n = props.globals.getNode("sim/emesary-models", 1);
        for (i = 0; 1==1; i += 1) {
          if (n.getChild("dynamic", i, 0) == nil) {
            break;
          }
        }
        m.ai = n.getChild("dynamic", i, 1);
        
        m.model.getNode("path", 1).setValue(path);
        
        # Create the AI position and orientation properties.
        m.lat   = m.ai.getNode("position/latitude-deg", 1);
        m.lon   = m.ai.getNode("position/longitude-deg", 1);
        m.alt_ft= m.ai.getNode("position/altitude-ft", 1);
        m.heading= m.ai.getNode("orientation/true-heading-deg", 1);
        m.pitch = m.ai.getNode("orientation/pitch-deg", 1);
        m.roll  = m.ai.getNode("orientation/roll-deg", 1);
        
        m.lat.setDoubleValue(lat);
        m.lon.setDoubleValue(lon);
        m.alt_ft.setDoubleValue(alt_ft);
        m.heading.setDoubleValue(heading);
        m.pitch.setDoubleValue(para?0:pitch);
        m.roll.setDoubleValue(0);
        
        m.vLat = lat;
        m.vLon = lon;
        m.vAlt_ft = alt_ft;
        m.vHeading = heading;
        m.vPitch = pitch;
        #m.vRoll = 0;
        m.pLat = m.vLat;
        m.pLon = m.vLon;
        m.pAlt_ft = m.vAlt_ft;
        m.pHeading = m.vHeading;
        m.pPitch = m.vPitch;
        
        
        m.model.getNode("latitude-deg-prop", 1).setValue(m.lat.getPath());
        m.model.getNode("longitude-deg-prop", 1).setValue(m.lon.getPath());
        m.model.getNode("elevation-ft-prop", 1).setValue(m.alt_ft.getPath());
        m.model.getNode("heading-deg-prop", 1).setValue(m.heading.getPath());
        m.model.getNode("pitch-deg-prop", 1).setValue(m.pitch.getPath());
        m.model.getNode("roll-deg-prop", 1).setValue(m.roll.getPath());
        
        m.coord = geo.Coord.new();
        m.uBody_fps = 0;
        m.last = [geo.Coord.new().set_latlon(lat,lon,alt_ft*FT2M).xyz(),systime()];
        m.past = m.last;
        m.frametime = 0;
        m.delayTime = 0;        
        
        return m;
    },
    moveRealtime: func (uBody_fps, dt, factor) {
        if (me.uBody_fps == 0) me.uBody_fps = uBody_fps;
        me.slant_ft   = (me.uBody_fps < uBody_fps?me.uBody_fps:uBody_fps) * dt * factor;
        me.uBody_fps  = uBody_fps;
        me.alt_dist   = me.slant_ft*math.sin(me.vPitch*D2R);
        me.horiz_dist = me.slant_ft*math.cos(me.vPitch*D2R);
        
        me.coord.set_latlon(me.vLat, me.vLon, (me.vAlt_ft+me.alt_dist) * FT2M);
        
        me.coord = me.coord.apply_course_distance(me.vHeading, me.horiz_dist*FT2M);
                
        me.latlon = me.coord.latlon();
        
        me.vLat    = me.latlon[0];
        me.vLon    = me.latlon[1];
        me.vAlt_ft = me.latlon[2]*M2FT;
        
        me.lat.setDoubleValue(me.vLat);
        me.lon.setDoubleValue(me.vLon);
        me.alt_ft.setDoubleValue(me.vAlt_ft);
    },
    moveDelayed: func (dt) {
        if (me.frametime==0) return;
        me.place();
        me.xyz = me.interpolate(me.past[0],me.last[0], me.delayTime/me.frametime);
        #print("% "~100*me.delayTime/me.frametime);
        me.coord.set_xyz(me.xyz[0],me.xyz[1],me.xyz[2]);
        me.latlon = me.coord.latlon();
        me.lat.setDoubleValue(me.latlon[0]);
        me.lon.setDoubleValue(me.latlon[1]);
        me.alt_ft.setDoubleValue(me.latlon[2]*M2FT);
        me.delayTime += dt;
    },
    interpolate: func (start, end, fraction) {
        me.xx = (start[0]*(1-fraction)+end[0]*fraction);
        me.yy = (start[1]*(1-fraction)+end[1]*fraction);
        me.zz = (start[2]*(1-fraction)+end[2]*fraction);
        return [me.xx,me.yy,me.zz];
    },
    place: func {
      if (me["loadNode"] == nil) {
        me.loadNode = me.model.getNode("load", 1);
        me.loadNode.setBoolValue(1);
        me.loadNode.setBoolValue(0);
      }
    },
    translateDelayed: func (lat,lon,alt_ft,heading,pitch, para) {
        me.heading.setDoubleValue(heading);
        me.pitch.setDoubleValue(para?0:pitch);
        
        me.pLat = me.vLat;
        me.pLon = me.vLon;
        me.pAlt_ft = me.vAlt_ft;
        me.pHeading = me.vHeading;
        me.pPitch = me.vPitch;
        
        me.vLat = lat;
        me.vLon = lon;
        me.vAlt_ft = alt_ft;
        me.vHeading = heading;
        me.vPitch = pitch;
        #me.vRoll = 0;
        
        me.past = me.last;
        me.last = [geo.Coord.new().set_latlon(lat,lon,alt_ft*FT2M).xyz(),systime()];
        me.delayTime = 0;
        me.frametime = me.last[1]-me.past[1];
    },
    translateRealtime: func (lat,lon,alt_ft,heading,pitch, para) {
        me.lat.setDoubleValue(lat);
        me.lon.setDoubleValue(lon);
        me.alt_ft.setDoubleValue(alt_ft);
        me.heading.setDoubleValue(heading);
        me.pitch.setDoubleValue(para?0:pitch);
        #me.roll.setDoubleValue(0);
                
        me.vLat = lat;
        me.vLon = lon;
        me.vAlt_ft = alt_ft;
        me.vHeading = heading;
        me.vPitch = pitch;
        #me.vRoll = 0;
    },
    del: func {
      me.model.remove();
      me.ai.remove();
    },
};

var reckon_create = func (kee, dyna, stime) {
  var path = getprop("payload/armament/models") ~ "parachutist.xml";
  if (dyna[7]==1) {
    path = getprop("payload/armament/models") ~ "light_smoke.xml";
  } elsif (dyna[7] ==2) {
    path = getprop("payload/armament/models") ~ "heavy_smoke.xml";
  } elsif (dyna[7] ==3) {
    path = getprop("payload/armament/models") ~ "the-flare.xml";
  } elsif (dyna[7] == -1) {
    return nil;
  }
  var static = ModelManager.new(path, dyna[1],dyna[2],dyna[3]*M2FT,dyna[5],dyna[6],dyna[7]==0);#path,lat,lon,alt_m,heading,pitch
  if (static != nil) {
    var entry = [kee, stime, static, dyna[4]];
    return entry;
  }
  print("NOT FOUND (Emesary): "~path);
  return nil;
}

var reckon_update = func (dyna, entry, stime) {
  var static = entry[2];
  var dynami2 = [entry[0], stime, static, dyna[4]];
  # translate
  static.translateDelayed(dyna[1],dyna[2],dyna[3]*M2FT,dyna[5],dyna[6],dyna[7]==0);
  static.moveDelayed(deadreckon_updatetime);
  return dynami2;
}

var reckon_move = func (entry, stime) {
  var static = entry[2];
  var time_then = entry[1];
  var time_now = stime;
  # dead-reckon
  #static.moveRealtime(entry[3] , time_now-time_then, entry[4]?0.25:0.5);
  static.moveDelayed(deadreckon_updatetime);#time_now-time_then);
}

var reckon_delete = func (entry) {
  entry[2].del();
}

dynamic_loop();

#==================================================================
#                       Flares over MP
#==================================================================

var last_prop = 0;
var last_release = 0;
var flare_list = [];
var flare_update_time = 0.4;
var flare_duration = 8;
var flare_terminal_speed = 50;#m/s
var flares_max_process_per_loop = 4;

var flare_sorter = func(a, b) {
    if(a[0] < b[0]){
        return -1; # A should before b in the returned vector
    }elsif(a[0] == b[0]){
        return 0; # A is equivalent to b 
    }else{
        return 1; # A should after b in the returned vector
    }
}

var animate_flare = func {
  # Send out notifications about own flare positions every 0.4s
  if (!getprop("payload/armament/msg")) {
    return;
  }
  var stime = systime();
  # old flares
  var old_flares = [];
  var flares_sent = 0;
  flare_list = sort(flare_list, flare_sorter);
  foreach(flare; flare_list) {
    if (stime-flare[1] > flare_duration) {
      var msg = notifications.ObjectInFlightNotification.new("ffly", flare[6], DESTROY, 21+95);
      msg.Flags = 0;
      msg.Position = flare[2];
      msg.IsDistinct = 1;
      msg.RemoteCallsign = "";
      msg.UniqueIndex = flare[6];
      msg.Pitch = 0;
      msg.Heading = 0;
      msg.u_fps = 0;
      notifications.objectBridgedTransmitter.NotifyAll(msg);
      continue;
    }
    if (flares_sent < flares_max_process_per_loop) {
      var flare_dt = stime-flare[0];
      #       update_t,start_t, coord,    heading,  speed_down_mps,                                                                   , speed_horiz_mps,                  unique
      flare = [stime, flare[1], flare[2], flare[3], (flare[4]<flare_terminal_speed)?(flare[4]+flare_dt*9.83*0.5):(flare[4]-flare_dt*3), math.max(0,flare[5]-flare_dt*20), flare[6]];
      flare[2].apply_course_distance(flare[3], flare_dt*flare[5]);
      flare[2].set_alt(flare[2].alt()-flare_dt*flare[4]);
      
      var msg = notifications.ObjectInFlightNotification.new("ffly", flare[6], MOVE, 21+95);
      msg.Flags = 0;
      msg.Position = flare[2];
      msg.IsDistinct = 1;
      msg.RemoteCallsign = "";
      msg.UniqueIndex = flare[6];
      msg.Pitch = 0;
      msg.Heading = 0;
      msg.u_fps = 0;
      notifications.objectBridgedTransmitter.NotifyAll(msg);
      flares_sent += 1;
    }
    append(old_flares, flare);
  }
  flare_list = old_flares;
  if(auto_flare_caller) {
    auto_flare_released();
  }  
}
var flaretimer = maketimer(flare_update_time, animate_flare);
flaretimer.start();

var auto_flare_released = func {
  # This detects own flares releases
  var prop = getprop("rotors/main/blade[3]/flap-deg");
  var stime = systime();
  if (prop != nil and prop != 0 and prop != last_prop and stime-last_release > 1)  {
    flare_released();
    last_release = stime;
  }
  last_prop = prop;
}

var flare_released = func {
    if (!getprop("payload/armament/msg")) {
      return;
    }
    # We released a flare. If you call this method manually, then make sure 'auto_flare_caller' is false.
    var stime = systime();
    var flare =[stime, stime,
                geo.aircraft_position(),
                getprop("orientation/heading-deg"),
                FT2M*getprop("velocities/speed-down-fps"),
                FT2M*math.sqrt(getprop("velocities/speed-north-fps")*getprop("velocities/speed-north-fps")+getprop("velocities/speed-east-fps")*getprop("velocities/speed-east-fps")),
                int(rand()*200)-100];
    append(flare_list, flare);
    var msg = notifications.ObjectInFlightNotification.new("ffly", flare[6], MOVE, 21+95);
    msg.Flags = 0;
    msg.Position = flare[2];
    msg.IsDistinct = 1;
    msg.RemoteCallsign = "";
    msg.UniqueIndex = flare[6];
    msg.Pitch = 0;
    msg.Heading = 0;
    msg.u_fps = 0;
    notifications.objectBridgedTransmitter.NotifyAll(msg);
}

#==================================================================
#                       Notification for getting craters
#==================================================================

setlistener("sim/multiplay/online", func {
  check_for_Request();
},0,0);

setlistener("payload/armament/msg", func {
  check_for_Request();
},0,0);

var last_check = -65;

var check_for_Request = func {
  # This sends out a notification to ask other aircraft for all craters
  if (getprop("payload/armament/enable-craters") == 1 and getprop("sim/multiplay/online") and getprop("payload/armament/msg") and systime()-last_check > 60) {
    last_check = systime();
    var msg = notifications.StaticNotification.new("stat", int(rand()*15000000), REQUEST_ALL, 0);
    msg.IsDistinct = 0;
    msg.Heading = 0;
    notifications.hitBridgedTransmitter.NotifyAll(msg);
  } else {
  }
}

settimer(check_for_Request, 60);# for aircraft like mig21 that starts with damage enabled

#==================================================================
#                       Damage functions
#==================================================================

var maxDamageDistFromWarhead = func (lbs) {
  # Calc at what distance the warhead will do zero damage every time.
  var dist = 3*math.sqrt(lbs);

  return dist;
}

var fail_systems = func (probability, factor = 100) {#this factor needs tuning after all asset hitpoints have been evaluated.
    if (is_fleet) {
      return fail_fleet_systems(probability, factor);
    } elsif (use_hitpoints_instead_of_failure_modes_bool) {
      hp -= factor * probability*(0.75+rand()*0.25);# from 75 to 100% damage
      printf("HP: %d/%d", hp, hp_max);
      setprop("sam/damage", math.max(0,100*hp/hp_max));#used in HUD
      if ( hp < 0 ) {
        setprop("/carrier/sunk/",1);#we are dead
        setprop("/sim/multiplay/generic/int[2]",1);#radar off
        setprop("/sim/multiplay/generic/int[0]",1);#smoke on
        setprop("/sim/messages/copilot", getprop("sim/multiplay/callsign")~" dead.");
      }
      return -1;
    } else {
      var failure_modes = FailureMgr._failmgr.failure_modes;
      var mode_list = keys(failure_modes);
      var failed = 0;
      foreach(var failure_mode_id; mode_list) {
        #print(failure_mode_id);
          if (rand() < probability) {
              FailureMgr.set_failure_level(failure_mode_id, 1);
              failed += 1;
              if (getprop("sim/flight-model") == "yasim") {
                if (failure_mode_id == "Engines/engine" and yasim_list2 == nil) {
                  # fail  yasim:
                  setprop("sim/model/uh1/state",0);
                  setprop("controls/engines/engine/magnetos", 0);
                  setprop("controls/engines/engine/cutoff", 1);
                  setprop("controls/engines/engine/on-fire", 1);
                  #set a listener so that if a restart is attempted, it'll fail.
                  yasim_list = setlistener("sim/model/uh1/state",func {setprop("sim/model/uh1/state",0);});
                  yasim_list2 = setlistener("controls/engines/engine/cutoff",func {setprop("controls/engines/engine/cutoff",1);});
                }
                if (failure_mode_id == "Engines/engine[1]" and yasim_list3 == nil) {
                  # fail  yasim:
                  setprop("controls/engines/engine[1]/magnetos", 0);
                  setprop("controls/engines/engine[1]/cutoff", 1);
                  setprop("controls/engines/engine[1]/on-fire", 1);
                  #set a listener so that if a restart is attempted, it'll fail.
                  yasim_list3 = setlistener("controls/engines/engine[1]/cutoff",func {setprop("controls/engines/engine[1]/cutoff",1);});
                }
                if (failure_mode_id == "Engines/engine[2]" and yasim_list4 == nil) {
                  # fail  yasim:
                  setprop("controls/engines/engine[2]/magnetos", 0);
                  setprop("controls/engines/engine[2]/cutoff", 1);
                  setprop("controls/engines/engine[2]/on-fire", 1);
                  #set a listener so that if a restart is attempted, it'll fail.
                  yasim_list4 = setlistener("controls/engines/engine[2]/cutoff",func {setprop("controls/engines/engine[2]/cutoff",1);});
                }
                if (failure_mode_id == "Engines/engine[3]" and yasim_list5 == nil) {
                  # fail  yasim:
                  setprop("controls/engines/engine[3]/magnetos", 0);
                  setprop("controls/engines/engine[3]/cutoff", 1);
                  setprop("controls/engines/engine[3]/on-fire", 1);
                  #set a listener so that if a restart is attempted, it'll fail.
                  yasim_list5 = setlistener("controls/engines/engine[3]/cutoff",func {setprop("controls/engines/engine[3]/cutoff",1);});
                }
              }
          }
      }
      
      return failed;
    }
};
var yasim_list = nil;
var yasim_list2 = nil;
var yasim_list3 = nil;
var yasim_list4 = nil;
var yasim_list5 = nil;

var repairYasim = func {
  if (yasim_list != nil) {removelistener(yasim_list); yasim_list=nil;}
  if (yasim_list2 != nil) {removelistener(yasim_list2); yasim_list2=nil;}
  if (yasim_list3 != nil) {removelistener(yasim_list3); yasim_list3=nil;}
  if (yasim_list4 != nil) {removelistener(yasim_list4); yasim_list4=nil;}
  if (yasim_list5 != nil) {removelistener(yasim_list5); yasim_list5=nil;}
  setprop("controls/engines/engine[0]/on-fire", 0);
  setprop("controls/engines/engine[1]/on-fire", 0);
  setprop("controls/engines/engine[2]/on-fire", 0);
  setprop("controls/engines/engine[3]/on-fire", 0);
  setprop("sim/crashed", 0);
  var failure_modes = FailureMgr._failmgr.failure_modes;
  var mode_list = keys(failure_modes);

    foreach(var failure_mode_id; mode_list) {
      FailureMgr.set_failure_level(failure_mode_id, 0);
    }
}

setlistener("/sim/signals/reinit", repairYasim);

hp_f = [hp_max,hp_max,hp_max,hp_max,hp_max,hp_max,hp_max];

var fail_fleet_systems = func (probability, factor) {
  
  var sinking_ships = (hp_f[0]<0) + (hp_f[1]<0) + (hp_f[2]<0) + (hp_f[3]<0) + (hp_f[4]<0) + (hp_f[5]<0) + (hp_f[6]<0);
  var hit_sinking = 0;
  if (sinking_ships == 0) {
    hit_sinking = 0;
  } elsif (sinking_ships == 7) {
    hit_sinking = 1;
  } else {
    hit_sinking = rand()<0.10;
  }
  if (hit_sinking) {
    armament.defeatSpamFilter("You shot one of our already sinking ships, you are just mean.");
    return;
  }
 
  var no = 0;
  
  for (no=0; no < 7; no+=1) {
    if (hp_f[no] > 0) {
      break;
    }
  }
  hp_f[no] -= factor * probability*(0.75+rand()*0.25);# from 75 to 100% damage
  printf("HP[%d]: %d/%d", no, hp_f[no], hp_max);
  #setprop("sam/damage", math.max(0,100*hp/hp_max));#used in HUD
  if ( hp_f[no] < 0 ) {
    setprop("/sim/multiplay/generic/bool["~(no+40)~"]",1);
    armament.defeatSpamFilter("So you sank one of our ships, we will get you for that!");
    if (!getprop("/carrier/disabled") and hp_f[0]<0 and hp_f[1]<0 and hp_f[2]<0) {
      setprop("/carrier/disabled",1);
      armament.defeatSpamFilter("Captain our offensive capability is crippled!");
    }
    if (hp_f[0]<0 and hp_f[1]<0 and hp_f[2]<0 and hp_f[3]<0 and hp_f[4]<0 and hp_f[5]<0 and hp_f[6]<0) {
      setprop("/carrier/sunk",1);
      setprop("/sim/multiplay/generic/int[2]",1);#radar off
      setprop("/sim/messages/copilot", getprop("sim/multiplay/callsign")~" dead.");
      armament.defeatSpamFilter("S.O.S. Heeelp");
    } else {
      armament.defeatSpamFilter("This is not over yet..");
    }    
  }
  return -1;
};

setlistener("payload/armament/MLW-count", func {
  setLaunch(getprop("payload/armament/MLW-launcher"), 0);#TODO: figure out if that callsign is a SAM/ship.
});

#==================================================================
#                       RWR and sound functions
#==================================================================

var setLaunch = func (c,s) {
  setprop("sound/rwr-launch-sam", s);
  setprop("sound/rwr-launch", c);
  settimer(func {stopLaunch();},7);
}

var stopLaunch = func () {
  setprop("sound/rwr-launch", "");
  setprop("sound/rwr-launch-sam", 0);
}

var playIncomingSound = func (clock) {
  setprop("sound/incoming"~clock, 1);
  settimer(func {stopIncomingSound(clock);},3);
}

var stopIncomingSound = func (clock) {
  setprop("sound/incoming"~clock, 0);
}

var nearby_explosion = func {
  setprop("damage/sounds/nearby-explode-on", 0);
  settimer(nearby_explosion_a, 0);
}

var nearby_explosion_a = func {
  setprop("damage/sounds/nearby-explode-on", 1);
  settimer(nearby_explosion_b, 0.5);
}

var nearby_explosion_b = func {
  setprop("damage/sounds/nearby-explode-on", 0);
}

#==================================================================
#                       Helper functions
#==================================================================

var callsign_struct = {};
var getCallsign = func (callsign) {
  var node = callsign_struct[callsign];
  return node;
}

var MAW_elapsed = 0;

var processCallsigns = func () {
  callsign_struct = {};
  var players = props.globals.getNode("ai/models").getChildren();
  var myCallsign = getprop("sim/multiplay/callsign");
  myCallsign = size(myCallsign) < 8 ? myCallsign : left(myCallsign,7);
  var painted = 0;
  foreach (var player; players) {
    if(player.getChild("valid") != nil and player.getChild("valid").getValue() == TRUE and player.getChild("callsign") != nil and player.getChild("callsign").getValue() != "" and player.getChild("callsign").getValue() != nil) {
      var callsign = player.getChild("callsign").getValue();
      callsign_struct[callsign] = player;
      var str6 = player.getNode("sim/multiplay/generic/string[6]");
      if (str6 != nil and str6.getValue() != nil and str6.getValue() != "" and size(""~str6.getValue())==4 and left(md5(myCallsign),4) == str6.getValue()) {
        painted = 1;
      }
    }
  }
  setprop("payload/armament/spike", painted);
  if (getprop("sim/time/elapsed-sec")-MAW_elapsed > 1.1) {
      setprop("payload/armament/MAW-active", 0);# resets every 1.1 seconds without warning
  }
}
processCallsignsTimer = maketimer(1.5, processCallsigns);
processCallsignsTimer.simulatedTime = 1;
processCallsignsTimer.start();

#==================================================================
#                       Stuff
#==================================================================

var code_ct = func () {
  #ANTIC
  if (getprop("payload/armament/msg")) {
      setprop("sim/rendering/redout/enabled", TRUE);
      #call(func{fgcommand('dialog-close', multiplayer.dialog.dialog.prop())},nil,var err= []);# props.Node.new({"dialog-name": "location-in-air"}));
      call(func{multiplayer.dialog.del();},nil,var err= []);
      if (!getprop("gear/gear[0]/wow")) {
        call(func{fgcommand('dialog-close', props.Node.new({"dialog-name": "WeightAndFuel"}))},nil,var err2 = []);
        call(func{fgcommand('dialog-close', props.Node.new({"dialog-name": "system-failures"}))},nil,var err2 = []);
        call(func{fgcommand('dialog-close', props.Node.new({"dialog-name": "instrument-failures"}))},nil,var err2 = []);
      }      
      setprop("sim/freeze/fuel",0);
      setprop("/sim/speed-up", 1);
      setprop("/gui/map/draw-traffic", 0);
      setprop("/sim/gui/dialogs/map-canvas/draw-TFC", 0);
      #fgcommand("timeofday", props.Node.new({"timeofday": "real"}));
      #setprop("/sim/rendering/als-filters/use-filtering", 1);
      call(func{var interfaceController = fg1000.GenericInterfaceController.getOrCreateInstance();
      interfaceController.stop();},nil,var err2=[]);      
  }  
}
code_ctTimer = maketimer(1, code_ct);
code_ctTimer.simulatedTime = 1;



setprop("/sim/failure-manager/display-on-screen", FALSE);

code_ctTimer.start();

#==================================================================
#                       Relocation function
#==================================================================

var re_init = func (node) {
  # repair the aircraft
  if (node.getValue() == 0) return;
  
  var failure_modes = FailureMgr._failmgr.failure_modes;
  var mode_list = keys(failure_modes);

  foreach(var failure_mode_id; mode_list) {
    FailureMgr.set_failure_level(failure_mode_id, 0);
  }
  damageLog.push("Aircraft was repaired due to re-init.");
}

#==================================================================
#                       Event log
#==================================================================

var damageLog = events.LogBuffer.new(echo: 0);

damageLog.push("Flightgear "~getprop("sim/version/flightgear")~" was loaded up with "~getprop("sim/description")~" - "~getprop("sim/time/gmt"));

setlistener("/sim/signals/reinit", re_init, 0, 0);

setlistener("payload/armament/msg", func {damageLog.push("Damage is now "~(getprop("payload/armament/msg")?"ON.":"OFF."));}, 1, 0);

setlistener("sim/multiplay/callsign", func {damageLog.push("Callsign is now "~getprop("sim/multiplay/callsign"));}, 1, 0);

setlistener("sim/multiplay/online", func {damageLog.push(getprop("sim/multiplay/online")?("Connected to "~getprop("sim/multiplay/txhost")):"Disconnected from MP.");}, 1, 0);

var printDamageLog = func {
  if (getprop("payload/armament/msg")) {print("disable damage to use this function");return;}
  var buffer = damageLog.get_buffer();
  var str = "";
  foreach(entry; buffer) {
      str = str~"    "~entry.time~" "~entry.message~"\n";
  }
  print();
  print(str);
  print();
}

#TODO testing:

var writeDamageLog = func {
  var output_file = getprop("/sim/fg-home") ~ "/Export/combat-log.txt";
  var buffer = damageLog.get_buffer();
  var str = "\n";
  foreach(entry; buffer) {
      str = str~"    "~entry.time~" "~entry.message~"\n";
  }
  str = str ~ "\n";
  var file = nil;
  if (io.stat(output_file) == nil) {
    file = io.open(output_file, "w");
    io.close(file);
  }
  file = io.open(output_file, "a");
  io.write(file, str);
  io.close(file);
}

setlistener("sim/signals/exit", writeDamageLog, 0, 0);

#screen.property_display.add("payload/armament/MAW-bearing");
#screen.property_display.add("payload/armament/MAW-active");
#screen.property_display.add("payload/armament/MLW-bearing");
#screen.property_display.add("payload/armament/MLW-count");
#screen.property_display.add("payload/armament/MLW-launcher");
#screen.property_display.add("payload/armament/spike");