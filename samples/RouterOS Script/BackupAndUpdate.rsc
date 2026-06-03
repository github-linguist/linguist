# Script name: BackupAndUpdate
#
#----------SCRIPT INFORMATION---------------------------------------------------
#
# Script:  Mikrotik RouterOS automatic backup & update
# Version: 22.01.17
# Created: 07/08/2018
# Updated: 17/01/2022
# Author:  Alexander Tebiev
# Website: https://github.com/beeyev
# You can contact me by e-mail at tebiev@mail.com
#
# IMPORTANT!
# Minimum supported RouterOS version is v6.43.7
#
#----------MODIFY THIS SECTION AS NEEDED----------------------------------------
## Notification e-mail
## (Make sure you have configurated Email settings in Tools -> Email)
:local emailAddress "yourmail@example.com";

## Script mode, possible values: backup, osupdate, osnotify.
# backup 	- 	Only backup will be performed. (default value, if none provided)
#
# osupdate 	- 	The Script will install a new RouterOS if it is available.
#				It will also create backups before and after update process (does not matter what value is set to `forceBackup`)
#				Email will be sent only if a new RouterOS version is available.
#				Change parameter `forceBackup` if you need the script to create backups every time when it runs (even when no updates were found).
#
# osnotify 	- 	The script will send email notification only (without backups) if a new RouterOS is available.
#				Change parameter `forceBackup` if you need the script to create backups every time when it runs.
:local scriptMode "osupdate";

## Additional parameter if you set `scriptMode` to `osupdate` or `osnotify`
# Set `true` if you want the script to perform backup every time it's fired, whatever script mode is set.
:local forceBackup false;

## Backup encryption password, no encryption if no password.
:local backupPassword ""

## If true, passwords will be included in exported config.
:local sensetiveDataInConfig true;

## Update channel. Possible values: stable, long-term, testing, development
:local updateChannel "stable";

## Install only patch versions of RouterOS updates.
## Works only if you set scriptMode to "osupdate"
## Means that new update will be installed only if MAJOR and MINOR version numbers remained the same as currently installed RouterOS.
## Example: v6.43.6 => major.minor.PATCH
## Script will send information if new version is greater than just patch.
:local installOnlyPatchUpdates	false;

##------------------------------------------------------------------------------------------##
#  !!!! DO NOT CHANGE ANYTHING BELOW THIS LINE, IF YOU ARE NOT SURE WHAT YOU ARE DOING !!!!  #
##------------------------------------------------------------------------------------------##

#Script messages prefix
:local SMP "Bkp&Upd:"

:log info "\r\n$SMP script \"Mikrotik RouterOS automatic backup & update\" started.";
:log info "$SMP Script Mode: $scriptMode, forceBackup: $forceBackup";

#Check proper email config
:if ([:len $emailAddress] = 0 or [:len [/tool e-mail get address]] = 0 or [:len [/tool e-mail get from]] = 0) do={
	:log error ("$SMP Email configuration is not correct, please check Tools -> Email. Script stopped.");   
	:error "$SMP bye!";
}

#Check if proper identity name is set
if ([:len [/system identity get name]] = 0 or [/system identity get name] = "MikroTik") do={
	:log warning ("$SMP Please set identity name of your device (System -> Identity), keep it short and informative.");  
};

############### vvvvvvvvv GLOBALS vvvvvvvvv ###############
# Function converts standard mikrotik build versions to the number.
# Possible arguments: paramOsVer
# Example:
# :put [$buGlobalFuncGetOsVerNum paramOsVer=[/system routerboard get current-RouterOS]];
# result will be: 64301, because current RouterOS version is: 6.43.1
:global buGlobalFuncGetOsVerNum do={
	:local osVer $paramOsVer;
	:local osVerNum;
	:local osVerMicroPart;
	:local zro 0;
	:local tmp;
	
	# Replace word `beta` with dot
	:local isBetaPos [:tonum [:find $osVer "beta" 0]];
	:if ($isBetaPos > 1) do={
		:set osVer ([:pick $osVer 0 $isBetaPos] . "." . [:pick $osVer ($isBetaPos + 4) [:len $osVer]]);
	}
	# Replace word `rc` with dot
	:local isRcPos [:tonum [:find $osVer "rc" 0]];
	:if ($isRcPos > 1) do={
		:set osVer ([:pick $osVer 0 $isRcPos] . "." . [:pick $osVer ($isRcPos + 2) [:len $osVer]]);
	}
	
	:local dotPos1 [:find $osVer "." 0];

	:if ($dotPos1 > 0) do={ 

		# AA
		:set osVerNum  [:pick $osVer 0 $dotPos1];
		
		:local dotPos2 [:find $osVer "." $dotPos1];
				#Taking minor version, everything after first dot
		:if ([:len $dotPos2] = 0) 	do={:set tmp [:pick $osVer ($dotPos1+1) [:len $osVer]];}
		#Taking minor version, everything between first and second dots
		:if ($dotPos2 > 0) 			do={:set tmp [:pick $osVer ($dotPos1+1) $dotPos2];}
		
		# AA 0B
		:if ([:len $tmp] = 1) 	do={:set osVerNum "$osVerNum$zro$tmp";}
		# AA BB
		:if ([:len $tmp] = 2) 	do={:set osVerNum "$osVerNum$tmp";}
		
		:if ($dotPos2 > 0) do={ 
			:set tmp [:pick $osVer ($dotPos2+1) [:len $osVer]];
			# AA BB 0C
			:if ([:len $tmp] = 1) do={:set osVerNum "$osVerNum$zro$tmp";}
			# AA BB CC
			:if ([:len $tmp] = 2) do={:set osVerNum "$osVerNum$tmp";}
		} else={
			# AA BB 00
			:set osVerNum "$osVerNum$zro$zro";
		}
	} else={
		# AA 00 00
		:set osVerNum "$osVer$zro$zro$zro$zro";
	}

	:return $osVerNum;
}


# Function creates backups (system and config) and returns array with names
# Possible arguments: 
#	`backupName` 			| string	| backup file name, without extension!
#	`backupPassword`		| string 	|
#	`sensetiveDataInConfig`	| boolean 	|
# Example:
# :put [$buGlobalFuncCreateBackups name="daily-backup"];
:global buGlobalFuncCreateBackups do={
	:log info ("$SMP Global function \"buGlobalFuncCreateBackups\" was fired.");  
	
	:local backupFileSys "$backupName.backup";
	:local backupFileConfig "$backupName.rsc";
	:local backupNames {$backupFileSys;$backupFileConfig};

	## Make system backup
	:if ([:len $backupPassword] = 0) do={
		/system backup save dont-encrypt=yes name=$backupName;
	} else={
		/system backup save password=$backupPassword name=$backupName;
	}
	:log info ("$SMP System backup created. $backupFileSys");   

	## Export config file
	:if ($sensetiveDataInConfig = true) do={
		# since RouterOS v7 it needs to be set precise that we want to export sensitive data
		:if ([:pick [/system package update get installed-version] 0 1] < 7) do={
			:execute "/export compact terse file=$backupName";
		} else={
			:execute "/export compact show-sensitive terse file=$backupName";
		}
	} else={
		/export compact hide-sensitive terse file=$backupName;
	}
	:log info ("$SMP Config file was exported. $backupFileConfig, the script execution will be paused for a moment.");   

	#Delay after creating backups
	:delay 20s;	
	:return $backupNames;
}

:global buGlobalVarUpdateStep;
############### ^^^^^^^^^ GLOBALS ^^^^^^^^^ ###############

:local scriptVersion	"22.01.17";

#Current date time in format: 2020jan15-221324 
:local dateTime ([:pick [/system clock get date] 7 11] . [:pick [/system clock get date] 0 3] . [:pick [/system clock get date] 4 6] . "-" . [:pick [/system clock get time] 0 2] . [:pick [/system clock get time] 3 5] . [:pick [/system clock get time] 6 8]);

:local deviceOsVerInst 			[/system package update get installed-version];
:local deviceOsVerInstNum 		[$buGlobalFuncGetOsVerNum paramOsVer=$deviceOsVerInst];
:local deviceOsVerAvail 		"";
:local deviceOsVerAvailNum 		0;
:local deviceRbModel			[/system routerboard get model];
:local deviceRbSerialNumber 	[/system routerboard get serial-number];
:local deviceRbCurrentFw 		[/system routerboard get current-firmware];
:local deviceRbUpgradeFw 		[/system routerboard get upgrade-firmware];
:local deviceIdentityName 		[/system identity get name];
:local deviceIdentityNameShort 	[:pick $deviceIdentityName 0 18]
:local deviceUpdateChannel 		[/system package update get channel];

:local isOsUpdateAvailable 	false;
:local isOsNeedsToBeUpdated	false;

:local isSendEmailRequired	true;

:local mailSubject   		"$SMP Device - $deviceIdentityNameShort.";
:local mailBody 	 		"";

:local mailBodyDeviceInfo	"\r\n\r\nDevice information: \r\nIdentity: $deviceIdentityName \r\nModel: $deviceRbModel \r\nSerial number: $deviceRbSerialNumber \r\nCurrent RouterOS: $deviceOsVerInst ($[/system package update get channel]) $[/system resource get build-time] \r\nCurrent routerboard FW: $deviceRbCurrentFw \r\nDevice uptime: $[/system resource get uptime]";
:local mailBodyCopyright 	"\r\n\r\nMikrotik RouterOS automatic backup & update (ver. $scriptVersion) \r\nhttps://github.com/beeyev/Mikrotik-RouterOS-automatic-backup-and-update";
:local changelogUrl			("Check RouterOS changelog: https://mikrotik.com/download/changelogs/" . $updateChannel . "-release-tree");

:local backupName 			"$deviceIdentityName.$deviceRbModel.$deviceRbSerialNumber.v$deviceOsVerInst.$deviceUpdateChannel.$dateTime";
:local backupNameBeforeUpd	"backup_before_update_$backupName";
:local backupNameAfterUpd	"backup_after_update_$backupName";

:local backupNameFinal		$backupName;
:local mailAttachments		[:toarray ""];


:local updateStep $buGlobalVarUpdateStep;
:do {/system script environment remove buGlobalVarUpdateStep;} on-error={}
:if ([:len $updateStep] = 0) do={
	:set updateStep 1;
}


## 	STEP ONE: Creating backups, checking for new RouterOs version and sending email with backups,
## 	steps 2 and 3 are fired only if script is set to automatically update device and if new RouterOs is available.
:if ($updateStep = 1) do={
	:log info ("$SMP Performing the first step.");   

	# Checking for new RouterOS version
	if ($scriptMode = "osupdate" or $scriptMode = "osnotify") do={
		log info ("$SMP Checking for new RouterOS version. Current version is: $deviceOsVerInst");
		/system package update set channel=$updateChannel;
		/system package update check-for-updates;
		:delay 5s;
		:set deviceOsVerAvail [/system package update get latest-version];

		# If there is a problem getting information about available RouterOS from server
		:if ([:len $deviceOsVerAvail] = 0) do={
			:log warning ("$SMP There is a problem getting information about new RouterOS from server.");
			:set mailSubject	($mailSubject . " Error: No data about new RouterOS!")
			:set mailBody 		($mailBody . "Error occured! \r\nMikrotik couldn't get any information about new RouterOS from server! \r\nWatch additional information in device logs.")
		} else={
			#Get numeric version of OS
			:set deviceOsVerAvailNum [$buGlobalFuncGetOsVerNum paramOsVer=$deviceOsVerAvail];

			# Checking if OS on server is greater than installed one.
			:if ($deviceOsVerAvailNum > $deviceOsVerInstNum) do={
				:set isOsUpdateAvailable true;
				:log info ("$SMP New RouterOS is available! $deviceOsVerAvail");
			} else={
				:set isSendEmailRequired false;
				:log info ("$SMP System is already up to date.");
				:set mailSubject ($mailSubject . " No new OS updates.");
				:set mailBody 	 ($mailBody . "Your system is up to date.");
			}
		};
	} else={
		:set scriptMode "backup";
	};

	if ($forceBackup = true) do={
		# In this case the script will always send email, because it has to create backups
		:set isSendEmailRequired true;
	}

	# if new OS version is available to install
	if ($isOsUpdateAvailable = true and $isSendEmailRequired = true) do={
		# If we only need to notify about new available version
		if ($scriptMode = "osnotify") do={
			:set mailSubject 	($mailSubject . " New RouterOS is available! v.$deviceOsVerAvail.")
			:set mailBody 		($mailBody . "New RouterOS version is available to install: v.$deviceOsVerAvail ($updateChannel) \r\n$changelogUrl")
		}

		# if we need to initiate RouterOs update process
		if ($scriptMode = "osupdate") do={
			:set isOsNeedsToBeUpdated true;
			# if we need to install only patch updates
			:if ($installOnlyPatchUpdates = true) do={
				#Check if Major and Minor builds are the same.
				:if ([:pick $deviceOsVerInstNum 0 ([:len $deviceOsVerInstNum]-2)] = [:pick $deviceOsVerAvailNum 0 ([:len $deviceOsVerAvailNum]-2)]) do={
					:log info ("$SMP New patch version of RouterOS firmware is available.");   
				} else={
					:log info ("$SMP New major or minor version of RouterOS firmware is available. You need to update it manually.");
					:set mailSubject 	($mailSubject . " New RouterOS: v.$deviceOsVerAvail needs to be installed manually.");
					:set mailBody 		($mailBody . "New major or minor RouterOS version is available to install: v.$deviceOsVerAvail ($updateChannel). \r\nYou chose to automatically install only patch updates, so this major update you need to install manually. \r\n$changelogUrl");
					:set isOsNeedsToBeUpdated false;
				}
			}

			#Check again, because this variable could be changed during checking for installing only patch updats
			if ($isOsNeedsToBeUpdated = true) do={
				:log info ("$SMP New RouterOS is going to be installed! v.$deviceOsVerInst -> v.$deviceOsVerAvail");
				:set mailSubject	($mailSubject . " New RouterOS is going to be installed! v.$deviceOsVerInst -> v.$deviceOsVerAvail.");
				:set mailBody 		($mailBody . "Your Mikrotik will be updated to the new RouterOS version from v.$deviceOsVerInst to v.$deviceOsVerAvail (Update channel: $updateChannel) \r\nFinal report with the detailed information will be sent when update process is completed. \r\nIf you have not received second email in the next 5 minutes, then probably something went wrong. (Check your device logs)");
				#!! There is more code connected to this part and first step at the end of the script.
			}
		
		}
	}

	## Checking If the script needs to create a backup
	:log info ("$SMP Checking If the script needs to create a backup.");
	if ($forceBackup = true or $scriptMode = "backup" or $isOsNeedsToBeUpdated = true) do={
		:log info ("$SMP Creating system backups.");
		if ($isOsNeedsToBeUpdated = true) do={
			:set backupNameFinal $backupNameBeforeUpd;
		};
		if ($scriptMode != "backup") do={
			:set mailBody ($mailBody . "\r\n\r\n");
		};

		:set mailSubject	($mailSubject . " Backup was created.");
		:set mailBody		($mailBody . "System backups were created and attached to this email.");

		:set mailAttachments [$buGlobalFuncCreateBackups backupName=$backupNameFinal backupPassword=$backupPassword sensetiveDataInConfig=$sensetiveDataInConfig];
	} else={
		:log info ("$SMP There is no need to create a backup.");
	}

	# Combine fisrst step email
	:set mailBody ($mailBody . $mailBodyDeviceInfo . $mailBodyCopyright);
}

## 	STEP TWO: (after first reboot) routerboard firmware upgrade
## 	steps 2 and 3 are fired only if script is set to automatically update device and if new RouterOs is available.
:if ($updateStep = 2) do={
	:log info ("$SMP Performing the second step.");   
	## RouterOS is the latest, let's check for upgraded routerboard firmware
	if ($deviceRbCurrentFw != $deviceRbUpgradeFw) do={
		:set isSendEmailRequired false;
		:delay 10s;
		:log info "$SMP Upgrading routerboard firmware from v.$deviceRbCurrentFw to v.$deviceRbUpgradeFw";
		## Start the upgrading process
		/system routerboard upgrade;
		## Wait until the upgrade is completed
		:delay 5s;
		:log info "$SMP routerboard upgrade process was completed, going to reboot in a moment!";
		## Set scheduled task to send final report on the next boot, task will be deleted when is is done. (That is why you should keep original script name)
		/system schedule add name=BKPUPD-FINAL-REPORT-ON-NEXT-BOOT on-event=":delay 5s; /system scheduler remove BKPUPD-FINAL-REPORT-ON-NEXT-BOOT; :global buGlobalVarUpdateStep 3; :delay 10s; /system script run BackupAndUpdate;" start-time=startup interval=0;
		## Reboot system to boot with new firmware
		/system reboot;
	} else={
		:log info "$SMP It appers that your routerboard is already up to date, skipping this step.";
		:set updateStep 3;
	};
}

## 	STEP THREE: Last step (after second reboot) sending final report
## 	steps 2 and 3 are fired only if script is set to automatically update device and if new RouterOs is available.
:if ($updateStep = 3) do={
	:log info ("$SMP Performing the third step.");   
	:log info "Bkp&Upd: RouterOS and routerboard upgrade process was completed. New RouterOS version: v.$deviceOsVerInst, routerboard firmware: v.$deviceRbCurrentFw.";
	## Small delay in case mikrotik needs some time to initialize connections
	:log info "$SMP The final email with report and backups of upgraded system will be sent in a minute.";
	:delay 1m;
	:set mailSubject	($mailSubject . " RouterOS Upgrade is completed, new version: v.$deviceOsVerInst!");
	:set mailBody 	  	"RouterOS and routerboard upgrade process was completed. \r\nNew RouterOS version: v.$deviceOsVerInst, routerboard firmware: v.$deviceRbCurrentFw. \r\n$changelogUrl \r\n\r\nBackups of the upgraded system are in the attachment of this email.  $mailBodyDeviceInfo $mailBodyCopyright";
	:set mailAttachments [$buGlobalFuncCreateBackups backupName=$backupNameAfterUpd backupPassword=$backupPassword sensetiveDataInConfig=$sensetiveDataInConfig];
}

# Remove functions from global environment to keep it fresh and clean.
:do {/system script environment remove buGlobalFuncGetOsVerNum;} on-error={}
:do {/system script environment remove buGlobalFuncCreateBackups;} on-error={}

##
## SENDING EMAIL
##
# Trying to send email with backups in attachment.

:if ($isSendEmailRequired = true) do={
	:log info "$SMP Sending email message, it will take around half a minute...";
	:do {/tool e-mail send to=$emailAddress subject=$mailSubject body=$mailBody file=$mailAttachments;} on-error={
		:delay 5s;
		:log error "$SMP could not send email message ($[/tool e-mail get last-status]). Going to try it again in a while."

		:delay 5m;

		:do {/tool e-mail send to=$emailAddress subject=$mailSubject body=$mailBody file=$mailAttachments;} on-error={
			:delay 5s;
			:log error "$SMP could not send email message ($[/tool e-mail get last-status]) for the second time."

			if ($isOsNeedsToBeUpdated = true) do={
				:set isOsNeedsToBeUpdated false;
				:log warning "$SMP script is not going to initialise update process due to inability to send backups to email."
			}
		}
	}

	:delay 30s;
	
	:if ([:len $mailAttachments] > 0 and [/tool e-mail get last-status] = "succeeded") do={
		:log info "$SMP File system cleanup."
		/file remove $mailAttachments; 
		:delay 2s;
	}
	
}


# Fire RouterOs update process
if ($isOsNeedsToBeUpdated = true) do={

	## Set scheduled task to upgrade routerboard firmware on the next boot, task will be deleted when upgrade is done. (That is why you should keep original script name)
	/system schedule add name=BKPUPD-UPGRADE-ON-NEXT-BOOT on-event=":delay 5s; /system scheduler remove BKPUPD-UPGRADE-ON-NEXT-BOOT; :global buGlobalVarUpdateStep 2; :delay 10s; /system script run BackupAndUpdate;" start-time=startup interval=0;
   
   :log info "$SMP everything is ready to install new RouterOS, going to reboot in a moment!"
	## command is reincarnation of the "upgrade" command - doing exactly the same but under a different name
	/system package update install;
}

:log info "$SMP script \"Mikrotik RouterOS automatic backup & update\" completed it's job.\r\n";