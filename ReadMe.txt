InternationalMountains

================================================================================
DESCRIPTION:

This sample demonstrates some ways to incorporate and manage localized data in
an iPhone application.  

It displays a list of mountains in a TableView, along with detail information
about a selected mountain in a detail view.  The mountain information is 
localized in three languages (English, French and Traditional Chinese).  The
detail information uses the current iPhone Region Format to format date and
measurement data.

The sample also provides a simple example of a localized application preferences
bundle.  There is one application setting which controls whether or not the
initial list of mountains is lexographically sorted or not.

Details
-------

This sample demonstrates the following basic localization practices:

Localized application UI: There are localized versions of the two nib files
for the application (MainWindow and DetailViewController) in the project 
Resources folder.  The DetailViewController shows a localized default text 
label.

Localized application data: There are localized versions of the application
data (Mountains.plist) in the project Resources folder.  We find and load the
correct version at run-time in MountainDataController by letting NSBundle
retrieve the correct localized file.

Localized, formatted text: Localizable strings for the main application are kept
in Localizable.strings in the project Resources folder, and accessed using
NSLocalizedString.  In DetailViewController:updateLabelWithMountain we use
a localized format string along with NSFormatter (and some manual measurement 
conversion) to produce a localized string with the correct date and number
formats for the current Region Format settings.  Also, note
that all .strings files for this sample are saved using UTF-16 encoding.

Localized Info.plist strings: We provide one localized string for the application
info.plist (CFBundleDisplayName) in the InfoPlist.strings file in the 
project Resources folder.

Localized application settings bundle: The project also contains a Settings.bundle,
which has been updated to include localized settings schema strings.  This was
done by manually adding localization folder (e.g. "fr.lproj") using Finder
("Show Package Contents" on Settings.bundle).

Note that for the title of the top-level settings panel, the name used will be
the same as the application name, which can be localized through the 
CFBundleDisplayName InfoPlist.strings entry.

================================================================================
BUILD REQUIREMENTS:

Xcode 5.0 or later, iOS SDK 7.0 or later

================================================================================
RUNTIME REQUIREMENTS:

iOS 6.0 or later

================================================================================
PACKAGING LIST:

main.m - Main source file for this sample.

InternationalMountainsAppDelegate.h/.m - The application's delegate to setup 
its window and content.

RootViewController.h/.m - The root-level view controller, who's view contains a 
tableview, and itself is contained in a navigation controller.  This controller
uses localized strings from Localized.strings to set the view title for the
navigation controller, and also owns and initializes a MountainDataController.

DetailViewController.h/.m - The detail-level view controller, pushed onto the
navigation controller stack when a user picks a specific mountain.  Uses
localized strings and a local-specific NSFormatter to format date information.

Mountain.h/.m - A simple data class to contain data for one mountain.

MountainDataController.h/.m - A simple data controller that loads a NSArray
of Mountains from the correct Mountains.plist data.

InfoPlist.strings - strings file containing localized strings for the application's
Info.plist.  For this application we localize the CFBundleDisplayName that
will be shown for the application in the iPhone springboard.  

Localizable.strings - strings file containing localized strings used at 
run-time for the main application.

DetailViewController.xib - Localized NIB file for detail view UI.

MainWindow.xib - Localized NIB file for main window UI.

Mountains.plist - A localized plist file that contains mountain data.

Info.plist - The info.plist file for the application.

Settings.bundle - A localized settings bundle used for the application 
preferences.  

================================================================================
FURTHER INFORMATION:

Please see also the section titled "Internationalizing Applications" in the
iPhone OS Programming Guide, as well as the more general "Internationalization
Programming Topics" document.  

Also, for descriptions on what can be localized in the application Settings.bundle, 
see the "Settings Application Schema Reference" document.

================================================================================
USING THE SAMPLE:

Via Settings, General, International, Language, set your current iPhone
language to one of the supported localizations the app supports (English, 
French or Chinese).  Launch the application, and navigate through the presented
localized list of mountains.  Selecting a mountain will present a detail view
with information about the mountain (height and date the mountain was climbed,
if applicable)

Changing the Region Format under Settings, General, International, Region Format
will change the format used to display the mountain height and climbed date
shown in the detail view UI.

Additionally, under Settings, you should find localized settings for the
Mountains application.  The one "Sort" toggle setting controls whether or
not the main list of mountains will be lexographically sorted or not.

Note that changing the language setting to a different language supported by
the application will properly switch to use resources for that language 
(application UI and data, as well as application name shown in the springboard,
and the application settings).  

Also, note that if the iPhone language settings are set to a language the application
does not support, the system will attempt to use the closest approximate 
language resources for the application.  For example, even though the 
application only supports traditional Chinese characters, not simplified Chinese,
setting the phone to simplified Chinese will automatically have the application
use its traditional Chinese character resources, rather than defaulting to
English.

ADDING A NEW LOCALIZATION
================================================================================

In this sample, adding a new localization can be done in XCode.  For each 
localized resource file (InfoPlist.strings, Localizable.strings, 
DetailViewController.xib, MainWindow.xib and Mountains.plist), you can 
select the file group in the Groups & Files pane and then pick File->Get Info.
In the Localized Group Info dialog, under the General tab, you can see the
current list of localizations, and pick "Add Localization" to add a new 
localization, specifying a language+locale identifier name formatted to match
BCP 47 standards, that is using a language and locale supported by the device.
XCode will make a new .lproj folder in the project directory with the specified
language+locale identifier, and copy the project native development region
version of the resource file (in this case, "en") to this folder, which can
then be localized.  

You can also add localizations manually by creating appropriately named
.lproj folders, copying resource files to this folder, localizing the files,
and then adding them manually as existing files to the project in XCode.

For the localized resources in the Settings.bundle bundle, you will have to
add new localizations manually, as XCode cannot interact directly with
resources in included bundles.

Please note that localizing the application icon is not currently supported
at this time on iOS.

================================================================================
CHANGES FROM PREVIOUS VERSIONS:

1.3 - Adopts Storyboard, Base Internationalization, and Auto Layout, adds code to elegantly handle system locale change notification and app setting change, and refines Traditional Chinese localization.
1.2 - Upgrade for iOS 7.0 SDK, adopts current best practices for Objective-C (including use of properties, autosynthesis, and literals)
1.1 - Updates to make project compatible with iOS 4.0 SDK.
1.0 - First release.

================================================================================
Copyright (C) 2009-2014 Apple Inc. All rights reserved.
