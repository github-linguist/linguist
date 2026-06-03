# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

## Firefox Brand
##
## Firefox must be treated as a brand, and kept in English.
## It cannot be:
## - Declined to adapt to grammatical case.
## - Transliterated.
## - Translated.
##
## Reference: https://www.mozilla.org/styleguide/communications/translation/

-brand-short-name = Firefox
-vendor-short-name = Mozilla
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

-sync-brand-short-name = Sync

# “Sync” can be localized, “Firefox” must be treated as a brand,
# and kept in English.
-sync-brand-name = Firefox Sync

# “Account” can be localized, “Firefox” must be treated as a brand,
# and kept in English.
-fxaccount-brand-name = Firefox Account
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

app-manager-window =
    .title = Application details
    .style = width: 30em; min-height: 20em;

app-manager-remove =
    .label = Remove
    .accesskey = R

app-manager-handle-webfeeds = The following applications can be used to handle Web Feeds.

# Variables:
#   $type (String) - the URI scheme of the link (e.g. mailto:)
app-manager-handle-protocol = The following applications can be used to handle { $type } links.

# Variables:
#   $type (String) - the MIME type (e.g. application/binary)
app-manager-handle-file = The following applications can be used to handle { $type } content.

## These strings are followed, on a new line,
## by the URL or path of the application.

app-manager-web-app-info = This web application is hosted at:
app-manager-local-app-info = This application is located at:
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

blocklist-window =
    .title = Block Lists
    .style = width: 55em

blocklist-desc = You can choose which list { -brand-short-name } will use to block Web elements that may track your browsing activity.
blocklist-close-key =
    .key = w

blocklist-treehead-list =
    .label = List

blocklist-button-cancel =
    .label = Cancel
    .accesskey = C

blocklist-button-ok =
    .label = Save Changes
    .accesskey = S
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

clear-site-data-window =
    .title = Clear Data
    .style = width: 35em

clear-site-data-description = Clearing all cookies and site data stored by { -brand-short-name } may sign you out of websites and remove offline web content. Clearing cache data will not affect your logins.

clear-site-data-close-key =
    .key = w

# The parameters in parentheses in this string describe disk usage
# in the format ($amount $unit), e.g. "Cookies and Site Data (24 KB)"
# Variables:
#   $amount (Number) - Amount of site data currently stored on disk
#   $unit (String) - Abbreviation of the unit that $amount is in, e.g. "MB"
clear-site-data-cookies-with-data =
    .label = Cookies and Site Data ({ $amount } { $unit })
    .accesskey = S

# This string is a placeholder for while the data used to fill
# clear-site-data-cookies-with-data is loading. This placeholder is usually
# only shown for a very short time (< 1s), so it should be very similar
# or the same as clear-site-data-cookies-with-data (except the amount and unit),
# to avoid flickering.
clear-site-data-cookies-empty =
    .label = Cookies and Site Data
    .accesskey = S

clear-site-data-cookies-info = You may get signed out of websites if cleared

# The parameters in parentheses in this string describe disk usage
# in the format ($amount $unit), e.g. "Cached Web Content (24 KB)"
# Variables:
#   $amount (Number) - Amount of cache currently stored on disk
#   $unit (String) - Abbreviation of the unit that $amount is in, e.g. "MB"
clear-site-data-cache-with-data =
    .label = Cached Web Content ({ $amount } { $unit })
    .accesskey = W

# This string is a placeholder for while the data used to fill
# clear-site-data-cache-with-data is loading. This placeholder is usually
# only shown for a very short time (< 1s), so it should be very similar
# or the same as clear-site-data-cache-with-data (except the amount and unit),
# to avoid flickering.
clear-site-data-cache-empty =
    .label = Cached Web Content
    .accesskey = W

clear-site-data-cache-info = Will require websites to reload images and data

clear-site-data-cancel =
    .label = Cancel
    .accesskey = C

clear-site-data-clear =
    .label = Clear
    .accesskey = l
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

colors-window =
    .title = Colors
    .style =
        { PLATFORM() ->
            [macos] width: 41em
           *[other] width: 38em
        }

colors-close-key =
    .key = w

colors-page-override = Override the colors specified by the page with your selections above
    .accesskey = O

colors-page-override-option-always =
    .label = Always
colors-page-override-option-auto =
    .label = Only with High Contrast themes
colors-page-override-option-never =
    .label = Never

colors-text-and-background = Text and Background

colors-text-header = Text
    .accesskey = T

colors-background = Background
    .accesskey = B

colors-use-system =
    .label = Use system colors
    .accesskey = s

colors-underline-links =
    .label = Underline links
    .accesskey = U

colors-links-header = Link Colors

colors-unvisited-links = Unvisited Links
    .accesskey = L

colors-visited-links = Visited Links
    .accesskey = V
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

connection-window =
    .title = Connection Settings
    .style =
        { PLATFORM() ->
            [macos] width: 44em
           *[other] width: 49em
        }

connection-close-key =
    .key = w

connection-disable-extension =
    .label = Disable Extension

connection-proxy-configure = Configure Proxy Access to the Internet

connection-proxy-option-no =
    .label = No proxy
    .accesskey = y
connection-proxy-option-system =
    .label = Use system proxy settings
    .accesskey = U
connection-proxy-option-auto =
    .label = Auto-detect proxy settings for this network
    .accesskey = w
connection-proxy-option-manual =
    .label = Manual proxy configuration
    .accesskey = M

connection-proxy-http = HTTP Proxy
    .accesskey = x
connection-proxy-http-port = Port
    .accesskey = P
connection-proxy-http-share =
    .label = Use this proxy server for all protocols
    .accesskey = s

connection-proxy-ssl = SSL Proxy
    .accesskey = L
connection-proxy-ssl-port = Port
    .accesskey = o

connection-proxy-ftp = FTP Proxy
    .accesskey = F
connection-proxy-ftp-port = Port
    .accesskey = r

connection-proxy-socks = SOCKS Host
    .accesskey = C
connection-proxy-socks-port = Port
    .accesskey = t

connection-proxy-socks4 =
    .label = SOCKS v4
    .accesskey = K
connection-proxy-socks5 =
    .label = SOCKS v5
    .accesskey = v
connection-proxy-noproxy = No Proxy for
    .accesskey = N

connection-proxy-noproxy-desc = Example: .mozilla.org, .net.nz, 192.168.1.0/24

connection-proxy-autotype =
    .label = Automatic proxy configuration URL
    .accesskey = A

connection-proxy-reload =
    .label = Reload
    .accesskey = e

connection-proxy-autologin =
    .label = Do not prompt for authentication if password is saved
    .accesskey = i
    .tooltip = This option silently authenticates you to proxies when you have saved credentials for them. You will be prompted if authentication fails.

connection-proxy-socks-remote-dns =
    .label = Proxy DNS when using SOCKS v5
    .accesskey = D
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

containers-window-new =
    .title = Add New Container
    .style = width: 45em
# Variables
#   $name (String) - Name of the container
containers-window-update =
    .title = { $name } Container Preferences
    .style = width: 45em

containers-window-close =
    .key = w

# This is a term to store style to be applied
# on the three labels in the containers add/edit dialog:
#   - name
#   - icon
#   - color
#
# Using this term and referencing it in the `.style` attribute
# of the three messages ensures that all three labels
# will be aligned correctly.
-containers-labels-style = min-width: 4rem

containers-name-label = Name
    .accesskey = N
    .style = { -containers-labels-style }

containers-name-text =
    .placeholder = Enter a container name

containers-icon-label = Icon
    .accesskey = I
    .style = { -containers-labels-style }

containers-color-label = Color
    .accesskey = o
    .style = { -containers-labels-style }

containers-button-done =
    .label = Done
    .accesskey = D

containers-color-blue =
    .label = Blue
containers-color-turquoise =
    .label = Turquoise
containers-color-green =
    .label = Green
containers-color-yellow =
    .label = Yellow
containers-color-orange =
    .label = Orange
containers-color-red =
    .label = Red
containers-color-pink =
    .label = Pink
containers-color-purple =
    .label = Purple

containers-icon-fingerprint =
    .label = Fingerprint
containers-icon-briefcase =
    .label = Briefcase
# String represents a money sign but currently uses a dollar sign
# so don't change to local currency. See Bug 1291672.
containers-icon-dollar =
    .label = Dollar sign
containers-icon-cart =
    .label = Shopping cart
containers-icon-circle =
    .label = Dot
containers-icon-vacation =
    .label = Vacation
containers-icon-gift =
    .label = Gift
containers-icon-food =
    .label = Food
containers-icon-fruit =
    .label = Fruit
containers-icon-pet =
    .label = Pet
containers-icon-tree =
    .label = Tree
containers-icon-chill =
    .label = Chill
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

fonts-window =
    .title = Fonts

fonts-window-close =
    .key = w

## Font groups by language

fonts-langgroup-header = Fonts for
    .accesskey = F

fonts-langgroup-arabic =
    .label = Arabic
fonts-langgroup-armenian =
    .label = Armenian
fonts-langgroup-bengali =
    .label = Bengali
fonts-langgroup-simpl-chinese =
    .label = Simplified Chinese
fonts-langgroup-trad-chinese-hk =
    .label = Traditional Chinese (Hong Kong)
fonts-langgroup-trad-chinese =
    .label = Traditional Chinese (Taiwan)
fonts-langgroup-cyrillic =
    .label = Cyrillic
fonts-langgroup-devanagari =
    .label = Devanagari
fonts-langgroup-ethiopic =
    .label = Ethiopic
fonts-langgroup-georgian =
    .label = Georgian
fonts-langgroup-el =
    .label = Greek
fonts-langgroup-gujarati =
    .label = Gujarati
fonts-langgroup-gurmukhi =
    .label = Gurmukhi
fonts-langgroup-japanese =
    .label = Japanese
fonts-langgroup-hebrew =
    .label = Hebrew
fonts-langgroup-kannada =
    .label = Kannada
fonts-langgroup-khmer =
    .label = Khmer
fonts-langgroup-korean =
    .label = Korean
# Translate "Latin" as the name of Latin (Roman) script, not as the name of the Latin language.
fonts-langgroup-latin =
    .label = Latin
fonts-langgroup-malayalam =
    .label = Malayalam
fonts-langgroup-math =
    .label = Mathematics
fonts-langgroup-odia =
    .label = Odia
fonts-langgroup-sinhala =
    .label = Sinhala
fonts-langgroup-tamil =
    .label = Tamil
fonts-langgroup-telugu =
    .label = Telugu
fonts-langgroup-thai =
    .label = Thai
fonts-langgroup-tibetan =
    .label = Tibetan
fonts-langgroup-canadian =
    .label = Unified Canadian Syllabary
fonts-langgroup-other =
    .label = Other Writing Systems

## Default fonts and their sizes

fonts-proportional-header = Proportional
    .accesskey = P

fonts-default-serif =
    .label = Serif
fonts-default-sans-serif =
    .label = Sans Serif

fonts-proportional-size = Size
    .accesskey = z

fonts-serif = Serif
    .accesskey = S

fonts-sans-serif = Sans-serif
    .accesskey = n

fonts-monospace = Monospace
    .accesskey = M

fonts-monospace-size = Size
    .accesskey = e

fonts-minsize = Minimum font size
    .accesskey = o

fonts-minsize-none =
    .label = None

fonts-allow-own =
    .label = Allow pages to choose their own fonts, instead of your selections above
    .accesskey = A

## Text Encodings
##
## Translate the encoding names as adjectives for an encoding, not as the name
## of the language.

fonts-languages-fallback-header = Text Encoding for Legacy Content
fonts-languages-fallback-desc = This text encoding is used for legacy content that fails to declare its encoding.

fonts-languages-fallback-label = Fallback Text Encoding
    .accesskey = T

fonts-languages-fallback-name-auto =
    .label = Default for Current Locale
fonts-languages-fallback-name-arabic =
    .label = Arabic
fonts-languages-fallback-name-baltic =
    .label = Baltic
fonts-languages-fallback-name-ceiso =
    .label = Central European, ISO
fonts-languages-fallback-name-cewindows =
    .label = Central European, Microsoft
fonts-languages-fallback-name-simplified =
    .label = Chinese, Simplified
fonts-languages-fallback-name-traditional =
    .label = Chinese, Traditional
fonts-languages-fallback-name-cyrillic =
    .label = Cyrillic
fonts-languages-fallback-name-greek =
    .label = Greek
fonts-languages-fallback-name-hebrew =
    .label = Hebrew
fonts-languages-fallback-name-japanese =
    .label = Japanese
fonts-languages-fallback-name-korean =
    .label = Korean
fonts-languages-fallback-name-thai =
    .label = Thai
fonts-languages-fallback-name-turkish =
    .label = Turkish
fonts-languages-fallback-name-vietnamese =
    .label = Vietnamese
fonts-languages-fallback-name-other =
    .label = Other (incl. Western European)
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

languages-window =
    .title = Languages
    .style = width: 30em

languages-close-key =
    .key = w

languages-description = Web pages are sometimes offered in more than one language. Choose languages for displaying these web pages, in order of preference

languages-customize-spoof-english =
    .label = Request English versions of web pages for enhanced privacy

languages-customize-moveup =
    .label = Move Up
    .accesskey = U

languages-customize-movedown =
    .label = Move Down
    .accesskey = D

languages-customize-remove =
    .label = Remove
    .accesskey = R

languages-customize-select-language =
    .placeholder = Select a language to add…

languages-customize-add =
    .label = Add
    .accesskey = A
# Variables:
#   $num - default value of the `dom.ipc.processCount` pref.
default-content-process-count
    .label = { $num } (default)
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

permissions-window =
    .title = Exceptions
    .style = width: 45em

permissions-close-key =
    .key = w

permissions-address = Address of website
    .accesskey = d

permissions-block =
    .label = Block
    .accesskey = B

permissions-session =
    .label = Allow for Session
    .accesskey = S

permissions-allow =
    .label = Allow
    .accesskey = A

permissions-site-name =
    .label = Website

permissions-status =
    .label = Status

permissions-remove =
    .label = Remove Website
    .accesskey = R

permissions-remove-all =
    .label = Remove All Websites
    .accesskey = e

permissions-button-cancel =
    .label = Cancel
    .accesskey = C

permissions-button-ok =
    .label = Save Changes
    .accesskey = S

permissions-searchbox =
    .placeholder = Search Website
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

do-not-track-description = Send websites a “Do Not Track” signal that you don’t want to be tracked
do-not-track-learn-more = Learn more
do-not-track-option-default =
    .label = Only when using Tracking Protection
do-not-track-option-always =
    .label = Always

pref-page =
    .title =
        { PLATFORM() ->
            [windows] Options
           *[other] Preferences
        }

# This is used to determine the width of the search field in about:preferences,
# in order to make the entire placeholder string visible
#
# Notice: The value of the `.style` attribute is a CSS string, and the `width`
# is the name of the CSS property. It is intended only to adjust the element's width.
# Do not translate.
search-input =
    .style = width: 15.4em

# This is used to determine the width of the search field in about:preferences,
# in order to make the entire placeholder string visible
#
# Please keep the placeholder string short to avoid truncation.
#
# Notice: The value of the `.style` attribute is a CSS string, and the `width`
# is the name of the CSS property. It is intended only to adjust the element's width.
# Do not translate.
search-input-box =
    .style = width: 15.4em
    .placeholder =
        { PLATFORM() ->
            [windows] Find in Options
           *[other] Find in Preferences
        }

policies-notice =
    { PLATFORM() ->
        [windows] Your organization has disabled the ability to change some options.
       *[other] Your organization has disabled the ability to change some preferences.
    }

pane-general-title = General
category-general =
    .tooltiptext = { pane-general-title }

pane-home-title = Home
category-home =
    .tooltiptext = { pane-home-title }

pane-search-title = Search
category-search =
    .tooltiptext = { pane-search-title }

pane-privacy-title = Privacy & Security
category-privacy =
    .tooltiptext = { pane-privacy-title }

# The word "account" can be translated, do not translate or transliterate "Firefox".
pane-sync-title = Firefox Account
category-sync =
    .tooltiptext = { pane-sync-title }

help-button-label = { -brand-short-name } Support

focus-search =
    .key = f

close-button =
    .aria-label = Close

## Browser Restart Dialog

feature-enable-requires-restart = { -brand-short-name } must restart to enable this feature.
feature-disable-requires-restart = { -brand-short-name } must restart to disable this feature.
should-restart-title = Restart { -brand-short-name }
should-restart-ok = Restart { -brand-short-name } now
cancel-no-restart-button = Cancel
restart-later = Restart Later

## Extension Control Notifications
##
## These strings are used to inform the user
## about changes made by extensions to browser settings.
##
## <img data-l10n-name="icon"/> is going to be replaced by the extension icon.
##
## Variables:
##   $name (String): name of the extension

# This string is shown to notify the user that their home page
# is being controlled by an extension.
extension-controlled-homepage-override = An extension, <img data-l10n-name="icon"/> { $name }, is controlling your home page.

# This string is shown to notify the user that their new tab page
# is being controlled by an extension.
extension-controlled-new-tab-url = An extension, <img data-l10n-name="icon"/> { $name }, is controlling your New Tab page.

# This string is shown to notify the user that the default search engine
# is being controlled by an extension.
extension-controlled-default-search = An extension, <img data-l10n-name="icon"/> { $name }, has set your default search engine.

# This string is shown to notify the user that Container Tabs
# are being enabled by an extension.
extension-controlled-privacy-containers = An extension, <img data-l10n-name="icon"/> { $name }, requires Container Tabs.

# This string is shown to notify the user that their tracking protection preferences
# are being controlled by an extension.
extension-controlled-websites-tracking-protection-mode = An extension, <img data-l10n-name="icon"/> { $name }, is controlling tracking protection.

# This string is shown to notify the user that their proxy configuration preferences
# are being controlled by an extension.
extension-controlled-proxy-config = An extension, <img data-l10n-name="icon"/> { $name }, is controlling how { -brand-short-name } connects to the internet.

# This string is shown after the user disables an extension to notify the user
# how to enable an extension that they disabled.
#
# <img data-l10n-name="addons-icon"/> will be replaced with Add-ons icon
# <img data-l10n-name="menu-icon"/> will be replaced with Menu icon
extension-controlled-enable = To enable the extension go to <img data-l10n-name="addons-icon"/> Add-ons in the <img data-l10n-name="menu-icon"/> menu.

## Preferences UI Search Results

search-results-header = Search Results

# `<span data-l10n-name="query"></span>` will be replaced by the search term.
search-results-empty-message =
    { PLATFORM() ->
        [windows] Sorry! There are no results in Options for “<span data-l10n-name="query"></span>”.
       *[other] Sorry! There are no results in Preferences for “<span data-l10n-name="query"></span>”.
    }

search-results-help-link = Need help? Visit <a data-l10n-name="url">{ -brand-short-name } Support</a>

## General Section

startup-header = Startup

# { -brand-short-name } will be 'Firefox Developer Edition',
# since this setting is only exposed in Firefox Developer Edition
separate-profile-mode =
    .label = Allow { -brand-short-name } and Firefox to run at the same time
use-firefox-sync = Tip: This uses separate profiles. Use { -sync-brand-short-name } to share data between them.
get-started-not-logged-in = Sign in to { -sync-brand-short-name }…
get-started-configured = Open { -sync-brand-short-name } preferences

always-check-default =
    .label = Always check if { -brand-short-name } is your default browser
    .accesskey = y

is-default = { -brand-short-name } is currently your default browser
is-not-default = { -brand-short-name } is not your default browser

set-as-my-default-browser =
    .label = Make Default…
    .accesskey = D

startup-page = When { -brand-short-name } starts
    .accesskey = s

startup-user-homepage =
    .label = Show your home page
startup-blank-page =
    .label = Show a blank page
startup-prev-session =
    .label = Show your windows and tabs from last time

startup-restore-previous-session =
    .label = Restore previous session
    .accesskey = s

disable-extension =
    .label = Disable Extension

home-page-header = Home page

tabs-group-header = Tabs

ctrl-tab-recently-used-order =
    .label = Ctrl+Tab cycles through tabs in recently used order
    .accesskey = T

open-new-link-as-tabs =
    .label = Open links in tabs instead of new windows
    .accesskey = w

warn-on-close-multiple-tabs =
    .label = Warn you when closing multiple tabs
    .accesskey = m

warn-on-open-many-tabs =
    .label = Warn you when opening multiple tabs might slow down { -brand-short-name }
    .accesskey = d

switch-links-to-new-tabs =
    .label = When you open a link in a new tab, switch to it immediately
    .accesskey = h

show-tabs-in-taskbar =
    .label = Show tab previews in the Windows taskbar
    .accesskey = k

browser-containers-enabled =
    .label = Enable Container Tabs
    .accesskey = n

browser-containers-learn-more = Learn more

browser-containers-settings =
    .label = Settings…
    .accesskey = i

containers-disable-alert-title = Close All Container Tabs?
containers-disable-alert-desc =
    { $tabCount ->
        [one] If you disable Container Tabs now, { $tabCount } container tab will be closed. Are you sure you want to disable Container Tabs?
       *[other] If you disable Container Tabs now, { $tabCount } container tabs will be closed. Are you sure you want to disable Container Tabs?
    }

containers-disable-alert-ok-button =
    { $tabCount ->
        [one] Close { $tabCount } Container Tab
       *[other] Close { $tabCount } Container Tabs
    }
containers-disable-alert-cancel-button = Keep enabled

containers-remove-alert-title = Remove This Container?

# Variables:
#   $count (Number) - Number of tabs that will be closed.
containers-remove-alert-msg =
    { $count ->
        [one] If you remove this Container now, { $count } container tab will be closed. Are you sure you want to remove this Container?
       *[other] If you remove this Container now, { $count } container tabs will be closed. Are you sure you want to remove this Container?
    }

containers-remove-ok-button = Remove this Container
containers-remove-cancel-button = Don’t remove this Container


## General Section - Language & Appearance

language-and-appearance-header = Language and Appearance

fonts-and-colors-header = Fonts & Colors

default-font = Default font
    .accesskey = D
default-font-size = Size
    .accesskey = S

advanced-fonts =
    .label = Advanced…
    .accesskey = A

colors-settings =
    .label = Colors…
    .accesskey = C

language-header = Language

choose-language-description = Choose your preferred language for displaying pages

choose-button =
    .label = Choose…
    .accesskey = o

translate-web-pages =
    .label = Translate web content
    .accesskey = T

# The <img> element is replaced by the logo of the provider
# used to provide machine translations for web pages.
translate-attribution = Translations by <img data-l10n-name="logo"/>

translate-exceptions =
    .label = Exceptions…
    .accesskey = x

check-user-spelling =
    .label = Check your spelling as you type
    .accesskey = t

## General Section - Files and Applications

files-and-applications-title = Files and Applications

download-header = Downloads

download-save-to =
    .label = Save files to
    .accesskey = v

download-choose-folder =
    .label =
        { PLATFORM() ->
            [macos] Choose…
           *[other] Browse…
        }
    .accesskey =
        { PLATFORM() ->
            [macos] e
           *[other] o
        }

download-always-ask-where =
    .label = Always ask you where to save files
    .accesskey = A

applications-header = Applications

applications-description = Choose how { -brand-short-name } handles the files you download from the web or the applications you use while browsing.

applications-filter =
    .placeholder = Search file types or applications

applications-type-column =
    .label = Content Type
    .accesskey = T

applications-action-column =
    .label = Action
    .accesskey = A

drm-content-header = Digital Rights Management (DRM) Content

play-drm-content =
    .label = Play DRM-controlled content
    .accesskey = P

play-drm-content-learn-more = Learn more

update-application-title = { -brand-short-name } Updates

update-application-description = Keep { -brand-short-name } up to date for the best performance, stability, and security.

update-application-info = Version { $version } <a>What's new</a>

update-application-version = Version { $version } <a data-l10n-name="learn-more">What’s new</a>

update-history =
    .label = Show Update History…
    .accesskey = p

update-application-allow-description = Allow { -brand-short-name } to

update-application-auto =
    .label = Automatically install updates (recommended)
    .accesskey = A

update-application-check-choose =
    .label = Check for updates but let you choose to install them
    .accesskey = C

update-application-manual =
    .label = Never check for updates (not recommended)
    .accesskey = N

update-application-use-service =
    .label = Use a background service to install updates
    .accesskey = b

update-enable-search-update =
    .label = Automatically update search engines
    .accesskey = e

## General Section - Performance

performance-title = Performance

performance-use-recommended-settings-checkbox =
    .label = Use recommended performance settings
    .accesskey = U

performance-use-recommended-settings-desc = These settings are tailored to your computer’s hardware and operating system.

performance-settings-learn-more = Learn more

performance-allow-hw-accel =
    .label = Use hardware acceleration when available
    .accesskey = r

performance-limit-content-process-option = Content process limit
    .accesskey = l

performance-limit-content-process-enabled-desc = Additional content processes can improve performance when using multiple tabs, but will also use more memory.
performance-limit-content-process-disabled-desc = Modifying the number of content processes is only possible with multiprocess { -brand-short-name }. <a>Learn how to check if multiprocess is enabled</a>

performance-limit-content-process-blocked-desc = Modifying the number of content processes is only possible with multiprocess { -brand-short-name }. <a data-l10n-name="learn-more">Learn how to check if multiprocess is enabled</a>

# Variables:
#   $num - default value of the `dom.ipc.processCount` pref.
performance-default-content-process-count =
    .label = { $num } (default)

## General Section - Browsing

browsing-title = Browsing

browsing-use-autoscroll =
    .label = Use autoscrolling
    .accesskey = a

browsing-use-smooth-scrolling =
    .label = Use smooth scrolling
    .accesskey = m

browsing-use-onscreen-keyboard =
    .label = Show a touch keyboard when necessary
    .accesskey = c

browsing-use-cursor-navigation =
    .label = Always use the cursor keys to navigate within pages
    .accesskey = k

browsing-search-on-start-typing =
    .label = Search for text when you start typing
    .accesskey = x

## General Section - Proxy

network-proxy-title = Network Proxy

network-proxy-connection-description = Configure how { -brand-short-name } connects to the internet.

network-proxy-connection-learn-more = Learn More

network-proxy-connection-settings =
    .label = Settings…
    .accesskey = e

## Home Section

home-new-windows-tabs-header = New Windows and Tabs

home-new-windows-tabs-description2 = Choose what you see when you open your homepage, new windows, and new tabs.

## Home Section - Home Page Customization

home-homepage-mode-label = Homepage and new windows

home-newtabs-mode-label = New tabs

home-restore-defaults =
    .label = Restore Defaults
    .accesskey = R

# "Firefox" should be treated as a brand and kept in English,
# while "Home" and "(Default)" can be localized.
home-mode-choice-default =
    .label = Firefox Home (Default)

home-mode-choice-custom =
    .label = Custom URLs…

home-mode-choice-blank =
    .label = Blank Page

home-homepage-custom-url =
    .placeholder = Paste a URL…

# This string has a special case for '1' and [other] (default). If necessary for
# your language, you can add {$tabCount} to your translations and use the
# standard CLDR forms, or only use the form for [other] if both strings should
# be identical.
use-current-pages =
    .label =
        { $tabCount ->
            [1] Use Current Page
           *[other] Use Current Pages
        }
    .accesskey = C

choose-bookmark =
    .label = Use Bookmark…
    .accesskey = B

restore-default =
    .label = Restore to Default
    .accesskey = R

## Search Section

search-bar-header = Search Bar
search-bar-hidden =
    .label = Use the address bar for search and navigation
search-bar-shown =
    .label = Add search bar in toolbar

search-engine-default-header = Default Search Engine
search-engine-default-desc = Choose the default search engine to use in the address bar and search bar.

search-suggestions-option =
    .label = Provide search suggestions
    .accesskey = s

search-show-suggestions-url-bar-option =
    .label = Show search suggestions in address bar results
    .accesskey = l

# This string describes what the user will observe when the system
# prioritizes search suggestions over browsing history in the results
# that extend down from the address bar. In the original English string,
# "ahead" refers to location (appearing most proximate to), not time
# (appearing before).
search-show-suggestions-above-history-option =
    .label = Show search suggestions ahead of browsing history in address bar results

search-suggestions-cant-show = Search suggestions will not be shown in location bar results because you have configured { -brand-short-name } to never remember history.

search-one-click-header = One-Click Search Engines

search-one-click-desc = Choose the alternative search engines that appear below the address bar and search bar when you start to enter a keyword.

search-choose-engine-column =
    .label = Search Engine
search-choose-keyword-column =
    .label = Keyword

search-restore-default =
    .label = Restore Default Search Engines
    .accesskey = D

search-remove-engine =
    .label = Remove
    .accesskey = R

search-find-more-link = Find more search engines

# This warning is displayed when the chosen keyword is already in use
# ('Duplicate' is an adjective)
search-keyword-warning-title = Duplicate Keyword
# Variables:
#   $name (String) - Name of a search engine.
search-keyword-warning-engine = You have chosen a keyword that is currently in use by “{ $name }”. Please select another.
search-keyword-warning-bookmark = You have chosen a keyword that is currently in use by a bookmark. Please select another.

## Containers Section

containers-back-link = « Go Back
containers-header = Container Tabs
containers-add-button =
    .label = Add New Container
    .accesskey = A

containers-preferences-button =
    .label = Preferences
containers-remove-button =
    .label = Remove

## Sync Section - Signed out

sync-signedout-caption = Take Your Web With You
sync-signedout-description = Synchronize your bookmarks, history, tabs, passwords, add-ons, and preferences across all your devices.

sync-signedout-account-title = Connect with a { -fxaccount-brand-name }
sync-signedout-account-create = Don’t have an account? Get started
    .accesskey = c

sync-signedout-account-signin =
    .label = Sign In…
    .accesskey = I

# This message contains two links and two icon images.
#   `<img data-l10n-name="android-icon"/>` - Android logo icon
#   `<a data-l10n-name="android-link">` - Link to Android Download
#   `<img data-l10n-name="ios-icon">` - iOS logo icon
#   `<a data-l10n-name="ios-link">` - Link to iOS Download
#
# They can be moved within the sentence as needed to adapt
# to your language, but should not be changed or translated.
sync-mobile-promo = Download Firefox for <img data-l10n-name="android-icon"/> <a data-l10n-name="android-link">Android</a> or <img data-l10n-name="ios-icon"/> <a data-l10n-name="ios-link">iOS</a> to sync with your mobile device.

## Sync Section - Signed in

sync-profile-picture =
    .tooltiptext = Change profile picture

sync-disconnect =
    .label = Disconnect…
    .accesskey = D

sync-manage-account = Manage account
    .accesskey = o

sync-signedin-unverified = { $email } is not verified.
sync-signedin-login-failure = Please sign in to reconnect { $email }

sync-resend-verification =
    .label = Resend Verification
    .accesskey = d

sync-remove-account =
    .label = Remove Account
    .accesskey = R

sync-sign-in =
    .label = Sign in
    .accesskey = g

sync-signedin-settings-header = Sync Settings
sync-signedin-settings-desc = Choose what to synchronize on your devices using { -brand-short-name }

sync-engine-bookmarks =
    .label = Bookmarks
    .accesskey = m

sync-engine-history =
    .label = History
    .accesskey = r

sync-engine-tabs =
    .label = Open tabs
    .tooltiptext = A list of what’s open on all synced devices
    .accesskey = t

sync-engine-logins =
    .label = Logins
    .tooltiptext = Usernames and passwords you’ve saved
    .accesskey = L

sync-engine-addresses =
    .label = Addresses
    .tooltiptext = Postal addresses you’ve saved (desktop only)
    .accesskey = e

sync-engine-creditcards =
    .label = Credit cards
    .tooltiptext = Names, numbers and expiry dates (desktop only)
    .accesskey = C

sync-engine-addons =
    .label = Add-ons
    .tooltiptext = Extensions and themes for Firefox desktop
    .accesskey = A

sync-engine-prefs =
    .label =
        { PLATFORM() ->
            [windows] Options
           *[other] Preferences
        }
    .tooltiptext = General, Privacy, and Security settings you’ve changed
    .accesskey = s

sync-device-name-header = Device Name

sync-device-name-change =
    .label = Change Device Name…
    .accesskey = h

sync-device-name-cancel =
    .label = Cancel
    .accesskey = n

sync-device-name-save =
    .label = Save
    .accesskey = v

sync-mobilepromo-single = Connect another device

sync-mobilepromo-multi = Manage devices

sync-tos-link = Terms of Service

sync-fxa-privacy-notice = Privacy Notice

## Privacy Section

privacy-header = Browser Privacy

## Privacy Section - Forms

forms-header = Forms & Passwords
forms-ask-to-save-logins =
    .label = Ask to save logins and passwords for websites
    .accesskey = r
forms-exceptions =
    .label = Exceptions…
    .accesskey = x
forms-saved-logins =
    .label = Saved Logins…
    .accesskey = L
forms-master-pw-use =
    .label = Use a master password
    .accesskey = U
forms-master-pw-change =
    .label = Change Master Password
    .accesskey = M

## Privacy Section - History

history-header = History

# This label is followed, on the same line, by a dropdown list of options
# (Remember history, etc.).
# In English it visually creates a full sentence, e.g.
# "Firefox will" + "Remember history".
#
# If this doesn't work for your language, you can translate this message:
#   - Simply as "Firefox", moving the verb into each option.
#     This will result in "Firefox" + "Will remember history", etc.
#   - As a stand-alone message, for example "Firefox history settings:".
history-remember-label = { -brand-short-name } will
    .accesskey = w

history-remember-option-all =
    .label = Remember history
history-remember-option-never =
    .label = Never remember history
history-remember-option-custom =
    .label = Use custom settings for history

history-remember-description = { -brand-short-name } will remember your browsing, download, form and search history.
history-dontremember-description = { -brand-short-name } will use the same settings as private browsing, and will not remember any history as you browse the Web.

history-private-browsing-permanent =
    .label = Always use private browsing mode
    .accesskey = p

history-remember-option =
    .label = Remember my browsing and download history
    .accesskey = b

history-remember-search-option =
    .label = Remember search and form history
    .accesskey = f

history-clear-on-close-option =
    .label = Clear history when { -brand-short-name } closes
    .accesskey = r

history-clear-on-close-settings =
    .label = Settings…
    .accesskey = t

history-clear-button =
    .label = Clear History…
    .accesskey = s

## Privacy Section - Site Data

sitedata-header = Cookies and Site Data

sitedata-learn-more = Learn more

sitedata-accept-cookies-option =
    .label = Accept cookies and site data from websites (recommended)
    .accesskey = A

sitedata-block-cookies-option =
    .label = Block cookies and site data (may cause websites to break)
    .accesskey = B

sitedata-keep-until = Keep until
    .accesskey = u

sitedata-keep-until-expire =
    .label = They expire
sitedata-keep-until-closed =
    .label = { -brand-short-name } is closed

sitedata-accept-third-party-desc = Accept third-party cookies and site data
    .accesskey = y

sitedata-accept-third-party-always-option =
    .label = Always
sitedata-accept-third-party-visited-option =
    .label = From visited
sitedata-accept-third-party-never-option =
    .label = Never

sitedata-clear =
    .label = Clear Data…
    .accesskey = l

sitedata-settings =
    .label = Manage Data…
    .accesskey = M

sitedata-cookies-exceptions =
    .label = Exceptions…
    .accesskey = E

## Privacy Section - Address Bar

addressbar-header = Address Bar

addressbar-suggest = When using the address bar, suggest

addressbar-locbar-history-option =
    .label = Browsing history
    .accesskey = h
addressbar-locbar-bookmarks-option =
    .label = Bookmarks
    .accesskey = k
addressbar-locbar-openpage-option =
    .label = Open tabs
    .accesskey = O

addressbar-suggestions-settings = Change preferences for search engine suggestions

## Privacy Section - Tracking

tracking-header = Tracking Protection

tracking-desc = Tracking Protection blocks online trackers that collect your browsing data across multiple websites. <a data-l10n-name="learn-more">Learn more about Tracking Protection and your privacy</a>

tracking-mode-label = Use Tracking Protection to block known trackers

tracking-mode-always =
    .label = Always
    .accesskey = y
tracking-mode-private =
    .label = Only in private windows
    .accesskey = l
tracking-mode-never =
    .label = Never
    .accesskey = N

# This string is displayed if privacy.trackingprotection.ui.enabled is set to false.
# This currently happens on the release and beta channel.
tracking-pbm-label = Use Tracking Protection in Private Browsing to block known trackers
    .accesskey = v

tracking-exceptions =
    .label = Exceptions…
    .accesskey = x

tracking-change-block-list =
    .label = Change Block List…
    .accesskey = C

## Privacy Section - Permissions

permissions-header = Permissions

permissions-location = Location
permissions-location-settings =
    .label = Settings…
    .accesskey = t

permissions-camera = Camera
permissions-camera-settings =
    .label = Settings…
    .accesskey = t

permissions-microphone = Microphone
permissions-microphone-settings =
    .label = Settings…
    .accesskey = t

permissions-notification = Notifications
permissions-notification-settings =
    .label = Settings…
    .accesskey = t
permissions-notification-link = Learn more

permissions-notification-pause =
    .label = Pause notifications until { -brand-short-name } restarts
    .accesskey = n

permissions-block-popups =
    .label = Block pop-up windows
    .accesskey = B

permissions-block-popups-exceptions =
    .label = Exceptions…
    .accesskey = E

permissions-addon-install-warning =
    .label = Warn you when websites try to install add-ons
    .accesskey = W

permissions-addon-exceptions =
    .label = Exceptions…
    .accesskey = E

permissions-a11y-privacy-checkbox =
    .label = Prevent accessibility services from accessing your browser
    .accesskey = a

permissions-a11y-privacy-link = Learn more

## Privacy Section - Data Collection

collection-header = { -brand-short-name } Data Collection and Use

collection-description = We strive to provide you with choices and collect only what we need to provide and improve { -brand-short-name } for everyone. We always ask permission before receiving personal information.
collection-privacy-notice = Privacy Notice

collection-health-report =
    .label = Allow { -brand-short-name } to send technical and interaction data to { -vendor-short-name }
    .accesskey = r
collection-health-report-link = Learn more

# This message is displayed above disabled data sharing options in developer builds
# or builds with no Telemetry support available.
collection-health-report-disabled = Data reporting is disabled for this build configuration

collection-browser-errors =
    .label = Allow { -brand-short-name } to send browser error reports (including error messages) to { -vendor-short-name }
    .accesskey = b
collection-browser-errors-link = Learn more

collection-backlogged-crash-reports =
    .label = Allow { -brand-short-name } to send backlogged crash reports on your behalf
    .accesskey = c
collection-backlogged-crash-reports-link = Learn more

## Privacy Section - Security
##
## It is important that wording follows the guidelines outlined on this page:
## https://developers.google.com/safe-browsing/developers_guide_v2#AcceptableUsage

security-header = Security

security-browsing-protection = Deceptive Content and Dangerous Software Protection

security-enable-safe-browsing =
    .label = Block dangerous and deceptive content
    .accesskey = B
security-enable-safe-browsing-link = Learn more

security-block-downloads =
    .label = Block dangerous downloads
    .accesskey = d

security-block-uncommon-software =
    .label = Warn you about unwanted and uncommon software
    .accesskey = c

## Privacy Section - Certificates

certs-header = Certificates

certs-personal-label = When a server requests your personal certificate

certs-select-auto-option =
    .label = Select one automatically
    .accesskey = S

certs-select-ask-option =
    .label = Ask you every time
    .accesskey = A

certs-enable-ocsp =
    .label = Query OCSP responder servers to confirm the current validity of certificates
    .accesskey = Q

certs-view =
    .label = View Certificates…
    .accesskey = C

certs-devices =
    .label = Security Devices…
    .accesskey = D
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

select-bookmark-window =
    .title = Set Home Page
    .style = width: 32em;

select-bookmark-desc = Choose a Bookmark to be your Home Page. If you choose a folder, the Bookmarks in that folder will be opened in Tabs.
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.


## Settings

site-data-settings-window =
    .title = Manage Cookies and Site Data

site-data-search-textbox =
    .placeholder = Search websites
    .accesskey = S

site-data-column-host =
    .label = Site
site-data-column-cookies =
    .label = Cookies
site-data-column-storage =
    .label = Storage
site-data-column-last-used =
    .label = Last Used

site-data-remove-selected =
    .label = Remove Selected
    .accesskey = R

site-data-button-cancel =
    .label = Cancel
    .accesskey = C

site-data-button-save =
    .label = Save Changes
    .accesskey = a

## Removing

site-data-removing-window =
    .title = { site-data-removing-header }

site-data-removing-header = Removing Cookies and Site Data

site-data-removing-desc = Removing cookies and site data may log you out of websites. Are you sure you want to make the changes?

site-data-removing-table = Cookies and site data for the following websites will be removed
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

translation-window =
    .title = Exceptions - Translation
    .style = width: 36em

translation-close-key =
    .key = w

translation-languages-disabled-desc = Translation will not be offered for the following languages

translation-languages-column =
    .label = Languages

translation-languages-button-remove =
    .label = Remove Language
    .accesskey = R

translation-languages-button-remove-all =
    .label = Remove All Languages
    .accesskey = e

translation-sites-disabled-desc = Translation will not be offered for the following sites

translation-sites-column =
    .label = Websites

translation-sites-button-remove =
    .label = Remove Site
    .accesskey = S

translation-sites-button-remove-all =
    .label = Remove All Sites
    .accesskey = i

translation-button-close =
    .label = Close
    .accesskey = C

