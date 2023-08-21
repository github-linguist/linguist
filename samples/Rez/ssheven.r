#include "ssheven-constants.r"
#include "ssheven-icons.r"

#include "Dialogs.r"
#include "Processes.r"
#include "MacTypes.r"
#include "Finder.r"
#include "Menus.r"
#include "Controls.r"
#include "ControlDefinitions.r"

resource 'DLOG' (DLOG_ABOUT, purgeable) {
	{0, 0, 148, 420},
	dBoxProc,
	visible,
	goAway,
	0,
	DITL_ABOUT,
	"",
	centerMainScreen
};

resource 'DITL' (DITL_ABOUT, purgeable) {
	{
		{ 10, 10, 138, 138 },
		Picture { enabled, PICT_ABOUT };

		{ 34, 160, 54, 410},
		StaticText { disabled, "ssheven" };

		{ 64, 160, 84, 410},
		StaticText { disabled, SSHEVEN_LONG_VERSION };

		{ 94, 160, 114, 410},
		StaticText { disabled, "https://github.com/cy384/ssheven" };
	}
};

resource 'DLOG' (DLOG_CONNECT) {
	{ 50, 100, 195, 420 },
	dBoxProc,
	visible,
	noGoAway,
	0,
	DITL_CONNECT,
	"",
	centerMainScreen
};

resource 'DITL' (DITL_CONNECT) {
	{
		{ 115, 320-10-80, 135, 320-10 },
		Button { enabled, "Connect" };

		{ 190-10-20-5, 320-10-80-5, 190-10+5, 320-10+5 },
		UserItem { enabled };

		{ 10, 10, 30, 270 },
		StaticText { enabled, "Address and port" };

		{ 35, 15, 51, 260 },
		EditText { enabled, "10.0.2.2" };

		{ 35, 270, 51, 305 },
		EditText { enabled, "22" };

		{ 60, 10, 80, 310 },
		StaticText { enabled, "Username" };

		{ 85, 15, 101, 150 },
		EditText { enabled, "" };

		{ 115, 10, 135, 90 },
		Button { enabled, "Cancel" };

		{ 85, 160, 101, 245 },
		RadioButton { enabled, "Password" };

		{ 85, 250, 101, 310 },
		RadioButton { enabled, "Key" };
	}
};

resource 'DITL' (DITL_OT) {
	{
		{ 50, 260, 70, 340 },
		Button { enabled, "Exit" };

		{ 10, 70, 30, 340 },
		StaticText { enabled, "Open Transport required but not found!" };
	}
};

resource 'ALRT' (ALRT_OT, purgeable) {
	{ 50, 100, 50+80, 100+350 },
	DITL_OT,

	/* OK means draw default border on first button */
	{
		OK, visible, silent,
		OK, visible, silent,
		OK, visible, silent,
		OK, visible, silent
	},
	alertPositionMainScreen
};

resource 'DITL' (DITL_TM) {
	{
		{ 50, 260, 70, 340 },
		Button { enabled, "Exit" };

		{ 10, 70, 30, 340 },
		StaticText { enabled, "Thread Manager required but not found!" };
	}
};

resource 'ALRT' (ALRT_TM, purgeable) {
	{ 50, 100, 50+80, 100+350 },
	DITL_TM,

	/* OK means draw default border on first button */
	{
		OK, visible, silent,
		OK, visible, silent,
		OK, visible, silent,
		OK, visible, silent
	},
	alertPositionMainScreen
};

resource 'DITL' (DITL_CPU_SLOW) {
	{
		{ 50, 260, 70, 340 },
		Button { enabled, "OK" };

		{ 10, 70, 30, 340 },
		StaticText { enabled, "Your CPU is probably too slow!" };
	}
};

resource 'ALRT' (ALRT_CPU_SLOW, purgeable) {
	{ 50, 100, 50+80, 100+350 },
	DITL_CPU_SLOW,

	/* OK means draw default border on first button */
	{
		OK, visible, silent,
		OK, visible, silent,
		OK, visible, silent,
		OK, visible, silent
	},
	alertPositionMainScreen
};

resource 'DITL' (DITL_CPU_BAD) {
	{
		{ 50, 260, 70, 340 },
		Button { enabled, "OK" };

		{ 10, 70, 30, 340 },
		StaticText { enabled, "SSHeven requires a 68020 or later!" };
	}
};

resource 'ALRT' (ALRT_CPU_BAD, purgeable) {
	{ 50, 100, 50+80, 100+350 },
	DITL_CPU_BAD,

	/* OK means draw default border on first button */
	{
		OK, visible, silent,
		OK, visible, silent,
		OK, visible, silent,
		OK, visible, silent
	},
	alertPositionMainScreen
};

resource 'DLOG' (DLOG_PASSWORD) {
	{ 50, 100, 150, 420 },
	dBoxProc,
	visible,
	noGoAway,
	0,
	DLOG_PASSWORD,
	"",
	centerMainScreen
};

resource 'DITL' (DITL_PASSWORD) {
	{
		{ 70, 320-10-80, 90, 320-10 },
		Button { enabled, "OK" };

		{ 190-10-20-5, 320-10-80-5, 190-10+5, 320-10+5 },
		UserItem { enabled };

		{ 10, 10, 30, 310 },
		StaticText { enabled, "Enter password:" };

		{ 35, 15, 51, 305 },
		EditText { enabled, "" };

		{ 240, 10, 240, 10 },
		EditText { enabled, "" };

		{ 70, 10, 90, 90 },
		Button { enabled, "Cancel" };
	}
};

resource 'DLOG' (DLOG_KEY_PASSWORD) {
	{ 50, 100, 150, 420 },
	dBoxProc,
	visible,
	noGoAway,
	0,
	DLOG_KEY_PASSWORD,
	"",
	centerMainScreen
};

resource 'DITL' (DITL_KEY_PASSWORD) {
	{
		{ 70, 320-10-80, 90, 320-10 },
		Button { enabled, "OK" };

		{ 190-10-20-5, 320-10-80-5, 190-10+5, 320-10+5 },
		UserItem { enabled };

		{ 10, 10, 30, 310 },
		StaticText { enabled, "Key decryption password:" };

		{ 35, 15, 51, 305 },
		EditText { enabled, "" };

		{ 240, 10, 240, 10 },
		EditText { enabled, "" };

		{ 70, 10, 90, 90 },
		Button { enabled, "Cancel" };
	}
};

resource 'DITL' (DITL_PW_FAIL) {
	{
		{ 50, 260, 70, 340 },
		Button { enabled, "OK" };

		{ 10, 70, 30, 340 },
		StaticText { enabled, "Server rejected username/password!" };
	}
};

resource 'ALRT' (ALRT_PW_FAIL, purgeable) {
	{ 50, 100, 50+80, 100+350 },
	DITL_PW_FAIL,

	/* OK means draw default border on first button */
	{
		OK, visible, silent,
		OK, visible, silent,
		OK, visible, silent,
		OK, visible, silent
	},
	alertPositionMainScreen
};

resource 'DITL' (DITL_PUBKEY) {
	{
		{ 50, 260, 70, 340 },
		Button { enabled, "OK" };

		{ 10, 70, 30, 340 },
		StaticText { enabled, "Please locate your public key." };
	}
};

resource 'ALRT' (ALRT_PUBKEY, purgeable) {
	{ 50, 100, 50+80, 100+350 },
	DITL_PUBKEY,

	/* OK means draw default border on first button */
	{
		OK, visible, silent,
		OK, visible, silent,
		OK, visible, silent,
		OK, visible, silent
	},
	alertPositionMainScreen
};

resource 'DITL' (DITL_PRIVKEY) {
	{
		{ 50, 260, 70, 340 },
		Button { enabled, "OK" };

		{ 10, 70, 30, 340 },
		StaticText { enabled, "Please locate your private key." };
	}
};

resource 'ALRT' (ALRT_PRIVKEY, purgeable) {
	{ 50, 100, 50+80, 100+350 },
	DITL_PRIVKEY,

	/* OK means draw default border on first button */
	{
		OK, visible, silent,
		OK, visible, silent,
		OK, visible, silent,
		OK, visible, silent
	},
	alertPositionMainScreen
};

resource 'DITL' (DITL_FILE_FAIL) {
	{
		{ 50, 260, 70, 340 },
		Button { enabled, "OK" };

		{ 10, 70, 30, 340 },
		StaticText { enabled, "Couldn't load key files!" };
	}
};

resource 'ALRT' (ALRT_FILE_FAIL, purgeable) {
	{ 50, 100, 50+80, 100+350 },
	DITL_FILE_FAIL,

	/* OK means draw default border on first button */
	{
		OK, visible, silent,
		OK, visible, silent,
		OK, visible, silent,
		OK, visible, silent
	},
	alertPositionMainScreen
};

resource 'DLOG' (DLOG_NEW_HOST) {
	{ 40, 30, 40+100, 30+390 },
	dBoxProc,
	visible,
	noGoAway,
	0,
	DLOG_NEW_HOST,
	"",
	centerMainScreen
};

resource 'DITL' (DITL_NEW_HOST) {
	{
		{ 70, 300, 70+20, 300+80 },
		Button { enabled, "Reject" };

		{ 70, 300, 70+20, 300+80 },
		UserItem { enabled };

		{ 10, 20, 10+20, 20+320 },
		StaticText { enabled, "Unrecognized server public key.  SHA256 hash:" };

		{ 30, 20, 30+20, 20+360 },
		StaticText { enabled, "01234567890123456789012345678901234567890123456789" };

		{ 70, 10, 90, 90 },
		Button { enabled, "Accept" };
	}
};

resource 'SIZE' (-1) {
	reserved,
	acceptSuspendResumeEvents,
	reserved,
	canBackground,
	doesActivateOnFGSwitch,
	backgroundAndForeground,
	dontGetFrontClicks,
	ignoreChildDiedEvents,
	is32BitCompatible,
	notHighLevelEventAware,
	onlyLocalHLEvents,
	notStationeryAware,
	dontUseTextEditServices,
	reserved,
	reserved,
	reserved,
	SSHEVEN_MINIMUM_PARTITION,
	SSHEVEN_REQUIRED_PARTITION
};

/* see macintosh tb essentials page 7-31 */
/* yes, we need two */
/* first one displayed in version field of info window */
resource 'vers' (1, purgeable) {
	SSHEVEN_VERSION_MAJOR, SSHEVEN_VERSION_MINOR,
	SSHEVEN_RELEASE_TYPE, SSHEVEN_VERSION_PRERELEASE,
	SSHEVEN_RELEASE_REGION,
	SSHEVEN_VERSION,
	SSHEVEN_LONG_VERSION
};

/* second one displayed beneath icon at top of info window */
resource 'vers' (2, purgeable) {
	SSHEVEN_VERSION_MAJOR, SSHEVEN_VERSION_MINOR,
	SSHEVEN_RELEASE_TYPE, SSHEVEN_VERSION_PRERELEASE,
	SSHEVEN_RELEASE_REGION,
	SSHEVEN_VERSION,
	SSHEVEN_LONG_VERSION
};

/* signature resource */
type 'SSH7' as 'STR ';
resource 'SSH7' (0, purgeable) {
	SSHEVEN_DESCRIPTION
};

/* application -> icon relation */
resource 'FREF' (SSHEVEN_APPLICATION_ICON, purgeable) {
	'APPL', 0, ""
};

/* preferences file -> icon relation */
resource 'FREF' (SSHEVEN_FILE_ICON, purgeable) {
	'SH7p', 1, ""
};

resource 'BNDL' (128, purgeable) {
	'SSH7', 0,
	{
		'ICN#', {0, SSHEVEN_APPLICATION_ICON, 1, SSHEVEN_FILE_ICON},
		'FREF', {0, SSHEVEN_APPLICATION_ICON, 1, SSHEVEN_FILE_ICON}
	}
};

resource 'MBAR' (MBAR_SSHEVEN, preload)
{
	{ MENU_APPLE, MENU_FILE, MENU_EDIT };
};

resource 'MENU' (MENU_APPLE) {
	MENU_APPLE, textMenuProc;
	allEnabled, enabled;
	apple;
	{
		"About ssheven...", noIcon, noKey, noMark, plain;
		"-", noIcon, noKey, noMark, plain;
	}
};

resource 'MENU' (MENU_FILE) {
	MENU_FILE, textMenuProc;
	allEnabled, enabled;
	"File";
	{
		"Connect...", noIcon, "K", noMark, plain;
		"Disconnect", noIcon, "D", noMark, plain;
		"-", noIcon, noKey, noMark, plain;
		"Preferences...", noIcon, noKey, noMark, plain;
		"-", noIcon, noKey, noMark, plain;
		"Quit", noIcon, "Q", noMark, plain;
	}
};

resource 'MENU' (MENU_EDIT) {
	MENU_EDIT, textMenuProc;
	allEnabled, enabled;
	"Edit";
	{
		"Undo", noIcon, "Z", noMark, plain;
		"-", noIcon, noKey, noMark, plain;
		"Cut", noIcon, "X", noMark, plain;
		"Copy", noIcon, "C", noMark, plain;
		"Paste", noIcon, "V", noMark, plain;
		"Clear", noIcon, noKey, noMark, plain;
		"Select All", noIcon, "A", noMark, plain;
		"-", noIcon, noKey, noMark, plain;
		"Show Clipboard", noIcon, noKey, noMark, plain;
	}
};

resource 'MENU' (MENU_COLOR) {
	MENU_COLOR, textMenuProc;
	allEnabled, enabled;
	"Color";
	{
		"Black", noIcon, noKey, noMark, plain;
		"Red", noIcon, noKey, noMark, plain;
		"Green", noIcon, noKey, noMark, plain;
		"Yellow", noIcon, noKey, noMark, plain;
		"Blue", noIcon, noKey, noMark, plain;
		"Magenta", noIcon, noKey, noMark, plain;
		"Cyan", noIcon, noKey, noMark, plain;
		"White", noIcon, noKey, noMark, plain;
	}
};

resource 'MENU' (MENU_TERM_TYPE) {
	MENU_TERM_TYPE, textMenuProc;
	allEnabled, enabled;
	"Type";
	{
		"Monochrome (faster)", noIcon, noKey, noMark, plain;
		"Color (slower)", noIcon, noKey, noMark, plain;
	}
};

resource 'MENU' (MENU_FONT_SIZE) {
	MENU_FONT_SIZE, textMenuProc;
	allEnabled, enabled;
	"Size";
	{
		"9", noIcon, noKey, noMark, plain;
		"10", noIcon, noKey, noMark, plain;
		"12", noIcon, noKey, noMark, plain;
		"14", noIcon, noKey, noMark, plain;
		"18", noIcon, noKey, noMark, plain;
		"24", noIcon, noKey, noMark, plain;
		"36", noIcon, noKey, noMark, plain;
	}
};

resource 'DLOG' (DLOG_PREFERENCES) {
	{ 50, 100, 205, 420 },
	dBoxProc,
	visible,
	noGoAway,
	0,
	DITL_PREFERENCES,
	"",
	centerMainScreen
};

resource 'DITL' (DITL_PREFERENCES) {
	{
		{ 125, 230, 145, 310 },
		Button { enabled, "OK" };

		{ 190-10-20-5, 320-10-80-5, 190-10+5, 320-10+5 }, /* box for border */
		UserItem { enabled };

		{ 10, 35, 28, 130 },
		StaticText { enabled, "Terminal type: " };

		{ 40, 10, 58, 130 },
		StaticText { enabled, "Background color: " };

		{ 70, 12, 88, 132 },
		StaticText { enabled, "Foreground color: " };

		{ 10, 150, 29, 300 },
		Control { disabled, CNTL_PREF_TERM_TYPE };

		{ 40, 150, 59, 300 },
		Control { enabled, CNTL_PREF_BG_COLOR };

		{ 70, 150, 89, 300 },
		Control { enabled, CNTL_PREF_FG_COLOR };

		{ 100, 66, 118, 132 },
		StaticText { enabled, "Font Size: " };

		{ 100, 150, 119, 300 },
		Control { enabled, CNTL_PREF_FONT_SIZE };

		{ 125, 10, 145, 90 },
		Button { enabled, "Cancel" };

		/*
		{ 115, 105, 135, 170 },
		Button { enabled, "Reset" };
		*/
	}
};

resource 'CNTL' (CNTL_PREF_FG_COLOR) {
	{ 0, 0, 19, 150 },
	popupTitleLeftJust,
	visible,
	0,
	MENU_COLOR,
	popupMenuProc,
	0,
	""
};

resource 'CNTL' (CNTL_PREF_BG_COLOR) {
	{ 0, 0, 19, 150 },
	popupTitleLeftJust,
	visible,
	0,
	MENU_COLOR,
	popupMenuProc,
	0,
	""
};

resource 'CNTL' (CNTL_PREF_TERM_TYPE) {
	{ 0, 0, 19, 150 },
	popupTitleLeftJust,
	visible,
	0,
	MENU_TERM_TYPE,
	popupMenuProc,
	0,
	""
};

resource 'CNTL' (CNTL_PREF_FONT_SIZE) {
	{ 0, 0, 19, 150 },
	popupTitleLeftJust,
	visible,
	0,
	MENU_FONT_SIZE,
	popupMenuProc,
	0,
	""
};
