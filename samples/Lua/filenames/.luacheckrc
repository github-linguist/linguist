-- Disable unused self warnings.
self = false;

-- Allow unused arguments.
unused_args = false;

-- Limit line length to 78 characters.
max_line_length = 78;
max_code_line_length = 78;
max_string_line_length = 78;
max_comment_line_length = 78;

-- Ignore generated files.
exclude_files = {
	".release",
	"Exporter/casc",
	"Exporter/Config",
	"Exporter/Data",
	"Exporter/Templates",
	"LibRPMedia-*-1.0.lua",
	"Libs",
};

-- The following globals are only read/written in non-packaged releases.
globals = {
	"LibRPMedia_BrowserMixin",
	"LibRPMedia_BrowserTabMixin",
	"LibRPMedia_IconBrowserMixin",
	"LibRPMedia_IconContentMixin",
	"LibRPMedia_IconPreviewMixin",
	"LibRPMedia_MusicBrowserMixin",
	"LibRPMedia_MusicColumnDisplayMixin",
	"LibRPMedia_MusicItemRowMixin",
	"LibRPMedia_MusicScrollMixin",
	"LibRPMedia_PaginationBarMixin",
	"LibRPMedia_SearchOptionsDropDownMixin",
	"SLASH_LIBRPMEDIA_SLASHCMD1",
	"SlashCmdList",
	"UIPanelWindows",
};

read_globals = {
	"LibRPMedia_BrowserFrame",
};

-- Add exceptions for external libraries.
std = "lua51+libstub+wow+wowstd"

stds.libstub = {
	read_globals = {
		"LibStub",
	},
};

stds.wow = {
	read_globals = {
		"CallbackRegistryBaseMixin",
		"CallErrorHandler",
		"Clamp",
		"ColumnDisplayMixin",
		"CreateFramePool",
		"CreateFromMixins",
		"debugprofilestop",
		"debugstack",
		"FauxScrollFrame_GetOffset",
		"FauxScrollFrame_OnVerticalScroll",
		"FauxScrollFrame_SetOffset",
		"FauxScrollFrame_Update",
		"GameTooltip_AddInstructionLine",
		"GameTooltip_AddNormalLine",
		"GameTooltip_SetTitle",
		"GameTooltip",
		"GetMouseFocus",
		"GREEN_FONT_COLOR",
		"HideUIPanel",
		"Mixin",
		"PanelTemplates_ResizeTabsToFit",
		"PanelTemplates_SetNumTabs",
		"PanelTemplates_SetTab",
		"PlayMusic",
		"PlaySound",
		"SecondsToTime",
		"SetPortraitToTexture",
		"ShowUIPanel",
		"SOUNDKIT",
		"StopMusic",
		"tostringall",
		"UIDropDownMenu_AddButton",
		"UIDropDownMenu_CreateInfo",
		"UIDropDownMenu_Initialize",
		"UIParent",
		"WOW_PROJECT_CLASSIC",
		"WOW_PROJECT_ID",
		"WOW_PROJECT_MAINLINE",
		"WrapTextInColorCode",
	},
};

stds.wowstd = {
	read_globals = {
		string = {
			fields = {
				"join",
				"split",
			},
		},
		table = {
			fields = {
				"wipe",
			},
		},
	},
};
