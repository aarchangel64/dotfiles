-- Pull in the wezterm API
local wezterm = require("wezterm")

-- This will hold the configuration.
local config = wezterm.config_builder()

config.color_scheme = "Horizon Dark (Gogh)"
local scheme = wezterm.color.get_builtin_schemes()[config.color_scheme]

-- wezterm.log_info(scheme)

-- Reference: https://horizontheme.netlify.app
local brights = scheme.brights
-- change bright grey / black color to be more legible
brights[1] = "#4B4C53"
-- purple
brights[6] = "#B180D7"

local ansi = scheme.ansi
-- purple
ansi[6] = "#B877DB"

local alt = "#2E303E"

config.colors = {
	ansi = ansi,
	brights = brights,
	scrollbar_thumb = alt,
	-- blue / cyan
	-- scrollbar_thumb = scheme.ansi[5],

	tab_bar = {
		active_tab = {
			fg_color = scheme.foreground,
			bg_color = scheme.background,
		},
		inactive_tab = {
			fg_color = "#6C6F93",
			bg_color = alt,
		},
	},
}

config.bold_brightens_ansi_colors = "No"

-- wezterm.log_info(scheme)
-- wezterm.log_info(config.colors)

config.font = wezterm.font("Iosevka Term Nerd Font")
config.font_size = 11.0

config.enable_scroll_bar = true
config.hide_tab_bar_if_only_one_tab = true

config.window_frame = {
	font = wezterm.font({ family = "Iosevka Aile", weight = "Bold" }),

	-- The size of the font in the tab bar.
	-- Default to 10.0 on Windows but 12.0 on other systems
	font_size = 10.0,

	-- The overall background color of the tab bar when
	-- the window is focused
	active_titlebar_bg = "#232530",

	-- The overall background color of the tab bar when
	-- the window is not focused
	inactive_titlebar_bg = "#232530",
}

config.window_padding = {
	left = 0,
	right = 0,
	top = 0,
	bottom = 0,
}

local act = wezterm.action
-- Note: you can run wezterm show-keys to show the effective key and mouse assignments.

config.mouse_bindings = {
	-- Disable copy on selection
	{
		event = { Up = { streak = 1, button = "Left" } },
		mods = "NONE",
		action = wezterm.action.OpenLinkAtMouseCursor,
	},
	-- Right click copy and paste
	{
		event = { Down = { streak = 1, button = "Right" } },
		action = wezterm.action_callback(function(window, pane)
			local has_selection = window:get_selection_text_for_pane(pane) ~= ""
			if has_selection then
				window:perform_action(act.CopyTo("ClipboardAndPrimarySelection"), pane)
				window:perform_action(act.ClearSelection, pane)
			else
				window:perform_action(act.PasteFrom("Clipboard"), pane)
			end
		end),
	},
}

-- Conditianal scrollbar
-- https://github.com/wez/wezterm/discussions/2590
wezterm.on("update-status", function(window, pane)
	local overrides = window:get_config_overrides() or {}
	local dimensions = pane:get_dimensions()

	overrides.enable_scroll_bar = dimensions.scrollback_rows > dimensions.viewport_rows
		and not pane:is_alt_screen_active()

	window:set_config_overrides(overrides)
end)

-- TODO: fix starship prompt resizing
--
-- wezterm.on('window-resized', function(window, pane)
-- end)

-- and finally, return the configuration to wezterm
return config
