local filesystem = require("gears.filesystem")
local theme_dir = filesystem.get_configuration_dir() .. "/theme"

local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local gtk_variable = beautiful.gtk.get_theme_variables

local theme = {}

theme.icons = theme_dir .. "/icons/"
theme.font = "Inter Regular 10"
theme.font_bold = "Inter Bold 10"

-- Colorscheme
theme.system_black_dark = "#3D4C5F"
theme.system_black_light = "#56687E"

theme.system_red_dark = "#EE4F84"
theme.system_red_light = "#F48FB1"

theme.system_green_dark = "#53E2AE"
theme.system_green_light = "#A1EFD3"

theme.system_yellow_dark = "#F1FF52"
theme.system_yellow_light = "#F1FA8C"

theme.system_blue_dark = "#6498EF"
theme.system_blue_light = "#92B6F4"

theme.system_magenta_dark = "#985EFF"
theme.system_magenta_light = "#BD99FF"

theme.system_cyan_dark = "#24D1E7"
theme.system_cyan_light = "#87DFEB"

theme.system_white_dark = "#E5E5E5"
theme.system_white_light = "#F8F8F2"

-- Accent color
theme.accent = theme.system_red_dark

-- Background color
theme.background = "#000000" .. "66"

-- Transparent
theme.transparent = "#00000000"

-- Awesome icon
theme.awesome_icon = theme.icons .. "awesome.svg"

local awesome_overrides = function(theme)
    -- Titlebar
    theme.titlebar_size = dpi(26)

    -- Client Decorations

    -- Borders
    -- theme.border_focus = "#fc9a9a" .. "36"
    -- theme.border_normal = "#1c1c1c" .. ""
    -- theme.border_marked = "#CC9393"
    -- theme.border_width = dpi(1)
    -- theme.border_radius = dpi(9)

    -- Decorations
    theme.useless_gap = dpi(3)
end

return {
    theme = theme,
    awesome_overrides = awesome_overrides
}
