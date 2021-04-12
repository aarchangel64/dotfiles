local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi()
return {[record] = {countdown = wibox.widget({font = "Inter Bold 64", id = "countdown_text", text = "4"}), toggle_button = wibox.widget({{record.toggle_imgbox, margins = dpi(7), widget = wibox.container.margin}, widget = clickable_container}), toggle_imgbox = wibox.widget({image = ".svg", resize = true, widget = wibox.container.margin})}}
