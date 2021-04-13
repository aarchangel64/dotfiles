local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local naughty = require("naughty")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local apps = require("configuration.apps")
local clickable_container = require("widget.clickable-container")
local config_dir = gears.filesystem.get_configuration_dir()
local widget_icon_dir = (config_dir .. "configuration/user-profile/")
local user_icon_dir = "/var/lib/AccountsService/icons/"
local sys = require("utilities.system-info")
local profile_imagebox
local function _0_(cr, width, height)
  return gears.shape.rounded_rect(cr, width, height, beautiful.groups_radius)
end
profile_imagebox = wibox.widget({{clip_shape = _0_, forced_height = dpi(45), forced_width = dpi(45), id = "icon", image = (widget_icon_dir .. "default.svg"), resize = true, widget = wibox.widget.imagebox}, layout = wibox.layout.align.horizontal})
local profile_name = wibox.widget({align = "left", font = "Inter Regular 10", markup = sys.info().user, valign = "center", widget = wibox.widget.textbox})
local distro_name = wibox.widget({align = "left", font = "Inter Regular 10", markup = sys.info().distro, valign = "center", widget = wibox.widget.textbox})
local kernel_version = wibox.widget({align = "left", font = "Inter Regular 10", markup = sys.info().kernel, valign = "center", widget = wibox.widget.textbox})
local uptime_time = wibox.widget({align = "left", font = "Inter Regular 10", markup = sys.info().uptime, valign = "center", widget = wibox.widget.textbox})
local function update_profile_image()
  local function _1_(_241)
    local _2_0 = string.gsub(_241, "%\n", "")
    if (_2_0 == "default") then
      return (profile_imagebox.icon):set_image((widget_icon_dir .. "default.svg"))
    else
      local _ = _2_0
      return (profile_imagebox.icon):set_image(_241)
    end
  end
  return awful.spawn.easy_async_with_shell(apps.utils.update_profile, _1_)
end
update_profile_image()
local uptime_updater_timer = gears.timer({autostart = true, call_now = true, callback = sys["update-uptime"], timeout = 60})
local user_profile
do
  local _1_0 = wibox.container.background()
  _1_0["bg"] = beautiful.groups_bg
  local function _2_(_241, _242, _243)
    return gears.shape.rounded_rect(_241, _242, _243, beautiful.groups_radius)
  end
  _1_0["shape"] = _2_
  _1_0["forced_height"] = dpi(92)
  _1_0["widget"] = profile_imagebox
  user_profile = _1_0
end
local function test()
  return gears.debug.dump(sys.info())
end
user_profile:connect_signal("mouse::enter", test)
return user_profile
