local spawn = require("awful.spawn")
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
local uptime_updater_timer = gears.timer({autostart = true, call_now = true, callback = sys["update-uptime"], timeout = 60})
local user_pic
do
  local _0_0 = wibox.widget.imagebox()
  _0_0["resize"] = true
  _0_0["image"] = (widget_icon_dir .. "default.svg")
  _0_0["forced_width"] = dpi(45)
  local function _1_(_241, _242, _243)
    return gears.shape.rounded_rect(_241, _242, _243, beautiful.groups_radius)
  end
  _0_0["clip_shape"] = _1_
  _0_0["forced_height"] = dpi(45)
  user_pic = _0_0
end
local align_image
do
  local _1_0 = wibox.layout.align.vertical()
  _1_0["expand"] = "none"
  align_image = _1_0
end
align_image:set_middle(user_pic)
local user_name
do
  local _2_0 = wibox.widget.textbox()
  _2_0["font"] = "Inter Regular 10"
  _2_0["align"] = "left"
  _2_0["markup"] = sys.info().user
  _2_0["valign"] = "center"
  user_name = _2_0
end
local distro_name
do
  local _3_0 = wibox.widget.textbox()
  _3_0["font"] = "Inter Regular 10"
  _3_0["align"] = "left"
  _3_0["markup"] = sys.info().distro
  _3_0["valign"] = "center"
  distro_name = _3_0
end
local kernel_version
do
  local _4_0 = wibox.widget.textbox()
  _4_0["font"] = "Inter Regular 10"
  _4_0["align"] = "left"
  _4_0["markup"] = sys.info().kernel
  _4_0["valign"] = "center"
  kernel_version = _4_0
end
local uptime_time
do
  local _5_0 = wibox.widget.textbox()
  _5_0["font"] = "Inter Regular 10"
  _5_0["align"] = "left"
  _5_0["markup"] = sys.info().uptime
  _5_0["valign"] = "center"
  uptime_time = _5_0
end
local vert_info
do
  local _6_0 = wibox.layout.fixed.vertical()
  vert_info = _6_0
end
local horizon_layout
do
  local _7_0 = wibox.layout.fixed.horizontal()
  _7_0["spacing"] = dpi(10)
  horizon_layout = _7_0
end
vert_info:add(user_name)
vert_info:add(distro_name)
vert_info:add(kernel_version)
vert_info:add(uptime_time)
horizon_layout:add(align_image)
horizon_layout:add(vert_info)
local margin
do
  local _8_0 = wibox.container.margin()
  _8_0["widget"] = horizon_layout
  _8_0["margins"] = dpi(10)
  margin = _8_0
end
local user_profile
do
  local _9_0 = wibox.container.background()
  _9_0["widget"] = margin
  _9_0["bg"] = beautiful.groups_bg
  local function _10_(_241, _242, _243)
    return gears.shape.rounded_rect(_241, _242, _243, beautiful.groups_radius)
  end
  _9_0["shape"] = _10_
  _9_0["forced_height"] = dpi(92)
  user_profile = _9_0
end
local function update_profile_image()
  local function _10_(_241)
    local _11_0 = string.gsub(_241, "%\n", "")
    if (_11_0 == "default") then
      user_pic["image"] = (widget_icon_dir .. "default.svg")
      return nil
    else
      local _ = _11_0
      user_pic["image"] = _241
      return nil
    end
  end
  return spawn.easy_async_with_shell(apps.utils.update_profile, _10_)
end
update_profile_image()
local function test()
  return gears.debug.dump(sys.info())
end
user_profile:connect_signal("mouse::enter", test)
return user_profile
