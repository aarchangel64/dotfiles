local shell = (require("awful")).spawn.easy_async_with_shell
local info = {distro = "cat /etc/os-release | awk 'NR==2'| awk -F '\"' '{print $2}'", host = "uname -n", kernel = "uname -r", uptime = "uptime -p", user = "whoami"}
local function run_cmd(cmd, field)
  local function _0_(stdout)
    info[field] = stdout
    return nil
  end
  return shell(cmd, _0_)
end
local function uptime()
  return run_cmd("uptime -p", "uptime")
end
for field, cmd in pairs(info) do
  run_cmd(cmd, field)
end
uptime()
local function _0_()
  return info
end
return {["update-uptime"] = uptime, info = _0_}
