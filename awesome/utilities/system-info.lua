local shell = (require("awful")).spawn.easy_async_with_shell
local function run_cmd(cmd, field)
  local function _0_(stdout)
    sys.info[field] = stdout
    return nil
  end
  return shell(cmd, _0_)
end
local function _0_()
  return run_cmd("uptime -p", "uptime")
end
sys = {["update-uptime"] = _0_, info = {distro = "GNU/Linux", host = "host", kernel = "Linux", uptime = "", user = "user"}}
run_cmd("whoami", "user")
run_cmd("uname -n", "host")
run_cmd("cat /etc/os-release | awk 'NR==2'| awk -F '\"' '{print $2}'", "distro")
run_cmd("uname -r", "kernel")
sys["update-uptime"]()
return sys
