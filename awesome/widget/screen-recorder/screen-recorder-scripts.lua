local awful = require("awful")
local naughty = require("naughty")
local user_config = require("widget.screen-recorder.screen-recorder-config")
local scripts_tbl = {}
local ffmpeg_pid = nil

-- Get user settings
scripts_tbl.user_resolution = user_config.user_resolution
scripts_tbl.user_offset = user_config.user_offset
scripts_tbl.user_audio = user_config.user_audio
scripts_tbl.user_dir = user_config.user_save_directory
scripts_tbl.user_mic_lvl = user_config.user_mic_lvl
scripts_tbl.user_fps = user_config.user_fps

scripts_tbl.update_user_settings = function(res, offset, audio)
    scripts_tbl.user_resolution = res
    scripts_tbl.user_offset = offset
    scripts_tbl.user_audio = audio
end

scripts_tbl.check_settings = function()
    -- For debugging purpose only
    -- naughty.notification({
    -- 	message=scripts_tbl.user_resolution .. ' ' .. scripts_tbl.user_offset .. tostring(scripts_tbl.user_audio)
    -- })
end

local create_save_directory = function()
    local create_dir_cmd = [[dir="]] .. scripts_tbl.user_dir .. [[
"if [ ! -d "$dir" ]; then
	mkdir -p "$dir"
fi
	]]

    awful.spawn.easy_async_with_shell(
        create_dir_cmd,
        function(stdout)
        end
    )
end

create_save_directory()

local kill_existing_recording_ffmpeg = function()
    -- Let's killall ffmpeg instance first after awesome (re)-starts if there's any
    awful.spawn.easy_async_with_shell(
        "pgrep -P $(pgrep awesome) ffmpeg | xargs kill",
        function(stdout)
        end
    )
end

kill_existing_recording_ffmpeg()

local turn_on_the_mic = function()
    awful.spawn.easy_async_with_shell(
        [[
		amixer set Capture cap
		amixer set Capture ]] .. scripts_tbl.user_mic_lvl .. [[%
		]],
        function()
        end
    )
end

local ffmpeg_stop_recording = function()
    awful.spawn.easy_async_with_shell(
        "pgrep -P $(pgrep awesome) ffmpeg | xargs kill -2",
        function(stdout)
        end
    )
    -- awful.spawn.easy_async_with_shell(
    --     [[
    -- 	ps x | grep 'ffmpeg -video_size' | grep -v grep | awk '{print $1}' | xargs kill -2
    -- 	]],
    --     function(stdout)
    --     end
    -- )
end

local create_notification = function(file_dir)
    local open_video =
        naughty.action {
        name = "Open",
        icon_only = false
    }

    local delete_video =
        naughty.action {
        name = "Delete",
        icon_only = false
    }

    open_video:connect_signal(
        "invoked",
        function()
            awful.spawn("xdg-open " .. file_dir, false)
        end
    )

    delete_video:connect_signal(
        "invoked",
        function()
            awful.spawn("gio trash " .. file_dir, false)
        end
    )

    naughty.notification(
        {
            app_name = "Screen Recorder",
            timeout = 60,
            title = "<b>Recording Finished!</b>",
            message = "Recording can now be viewed.",
            actions = {open_video, delete_video}
        }
    )
end

local ffmpeg_start_recording = function(audio, window, filedir)
    local add_audio_str = ""
    if audio then
        turn_on_the_mic()
        add_audio_str = "-f pulse -ac 2 -i default"
    end

    local add_window_str = "-video_size " .. scripts_tbl.user_resolution .. " -i :0.0+" .. scripts_tbl.user_offset
    if window then
        -- From https://stackoverflow.com/questions/25396784/how-to-record-a-specific-window-using-ffmpeg
        add_window_str =
            [[$(xwininfo | gawk 'match($0, /-geometry ([0-9]+x[0-9]+).([0-9]+).([0-9]+)/, a) { print "-video_size " a[1] " -i +" a[2] "," a[3] }')]]
    end

    local filename = "'" .. filedir .. os.date("%Y-%m-%d (%a %H:%M:%S)") .. ".mp4'"
    naughty.notification({message = filename})

    local ffmpeg_command =
        table.concat(
        {
            "ffmpeg",
            "-vsync 1 -hwaccel cuda -hwaccel_output_format cuda ", -- cuda acceleration options
            "-threads:v 2 -thread_queue_size 512 -probesize 128M ", -- threading and probe options
            "-f x11grab -framerate 60 ", -- screen capture options
            add_window_str .. " ", -- capture size options
            add_audio_str .. " ", -- audio capture options
            "-c:v h264_nvenc -preset p7 -tune hq -profile:v high ", -- nvenc encoder options
            "-b:v 6M -bufsize 6M -maxrate 10M -qmin 0 -g 240 ", -- bitrate options
            "-bf 3 -b_ref_mode middle -temporal-aq 1 -rc-lookahead 20 -i_qfactor 0.75 -b_qfactor 1.1 ", -- encoder optimisation options
            filename
        }
    )

    naughty.notification({message = ffmpeg_command})

    ffmpeg_pid =
        awful.spawn.easy_async_with_shell(
        ffmpeg_command,
        function(stdout, stderr)
            if stderr and stderr:match("Invalid argument") then
                naughty.notification(
                    {
                        app_name = "Screen Recorder",
                        title = "<b>Invalid Configuration!</b>",
                        message = "Invalid settings!",
                        timeout = 60,
                        urgency = "normal"
                    }
                )
                awesome.emit_signal("widget::screen_recorder")
                return
            end
            create_notification(filename)
        end
    )
end

scripts_tbl.start_recording = function(audio, window)
    create_save_directory()
    local dir_command = "echo " .. scripts_tbl.user_dir
    awful.spawn.easy_async_with_shell(
        dir_command,
        function(stdout)
            ffmpeg_start_recording(audio, window, stdout)
        end
    )
end

scripts_tbl.stop_recording = function()
    ffmpeg_stop_recording()
end

return scripts_tbl
