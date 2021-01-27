local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
local wibox = require("wibox")
local dpi = beautiful.xresources.apply_dpi

awful.titlebar.enable_tooltip = true
awful.titlebar.fallback_name = "Client"

local margin_size = dpi(6)
local spacing_size = dpi(7)
local default_pos = "right"

local double_click_event_handler = function(double_click_event)
    if double_click_timer then
        double_click_timer:stop()
        double_click_timer = nil
        double_click_event()
        return
    end
    double_click_timer =
        gears.timer.start_new(
        0.20,
        function()
            double_click_timer = nil
            return false
        end
    )
end

local create_click_events = function(c)
    -- Titlebar button/click events
    local buttons =
        gears.table.join(
        awful.button(
            {},
            1,
            function()
                double_click_event_handler(
                    function()
                        if c.floating then
                            c.floating = false
                            return
                        end
                        c.maximized = not c.maximized
                        c:raise()
                        return
                    end
                )
                c:activate {context = "titlebar", action = "mouse_move"}
            end
        ),
        awful.button(
            {},
            3,
            function()
                c:activate {context = "titlebar", action = "mouse_resize"}
            end
        )
    )
    return buttons
end

local function win_control_buttons(c, orientation)
    return {
        -- Min/Max/Fullscreen Buttons
        {
            awful.titlebar.widget.closebutton(c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.minimizebutton(c),
            spacing = spacing_size,
            layout = wibox.layout.fixed[orientation]
        },
        margins = margin_size,
        widget = wibox.container.margin
    }
end

local function centre_region(c, orientation, pos)
    local dir = {left = "east", right = "west", top = "north", bottom = "north"}
    return {
        -- Title
        {
            {
                align = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            direction = dir[pos],
            widget = wibox.container.rotate
        },
        -- Drag Events
        buttons = create_click_events(c),
        layout = wibox.layout.flex[orientation]
    }
end

local function win_float_buttons(c, orientation)
    return {
        -- Floating & Pin Window Buttons
        {
            awful.titlebar.widget.ontopbutton(c),
            awful.titlebar.widget.floatingbutton(c),
            spacing = spacing_size,
            layout = wibox.layout.fixed[orientation]
        },
        margins = margin_size,
        widget = wibox.container.margin
    }
end

local function create_titlebar(c, bg, pos)
    pos = pos or default_pos
    local orientation = ((pos == "left" or pos == "right") and "vertical") or "horizontal"
    local size = beautiful.titlebar_size

    awful.titlebar(c, {position = pos, bg = bg, size = size}):setup {
        win_control_buttons(c, orientation),
        centre_region(c, orientation, pos),
        win_float_buttons(c, orientation),
        layout = wibox.layout.align[orientation]
    }
end

-- Function for different dialog titlebars, didn't really like it though
--
-- local function create_dialog_titlebar(c, bg, pos)
--     pos = pos or "top"
--     local orientation = ((pos == "left" or pos == "right") and "vertical") or "horizontal"
--     local size = beautiful.titlebar_size

--     awful.titlebar(c, {position = pos, bg = bg, size = size}):setup {
--         nil,
--         centre_region(c, orientation, pos),
--         win_control_buttons(c, orientation),
--         layout = wibox.layout.align[orientation]
--     }
-- end

client.connect_signal(
    "request::titlebars",
    function(c)
        -- Customize here
        if c.class == "kitty" then
            create_titlebar(c, "#00000099")
        else
            create_titlebar(c, beautiful.background)
        end
    end
)
