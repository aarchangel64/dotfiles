local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
local wibox = require("wibox")
local dpi = beautiful.xresources.apply_dpi
awful.titlebar.enable_tooltip = true
awful.titlebar.fallback_name = "Client"

local margin_size = dpi(6)
local spacing_size = dpi(7)

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

local function create_titlebar(c, pos, bg, size, horizontal)
    -- if (size == nil) then size = beautiful.titlebar_size
     size = beautiful.titlebar_size

    -- Check if passed position is valid
    if (horizontal and pos ~= "top" and pos ~= "bottom") then
        pos = "left"
        bg = "#FF00FF"
    elseif (pos ~= "left" and pos ~= "right") then
        pos = "top"
        bg = "#FF00FF"
    end

    awful.titlebar(c, {position = pos, bg = bg, size = size}):setup {
        -- Top/Left Section
        {
            -- Window Control Buttons
            {
                awful.titlebar.widget.closebutton(c),
                awful.titlebar.widget.maximizedbutton(c),
                awful.titlebar.widget.minimizebutton(c),
                spacing = spacing_size
            },
            margins = margin_size,
            widget = wibox.container.margin
        },
        -- Centre Section
        {
            -- Title and Drag events
            -- {
            --     align = "center",
            --     widget = awful.titlebar.widget.titlewidget(c)
            -- },
            buttons = create_click_events(c),
            -- layout = (horizontal and wibox.layout.flex.horizontal) or wibox.layout.flex.vertical
            layout = wibox.layout.flex.vertical
        },
        -- Bottom/Right Section
        {
            -- Layout Control Buttons
            {
                awful.titlebar.widget.ontopbutton(c),
                awful.titlebar.widget.floatingbutton(c),
                spacing = spacing_size,
                -- layout = (horizontal and wibox.layout.fixed.horizontal) or wibox.layout.fixed.vertical
                layout = wibox.layout.fixed.vertical
            },
            margins = margin_size,
            widget = wibox.container.margin
        },
        -- layout = (horizontal and wibox.layout.align.horizontal) or wibox.layout.align.vertical
        layout = wibox.layout.align.vertical
    }
end

local create_horizontal_bar = function(c, pos, bg, size)
    -- Check if passed position is valid

    awful.titlebar(c, {position = pos, bg = bg, size = size}):setup {
        {
            {
                awful.titlebar.widget.closebutton(c),
                awful.titlebar.widget.maximizedbutton(c),
                awful.titlebar.widget.minimizebutton(c),
                spacing = spacing_size,
                layout = wibox.layout.fixed.horizontal
            },
            margins = margin_size,
            widget = wibox.container.margin
        },
        {
            buttons = create_click_events(c),
            layout = wibox.layout.flex.horizontal
        },
        {
            {
                awful.titlebar.widget.ontopbutton(c),
                awful.titlebar.widget.floatingbutton(c),
                spacing = spacing_size,
                layout = wibox.layout.fixed.horizontal
            },
            margins = margin_size,
            widget = wibox.container.margin
        },
        layout = wibox.layout.align.horizontal
    }
end

local create_titlebar_dialog = function(c, pos, bg, size)
    -- Check if passed position is valid
    if (pos == "top" or pos == "bottom") then
        pos = "left"
        bg = "#FF00FF"
    end

    awful.titlebar(c, {position = pos, bg = bg, size = size}):setup {
        {
            {
                awful.titlebar.widget.closebutton(c),
                awful.titlebar.widget.minimizebutton(c),
                awful.titlebar.widget.ontopbutton(c),
                spacing = spacing_size,
                layout = wibox.layout.fixed.vertical
            },
            margins = margin_size,
            widget = wibox.container.margin
        },
        {
            buttons = create_click_events(c),
            layout = wibox.layout.flex.vertical
        },
        nil,
        layout = wibox.layout.align.vertical
    }
end

local create_horizontal_bar_dialog = function(c, pos, bg, size)
    -- Check if passed position is valid
    if (pos == "left" or pos == "right") then
        pos = "top"
        bg = "#FF00FF"
    end

    awful.titlebar(c, {position = pos, bg = bg, size = size}):setup {
        {
            {
                awful.titlebar.widget.closebutton(c),
                awful.titlebar.widget.ontopbutton(c),
                awful.titlebar.widget.minimizebutton(c),
                spacing = spacing_size,
                layout = wibox.layout.fixed.horizontal
            },
            margins = margin_size,
            widget = wibox.container.margin
        },
        {
            buttons = create_click_events(c),
            layout = wibox.layout.flex.horizontal
        },
        nil,
        layout = wibox.layout.align.horizontal
    }
end

client.connect_signal(
    "request::titlebars",
    function(c)
        -- Customize here
        if c.type == "normal" then
            if c.class == "kitty" then
                create_titlebar(c, "left", "#00000099")
            elseif c.class == "firefox" then
                create_titlebar(c, "left", beautiful.background)
            elseif c.class == "XTerm" or c.class == "UXTerm" then
                create_horizontal_bar(c, "top", beautiful.xresources.get_current_theme().background)
            elseif c.class == "ark" or c.class == "dolphin" then
                create_titlebar(c, "left", "#00000099")
            elseif c.instance == "transmission-qt" then
                create_titlebar(c, "left", "#00000099")
            elseif c.class == "Gimp-2.10" or c.class == "Inkscape" then
                create_titlebar(c, "left", beautiful.gtk.get_theme_variables().bg_color)
            elseif c.class == "Com.github.johnfactotum.Foliate" then
                create_titlebar(c, "left", beautiful.gtk.get_theme_variables().bg_color)
            elseif c.class == "Arandr" then
                create_titlebar(c, "left", beautiful.gtk.get_theme_variables().bg_color)
            elseif c.class == "Ettercap" then
                create_titlebar(c, "left", beautiful.gtk.get_theme_variables().base_color)
            elseif c.class == "Google-chrome" or c.class == "Chromium" then
                create_titlebar(c, "left", beautiful.gtk.get_theme_variables().base_color)
            elseif c.class == "TelegramDesktop" then
                create_titlebar(c, "left", "#17212b")
            elseif c.class == "Kvantum Manager" then
                create_titlebar(c, "left", "#00000099")
            elseif c.class == "qt5ct" then
                create_titlebar(c, "left", "#00000099")
            elseif c.class == "Nemo" then
                create_horizontal_bar(c, "top", beautiful.gtk.get_theme_variables().base_color)
            else
                create_titlebar(c, "left", beautiful.background)
            end
        elseif c.type == "dialog" then
            if c.role == "GtkFileChooserDialog" then
                create_titlebar_dialog(c, "left", beautiful.gtk.get_theme_variables().bg_color)
            elseif c.class == "firefox" then
                create_titlebar_dialog(c, "left", beautiful.gtk.get_theme_variables().bg_color)
            elseif c.class == "Gimp-2.10" then
                create_titlebar_dialog(c, "left", beautiful.gtk.get_theme_variables().bg_color)
            elseif c.class == "Arandr" then
                create_titlebar(c, "left", beautiful.gtk.get_theme_variables().bg_color)
            else
                create_titlebar_dialog(c, "left", "#00000099")
            end
        elseif c.type == "modal" then
            create_titlebar(c, "left", "#00000099")
        else
            create_titlebar(c, "left", beautiful.background)
        end
    end
)
