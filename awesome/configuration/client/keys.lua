local awful = require("awful")
local gears = require("gears")
local dpi = require("beautiful").xresources.apply_dpi
require("awful.autofocus")
local modkey = require("configuration.keys.mod").mod_key
local altkey = require("configuration.keys.mod").alt_key

-- Helper functions for sane(er) keyboard resizing in layout.suit.tile.* modes
local function resize_horizontal(factor)
    local layout = awful.layout.get(awful.screen.focused())
    if layout == awful.layout.suit.tile then
        awful.tag.incmwfact(-factor)
    elseif layout == awful.layout.suit.tile.left then
        awful.tag.incmwfact(factor)
    elseif layout == awful.layout.suit.tile.top then
        awful.client.incwfact(-factor)
    elseif layout == awful.layout.suit.tile.bottom then
        awful.client.incwfact(-factor)
    end
end

local function resize_vertical(factor)
    local layout = awful.layout.get(awful.screen.focused())
    if layout == awful.layout.suit.tile then
        awful.client.incwfact(-factor)
    elseif layout == awful.layout.suit.tile.left then
        awful.client.incwfact(-factor)
    elseif layout == awful.layout.suit.tile.top then
        awful.tag.incmwfact(-factor)
    elseif layout == awful.layout.suit.tile.bottom then
        awful.tag.incmwfact(factor)
    end
end

local client_keys =
    awful.util.table.join(
    --
    -- Window Resizing
    awful.key(
        {modkey},
        "F11",
        function(c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}
    ),
    --
    -- Layout-aware resizing
    awful.key(
        {modkey},
        "Shift",
        "h",
        function()
            resize_horizontal(0.05)
        end,
        {group = "layout", description = "increase master width factor"}
    ),
    awful.key(
        {modkey, "Shift"},
        "l",
        function()
            resize_horizontal(-0.05)
        end,
        {group = "layout", description = "decrease master width factor"}
    ),
    awful.key(
        {altkey, "Shift"},
        "k",
        function()
            resize_vertical(-0.05)
        end,
        {group = "layout", description = "increase master width factor"}
    ),
    awful.key(
        {altkey, "Shift"},
        "j",
        function()
            resize_vertical(0.05)
        end,
        {group = "layout", description = "decrease master width factor"}
    ),
    --
    --
    --
    awful.key(
        {modkey},
        "F4",
        function(c)
            c:kill()
        end,
        {description = "close", group = "client"}
    ),
    awful.key(
        {modkey},
        "n",
        function()
            awful.client.swap.byidx(1)
        end,
        {description = "swap with next client by index", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "N",
        function()
            awful.client.swap.byidx(-1)
        end,
        {description = "swap with next client by index", group = "client"}
    ),
    awful.key({modkey}, "u", awful.client.urgent.jumpto, {description = "jump to urgent client", group = "client"}),
    --
    --
    awful.key(
        {modkey},
        "Tab",
        function()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}
    ),
    awful.key(
        {modkey},
        "Left",
        function()
            awful.client.focus.global_bydirection("left")
        end,
        {description = "Focus Left", group = "client"}
    ),
    awful.key(
        {modkey},
        "Right",
        function()
            awful.client.focus.global_bydirection("right")
        end,
        {description = "Focus Right", group = "client"}
    ),
     awful.key(
        {modkey},
        "Down",
        function()
            awful.client.focus.global_bydirection("down")
        end,
        {description = "Focus Down", group = "client"}
    ),
      awful.key(
        {modkey},
        "Up",
        function()
            awful.client.focus.global_bydirection("up")
        end,
        {description = "Focus Up", group = "client"}
    ),
 



    awful.key(
        {modkey},
        "m",
        function(c)
            c.minimized = true
        end,
        {description = "minimize client", group = "client"}
    )
)

return client_keys
