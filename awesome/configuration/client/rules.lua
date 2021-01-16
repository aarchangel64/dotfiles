local awful = require("awful")
local gears = require("gears")
local ruled = require("ruled")
local beautiful = require("beautiful")
local client_keys = require("configuration.client.keys")
local client_buttons = require("configuration.client.buttons")

ruled.client.connect_signal(
    "request::rules",
    function()
        -- All clients will match this rule.
        ruled.client.append_rule {
            id = "global",
            rule = {},
            properties = {
                focus = awful.client.focus.filter,
                raise = false,
                floating = false,
                maximized = false,
                above = false,
                below = false,
                ontop = false,
                sticky = false,
                maximized_horizontal = false,
                maximized_vertical = false,
                keys = client_keys,
                buttons = client_buttons,
                screen = awful.screen.preferred,
                placement = awful.placement.no_overlap + awful.placement.no_offscreen
            }
        }

        -- Titlebar rules
        ruled.client.append_rule {
            id = "titlebars",
            rule_any = {
                type = {
                    "normal",
                    "dialog",
                    "modal",
                    "utility"
                }
            },
            properties = {
                titlebars_enabled = true
            }
        }

        -- Dialogs
        ruled.client.append_rule {
            id = "dialog",
            rule_any = {
                type = {"dialog"},
                class = {"Wicd-client.py", "calendar.google.com"}
            },
            properties = {
                titlebars_enabled = true,
                floating = true,
                above = true,
                skip_decoration = true,
                placement = awful.placement.centered
            }
        }

        -- Modals
        ruled.client.append_rule {
            id = "modal",
            rule_any = {
                type = {"modal"}
            },
            properties = {
                titlebars_enabled = true,
                floating = true,
                above = true,
                skip_decoration = true,
                placement = awful.placement.centered
            }
        }

        -- Utilities
        ruled.client.append_rule {
            id = "utility",
            rule_any = {
                type = {"utility"}
            },
            properties = {
                titlebars_enabled = false,
                floating = true,
                skip_decoration = true,
                placement = awful.placement.centered
            }
        }

        -- Splash
        ruled.client.append_rule {
            id = "splash",
            rule_any = {
                type = {"splash"},
                name = {"Discord Updater"}
            },
            properties = {
                titlebars_enabled = false,
                round_corners = false,
                floating = true,
                above = true,
                skip_decoration = true,
                placement = awful.placement.centered
            }
        }

        -- Terminal emulators
        ruled.client.append_rule {
            id = "terminals",
            rule_any = {
                class = {
                    "kitty"
                }
            },
            properties = {
                size_hints_honor = false,
                titlebars_enabled = true
            }
        }

        -- Gaming
        ruled.client.append_rule {
            id = "gaming",
            rule_any = {
                class = {
                    "Wine",
                    "dolphin-emu",
                    "Steam",
                    "Citra",
                    "supertuxkart"
                },
                name = {"Steam"}
            },
            properties = {
                tag = "6",
                switch_to_tags = true,
                skip_decoration = true
            }
        }

        -- IDEs and Tools
        ruled.client.append_rule {
            id = "development",
            rule_any = {
                class = {
                    "Unity",
                    "UnityHub",
                    "jetbrains-studio"
                }
            },
            properties = {
                skip_decoration = true
            }
        }

        -- Image viewers
        ruled.client.append_rule {
            id = "image_viewers",
            rule_any = {
                class = {
                    "feh",
                    "Pqiv",
                    "Sxiv"
                }
            },
            properties = {
                titlebars_enabled = true,
                skip_decoration = true,
                floating = true,
                ontop = true,
                placement = awful.placement.centered
            }
        }

        -- Floating
        ruled.client.append_rule {
            id = "floating",
            rule_any = {
                instance = {
                    "file_progress",
                    "Popup",
                    "nm-connection-editor"
                },
                class = {
                    "scrcpy",
                    "Pulseeffects"
                },
                role = {
                    "AlarmWindow",
                    "ConfigManager",
                    "pop-up"
                }
            },
            properties = {
                titlebars_enabled = true,
                skip_decoration = true,
                ontop = true,
                floating = true,
                focus = awful.client.focus.filter,
                raise = true,
                keys = client_keys,
                buttons = client_buttons,
                placement = awful.placement.centered
            }
        }
    end
)
