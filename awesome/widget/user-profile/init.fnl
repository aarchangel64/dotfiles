(local awful (require :awful))
(local wibox (require :wibox))
(local gears (require :gears))
(local naughty (require :naughty))
(local beautiful (require :beautiful))
(local dpi beautiful.xresources.apply_dpi)
(local apps (require :configuration.apps))
(local clickable-container (require :widget.clickable-container))
(local config-dir (gears.filesystem.get_configuration_dir))
(local widget-icon-dir (.. config-dir :configuration/user-profile/))
(local user-icon-dir :/var/lib/AccountsService/icons/)
(local sys (require :utilities.system-info))

(local profile-imagebox
       (wibox.widget {1 {:id :icon
                         :forced_height (dpi 45)
                         :forced_width (dpi 45)
                         :image (.. widget-icon-dir :default.svg)
                         :widget wibox.widget.imagebox
                         :resize true
                         :clip_shape (fn [cr width height]
                                       (gears.shape.rounded_rect cr width height beautiful.groups_radius))}
                      :layout wibox.layout.align.horizontal}))

(local profile-name
       (wibox.widget {:font "Inter Regular 10"
                      :markup sys.info.user
                      :align :left
                      :valign :center
                      :widget wibox.widget.textbox}))

(local distro-name
       (wibox.widget {:font "Inter Regular 10"
                      :markup sys.info.distro
                      :align :left
                      :valign :center
                      :widget wibox.widget.textbox}))

(local kernel-version
       (wibox.widget {:font "Inter Regular 10"
                      :markup sys.info.kernel
                      :align :left
                      :valign :center
                      :widget wibox.widget.textbox}))

(local uptime-time
       (wibox.widget {:font "Inter Regular 10"
                      :markup sys.info.uptime
                      :align :left
                      :valign :center
                      :widget wibox.widget.textbox}))

(fn update-profile-image []
  (awful.spawn.easy_async_with_shell apps.utils.update_profile
                                     (fn [stdout]
                                       (set-forcibly! stdout
                                                      (stdout:gsub "%\n" ""))
                                       (if (not (stdout:match :default))
                                           (profile-imagebox.icon:set_image stdout)
                                           (profile-imagebox.icon:set_image (.. widget-icon-dir
                                                                                :default.svg))))))

(update-profile-image)

(local uptime-updater-timer
       (gears.timer {:timeout 60
                     :autostart true
                     :call_now true
                     :callback sys.update-uptime}))

(local user-profile
       (wibox.widget {1 {1 {:layout wibox.layout.fixed.horizontal
                            :spacing (dpi 10)
                            3 {:layout wibox.layout.align.vertical
                               :expand :none
                               3 nil
                               4 profile-imagebox
                               5 nil}
                            4 {:layout wibox.layout.align.vertical
                               :expand :none
                               3 nil
                               4 {:layout wibox.layout.fixed.vertical
                                  2 profile-name
                                  3 distro-name
                                  4 kernel-version
                                  5 uptime-time}
                               5 nil}}
                         :margins (dpi 10)
                         :widget wibox.container.margin}
                      :forced_height (dpi 92)
                      :bg beautiful.groups_bg
                      :shape (fn {1 cr 2 width 3 height}
                               (gears.shape.rounded_rect cr width height
                                                         beautiful.groups_radius))
                      :widget wibox.container.background}))

(user-profile:connect_signal "mouse::enter" sys.update-uptime)

user-profile
