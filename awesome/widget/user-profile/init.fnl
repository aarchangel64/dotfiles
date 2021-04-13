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
       (wibox.widget
        { 1 {
             :id :icon
             :forced_height (dpi 45)
             :forced_width (dpi 45)
             :image (.. widget-icon-dir :default.svg)
             :widget wibox.widget.imagebox
             :resize true
             :clip_shape (fn [cr width height] (gears.shape.rounded_rect cr width height beautiful.groups_radius))
             }
         :layout wibox.layout.align.horizontal
         }))

(local profile-name
       (wibox.widget
        {
         :font "Inter Regular 10"
         :markup (. (sys.info) :user)
         :align :left
         :valign :center
         :widget wibox.widget.textbox
         }))

(local distro-name
       (wibox.widget
        {
         :font "Inter Regular 10"
         :markup (. (sys.info) :distro)
         :align :left
         :valign :center
         :widget wibox.widget.textbox
         }))

(local kernel-version
       (wibox.widget
        {
         :font "Inter Regular 10"
         :markup (. (sys.info) :kernel)
         :align :left
         :valign :center
         :widget wibox.widget.textbox
         }))

(local uptime-time
       (wibox.widget
        {
         :font "Inter Regular 10"
         :markup (. (sys.info) :uptime)
         :align :left
         :valign :center
         :widget wibox.widget.textbox
         }))

(fn update-profile-image []
  (awful.spawn.easy_async_with_shell
   apps.utils.update_profile
   #(match (string.gsub $ "%\n" "")
      "default"    (profile-imagebox.icon:set_image (.. widget-icon-dir :default.svg))
      _            (profile-imagebox.icon:set_image $))))

(update-profile-image)

(local uptime-updater-timer
       (gears.timer {
                     :timeout 60
                     :autostart true
                     :call_now true
                     :callback sys.update-uptime
                     }))

;; (fn a [widget prop val]
;;   (set (. widget prop) val))

;; TODO: maybe use `each` with a table of (props, val) instead?
(local user-profile
       (doto (wibox.container.background)
         (tset :bg beautiful.groups_bg)
         (tset :shape #(gears.shape.rounded_rect $1 $2 $3 beautiful.groups_radius))
         (tset :forced_height (dpi 92))
         (tset :widget profile-imagebox)
         ))

;; (wibox.widget
;;  { 1 {
;;       1 {
;;          :layout wibox.layout.fixed.horizontal
;;          :spacing (dpi 10)
;;          3 {
;;             :layout wibox.layout.align.vertical
;;             :expand :none
;;             3 nil
;;             4 profile-imagebox
;;             5 nil
;;             }
;;          4 {
;;             :layout wibox.layout.align.vertical
;;             :expand :none
;;             3 nil
;;             4 {
;;                :layout wibox.layout.fixed.vertical
;;                2 profile-name
;;                3 distro-name
;;                4 kernel-version
;;                5 uptime-time
;;                }
;;             5 nil
;;             }
;;          }
;;       :margins (dpi 10)
;;       :widget wibox.container.margin
;;       }
;;   :forced_height (dpi 92)
;;   :bg beautiful.groups_bg
;;   :shape #(gears.shape.rounded_rect $1 $2 $3 beautiful.groups_radius)
;;   :widget wibox.container.background
;;   }))

(user-profile:connect_signal "mouse::enter" ;; sys.update-uptime
                             (fn test []
                               (gears.debug.dump (sys.info))))

user-profile
