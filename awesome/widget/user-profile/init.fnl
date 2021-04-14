(import-macros {: apply : widget} :utilities.macros)

(local spawn (require :awful.spawn))
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

(local uptime-updater-timer
       (gears.timer {:timeout 60
                     :autostart true
                     :call_now true
                     :callback sys.update-uptime}))

(let [user-pic
      (widget (wibox.widget.imagebox)
              {:forced_height (dpi 45)
               :forced_width (dpi 45)
               :image (.. widget-icon-dir :default.svg)
               :resize true
               :clip_shape #(gears.shape.rounded_rect $1 $2 $3 beautiful.groups_radius)})
      align-image
      (widget (wibox.layout.align.vertical) {:expand :none})]

  (align-image:set_middle user-pic)

  (let [user-name
        (widget (wibox.widget.textbox)
                {:font "Inter Regular 10"
                 :markup (. (sys.info) :user)
                 :align :left
                 :valign :center})
        distro-name
        (widget (wibox.widget.textbox)
                {:font "Inter Regular 10"
                 :markup (. (sys.info) :distro)
                 :align :left
                 :valign :center})
        kernel-version
        (widget (wibox.widget.textbox)
                {:font "Inter Regular 10"
                 :markup (. (sys.info) :kernel)
                 :align :left
                 :valign :center})
        uptime-time
        (widget (wibox.widget.textbox)
                {:font "Inter Regular 10"
                 :markup (. (sys.info) :uptime)
                 :align :left
                 :valign :center})

        vert-info
        (widget (wibox.layout.fixed.vertical) {})

        horizon-layout
        (widget (wibox.layout.fixed.horizontal) {:spacing (dpi 10)})
        ]

    (vert-info:add user-name)
    (vert-info:add distro-name)
    (vert-info:add kernel-version)
    (vert-info:add uptime-time)

    (horizon-layout:add align-image)
    (horizon-layout:add vert-info)

    (let [margin
          (widget (wibox.container.margin)
                  {:margins (dpi 10)
                   :widget horizon-layout})]

      (let [user-profile
            (widget (wibox.container.background)
                    {:bg beautiful.groups_bg
                     :shape #(gears.shape.rounded_rect $1 $2 $3 beautiful.groups_radius)
                     :forced_height (dpi 92)
                     :widget margin})]

        (fn update-profile-image []
          (spawn.easy_async_with_shell apps.utils.update_profile
                                       #(match (string.gsub $ "%\n" "")
                                          :default (tset user-pic :image (.. widget-icon-dir :default.svg))
                                          _ (tset user-pic :image $))))

        (update-profile-image)

        (user-profile:connect_signal "mouse::enter" ;; sys.update-uptime
                                     (fn test []
                                       (gears.debug.dump (sys.info))))
        user-profile))))

;; (local user-profile (doto (wibox.container.background)
;;                       (tset :bg beautiful.groups_bg)
;;                       (tset :shape
;;                             #(gears.shape.rounded_rect $1 $2 $3
;;                                                        beautiful.groups_radius))
;;                       (tset :forced_height (dpi 92))
;;                       (tset :widget _G.profile-imagebox)))
