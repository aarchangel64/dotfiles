(let [awful (require :awful)
      gears (require :gears)
      wibox (require :wibox)
      beautiful (require :beautiful)
      dpi (beautiful.xresources.apply_dpi)]
  {record {
           :toggle_imgbox
           (wibox.widget {
                          :image    :.svg
                          :resize   true
                          :widget   wibox.container.margin
                          })
           :toggle_button
           (wibox.widget {
                          1 {
                             1          record.toggle_imgbox
                             :margins   (dpi 7)
                             :widget    wibox.container.margin
                             }
                          :widget clickable_container
                          })
           :countdown
           (wibox.widget {
                          :id   "countdown_text"
                          :font "Inter Bold 64"
                          :text "4"
                          })
           }
   }
  )
