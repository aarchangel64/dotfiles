(local shell (. (require :awful) :spawn :easy_async_with_shell))

(fn run-cmd [cmd field]
  (shell cmd (fn [stdout]
               (tset sys.info field stdout))))

(global sys {:update-uptime (fn [] (run-cmd "uptime -p" :uptime))
            :info {:user :user
                   :host :host
                   :distro :GNU/Linux
                   :kernel :Linux
                   :uptime ""}})


(run-cmd :whoami :user)
(run-cmd "uname -n" :host)
(run-cmd "cat /etc/os-release | awk 'NR==2'| awk -F '\"' '{print $2}'" :distro)
;; (run-cmd "hostnamectl status --static" host)
(run-cmd "uname -r" :kernel)

(sys.update-uptime)

sys
