(local shell (. (require :awful) :spawn :easy_async_with_shell))

(let [info {:user "whoami"
            :host "uname -n"
            :distro "cat /etc/os-release | awk 'NR==2'| awk -F '\"' '{print $2}'"
            :kernel "uname -r"
            :uptime "uptime -p"}]

  (fn run-cmd [cmd field]
    (shell cmd (fn [stdout]
                 (tset info field stdout))))

  (fn uptime []
    (run-cmd "uptime -p" :uptime))

  (each [field cmd (pairs info)]
    (run-cmd cmd field))


  ;; (run-cmd :whoami :user)
  ;; (run-cmd "uname -n" :host)
  ;; (run-cmd "cat /etc/os-release | awk 'NR==2'| awk -F '\"' '{print $2}'" :distro)
  ;; ;; (run-cmd "hostnamectl status --static" host)
  ;; (run-cmd "uname -r" :kernel)
  (uptime)
  {:info #info :update-uptime uptime})
