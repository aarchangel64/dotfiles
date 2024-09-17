;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Needed for verilog verilator fix
(unpin! flycheck)
(unpin! tree-sitter-langs)

;; Use latest commit to get support for Astro
(unpin! web-mode)
(unpin! lsp-mode)

(package! aggressive-indent)
;; (package! math-preview)
(package! ialign)
(package! banner-comment)
(package! pkgbuild-mode)
(package! platformio-mode)
(package! verilog-mode :recipe (:host github :repo "veripool/verilog-mode"))
;; (package! sphinx-doc)

;; https://github.com/emacs-vs/ts-docstr/issues/1
(package! msgu :recipe (:host github :repo "jcs-elpa/msgu"))
(package! ts-docstr :recipe (:host github :repo "emacs-vs/ts-docstr"
                             :files (:defaults "langs/*.el")))

(package! elcord)
(package! nhexl-mode)
(package! prettier-js)
(package! clang-format+)
(package! elisp-autofmt)

(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
(package! tiny :recipe (:host github :repo "abo-abo/tiny"))

(package! markdown-xwidget :recipe (:host github :repo "Cosmic-Goat/markdown-xwidget" :files (:defaults "resources")))
;; (package! markdown-xwidget :recipe (:host github :repo "cfclrk/markdown-xwidget" :files (:defaults "resources")))


;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)
