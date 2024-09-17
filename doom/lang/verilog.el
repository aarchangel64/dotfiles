;;; ../../.dotfiles/doom/lang/pkgbuild.el -*- lexical-binding: t; -*-

;;; ============================= Verilog / SystemVerilog =============================

;; (lsp-clients-svlangserver-workspace-additional-dirs . ("/opt/intelFPGA/20.1/modelsim_ase")))

(add-hook! verilog-mode
           #'lsp-deferred)

;; After! / eval-after-load only runs once, when the package is loaded
(after! (verilog-mode flycheck)
  ;; (setq! flycheck-verilog-verilator-executable "verilator_bin")
  (setq! verilog-indent-level 2)
  (setq! verilog-indent-level-behavioral 2)
  (setq! verilog-indent-level-declaration 2)
  (setq verilog-linter "verilator --lint-only")
  ;; (setq verilog-coverage "coverage ...")
  ;; (setq verilog-simulator "verilator ... ")
  ;; (setq! verilog-compiler "verilator --lint-only")
  (setq! verilog-tool 'verilog-linter))


;; It no worky
;; (with-eval-after-load 'lsp-mode
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection "veridian")
;;                     :major-modes '(verilog-mode)
;;                     :server-id 'veridian
;;                     :priority 3)))
(after! lsp-mode
  (setq! lsp-clients-svlangserver-launchConfiguration "verilator -sv --timing --lint-only -Wall")
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "verible-verilog-ls")
                    :major-modes '(verilog-mode)
                    :server-id 'verible-ls
                    :priority -2)))
