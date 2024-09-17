;;; ../../.dotfiles/doom/lang/org.el -*- lexical-binding: t; -*-

;;; =================================== Org Mode ==================================

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/Org")


;; (add-to-list 'org-export-latex-classes
;;              '("book"
;;                "\\documentclass[10pt]{memoir}"
;;                ("\\chapter{%s}" . "\\chapter*{%s}")
;;                ("\\section{%s}" . "\\section*{%s}")
;;                ("\\subsection{%s}" . "\\subsection*{%s}")
;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
;;              )

(after! ox-latex
  ;; Packages
  (add-to-list 'org-latex-packages-alist '("" "tabularx"))

  ;; Classes

  (defun add-latex-class (name command)
    "Add NAME class to org-latex-classes with COMMAND"
    (add-to-list 'org-latex-classes
                 (list name command
                       '("\\chapter{%s}" . "\\chapter*{%s}")
                       '("\\section{%s}" . "\\section*{%s}")
                       '("\\subsection{%s}" . "\\subsection*{%s}")
                       '("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                       '("\\paragraph{%s}" . "\\paragraph*{%s}")
                       '("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

  (add-latex-class "book" "\\documentclass[10pt]{memoir}")
  (add-latex-class "scrartcl" "\\documentclass{scrartcl}")
  (add-latex-class "tufte-handout" "\\documentclass{tufte-handout}")
  (add-latex-class "studyguide" "\\documentclass{studyguide}"))



;;; ==================================== Keymap ===================================

(map! :mode org-mode
      :v "*" #'org-emphasize)

;;; ===================================== Babel ====================================
(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t) (julia . t) (gnuplot . t) (latex . t))))

;; (defun org-babel-edit-prep:python (babel-info)
;;   (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
;;   (lsp))

;; ;; Make sure rustic gets activated in the org-src block and add the original file's source code.
;; (defun org-babel-edit-prep:rust (babel-info)
;;   ;; This gets the second item in the "babel-info" list, which holds the code in the original src block
;;   (setq-local src-code (nth 1 babel-info))
;;   (setq-local buffer-file-name (expand-file-name (->> babel-info caddr (alist-get :tangle))))
;;   (setq-local buffer-src-code (replace-regexp-in-string src-code "" (my-read-file-to-string (buffer-file-name))))
;;   (goto-char (point-max))
;;   (insert buffer-src-code)
;;   (narrow-to-region (point-min) (+ (point-min) (length src-code)))
;;   (rustic-mode)
;;   (org-src-mode))

;; (defun my-delete-hidden-text ()
;;   "Remove all text that would be revealed by a call to `widen'"
;;   (-let [p-start (point-max)]
;;     (widen)
;;     (delete-region p-start (point-max))))

;; (define-advice org-edit-src-exit
;;     (:before (&rest _args) remove-src-block)
;;   (when (eq major-mode 'rustic-mode)
;;     (my-delete-hidden-text)))

;; (define-advice org-edit-src-save
;;     (:before (&rest _args) remove-src-block)
;;   (when (eq major-mode 'rustic-mode)
;;     (my-delete-hidden-text)))


;;; =================================== Misc. ===================================

(after! org
  (setq org-hide-emphasis-markers nil)
  (setq org-support-shift-select t)
  (setq evil-org-movement-bindings '((up . "<up>") (down . "<down>") (left . "<left>") (right . "<right>")))
  (setq org-todo-keywords
        '((sequence "TODO(!t/)" "IN PROGRESS(!p/)" "FINISHING UP(f)" "|" "DONE(!d/)")))
  (setq org-clock-clocktable-default-properties '(:maxlevel 5 :timestamp t)))



;; (setq org-todo-keyword-faces
;;       '(("IN PROGRESS" . (:foreground "yellow")))
;; )
