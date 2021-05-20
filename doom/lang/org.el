;;; ../../.dotfiles/doom/lang/org.el -*- lexical-binding: t; -*-

;;; =================================== Org Mode ==================================

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")


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
  (add-to-list 'org-latex-classes
               '("studyguide" "\\documentclass{studyguide}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


;;; ==================================== Keymap ===================================

(after! org
  (setq evil-org-movement-bindings '((up . "<up>") (down . "<down>") (left . "<left>") (right . "<right>")))
  (setq org-todo-keywords
        '((sequence "TODO(!t/)" "IN PROGRESS(!p/)" "FINISHING UP(f)" "|" "DONE(!d/)"))))
;; (setq org-todo-keyword-faces
;;       '(("IN PROGRESS" . (:foreground "yellow")))
;; )
