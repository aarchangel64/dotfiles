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

;; TODO: Put this in a .cls LaTex class instead of here
(after! ox-latex
  (add-to-list 'org-latex-classes
               '("memoir" "
\\documentclass{memoir}[12pt,oneside,openany,a4paper,notitlepage]
\\usepackage[utf8]{inputenc} %
\\usepackage[T1]{fontenc}    %
\\usepackage[final]{microtype} % Less badboxes
\\usepackage{titlesec}
\\usepackage[bottom=2cm,top=3cm,left=2cm,right=2cm]{geometry}
\\titleformat{\\chapter}{}{}{0em}{\\bfseries\\Huge}
\\titleformat{\\subsection}{}{}{0em}{\\bfseries\\large}
\\setcounter{secnumdepth}{0}
\\setcounter{tocdepth}{3}

\\newlength{\\drop}
\\newcommand*{\\titleGM}[4]{
\\drop=0.1\\textheight
\\hbox{%
\\hspace*{0.05\\textwidth}%
\\rule{1pt}{\\textheight}
\\hspace*{0.05\\textwidth}%
\\parbox[b]{0.95\\textwidth}{
\\vbox{%
\\vspace{\\drop}
{\\noindent\\HUGE\\bfseries #1 \\\\[0.5\\baselineskip]
}\\\\[2\\baselineskip]
{\\Large\\itshape #2}\\\\[4\\baselineskip]
{\\Large #3}\\par
\\vspace{0.5\\textheight}
{\\noindent #4}\\\\[\\baselineskip]
}% end of vbox
}% end of parbox
}% end of hbox
}
"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (setq org-latex-title-command "\\titleGM{%t}{%s}{%a}{%D} \\thispagestyle{empty} \\clearpage"))


;;; ==================================== Keymap ===================================

(after! org
  (setq evil-org-movement-bindings '((up . "<up>") (down . "<down>") (left . "<left>") (right . "<right>")))
  (setq org-todo-keywords
        '((sequence "TODO(!t/)" "IN PROGRESS(!p/)" "FINISHING UP(f)" "|" "DONE(!d/)")))
  ;; (setq org-todo-keyword-faces
  ;;       '(("IN PROGRESS" . (:foreground "yellow")))
  ;; )
  )
