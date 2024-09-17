;;; ../../.dotfiles/doom/lang/latex.el -*- lexical-binding: t; -*-

;;; ==================================== LaTeX / AucTeX ====================================

;; auctex options

(after! tex
  ;; Compilation settings
  (setq TeX-command-extra-options "-shell-escape")
  ;; TeX-engine-alist
  ;; '((xetex "XeTeX -shell escape"
  ;; "xetex -shell-escape"
  ;; "xelatex -shell-escape")
  (setq-default TeX-engine 'xetex))

(after! preview
  (set-default 'preview-scale-function 2.0)
  (setq preview-image-type 'dvipng
        TeX-fold-type-list '(env macro math)))



;;; NOTE: Disable math-preview since auctex preview isn't blurry anymore
;; math-preview options
;; (defun cosmic/custom-preview-marks (envs)
;;   (mapcan (lambda (env) (list (cons (concat "\\begin{" env "}") (concat "\\end{" env "}"))
;;                               (cons (concat "\\begin{" env "*}") (concat "\\end{" env "*}"))))
;;           envs))

;; (use-package! math-preview
;;   :hook tex-mode
;;   :commands (cosmic/math-preview)
;;   :custom
;;   (math-preview-scale 1)
;;   (math-preview-raise 0.5)
;;   (math-preview-margin '(5 . 20))
;;   (math-preview-marks (append (cosmic/custom-preview-marks '("equation" "gather" "align"))
;;                               '(("\\[" . "\\]")
;;                                 ("\\(" . "\\)")
;;                                 ("$$" . "$$")
;;                                 ("$" . "$"))))
;;   (math-preview-preprocess-functions '((lambda (s)
;;                                          (replace-regexp-in-string "&" "" s))))
;;   :config
;;   (defun cosmic/math-preview ()
;;     "deals with auctex folding before activates math-preview-all"
;;     (interactive)
;;     (->> (math-preview--find-gaps (point-min) (point-max))
;;       (--map (math-preview--search (car it) (cdr it)))
;;       (-flatten)
;;       (--map (progn
;;                (message "test %s %s" (car it) (cdr it))
;;                (TeX-fold-clearout-region (car it) (cdr it))
;;                (math-preview--submit (car it) (cdr it)
;;                                      (math-preview--strip-marks
;;                                       (buffer-substring (car it) (cdr it)))))))))
;;   ;; (math-preview-all)


;; (after! math-preview

;; (math-preview--overlays)
;; (LaTeX-mark-environment)
;; (TeX-fold-clearout-region)
;; (math-preview-all))
;; )


(map! :map LaTeX-mode-map
      :localleader
      ;; :desc "Inline Preview" "p" #'cosmic/math-preview
      :desc "Fold Entire Buffer" "n" #'TeX-fold-buffer
      :desc "Compile LaTeX" "c" #'TeX-command-run-all
      :desc "View PDF" "v" #'TeX-view)


;; Based on https://tecosaur.github.io/emacs-config/config.html
;; (add-hook 'latex-mode-hook #'TeX-latex-mode)

(use-package! cdlatex
  :hook tex-mode
  :custom
  ;; (cdlatex-math-symbol-prefix ?\;) ;; doesn't work at the moment :(
  (cdlatex-math-symbol-alist
   '(;; adding missing functions to 3rd level symbols
     (?_ ("\\downarrow" "" "\\inf"))
     (?2 ("^2" "\\sqrt{?}" ""))
     (?3 ("^3" "\\sqrt[3]{?}" ""))
     (?^ ("\\uparrow" "" "\\sup"))
     (?k ("\\kappa" "" "\\ker"))
     (?m ("\\mu" "" "\\lim"))
     (?c ("" "\\circ" "\\cos"))
     (?d ("\\delta" "\\partial" "\\dim"))
     (?D ("\\Delta" "\\nabla" "\\deg"))
     ;; no idea why \Phi isnt on 'F' in first place, \phi is on 'f'.
     (?F ("\\Phi"))
     ;; now just conveniance
     (?. ("\\cdot" "\\dots"))
     (?: ("\\vdots" "\\ddots"))
     (?* ("\\times" "\\star" "\\ast"))
     (?b ("\\beta" "\\binom{?}{}"))
     (?I ("\\implies" "\\Im"))))
  (cdlatex-math-modify-alist
   '(;;my own stuff
     (?B "\\mathbb" nil t nil nil)
     (?a "\\abs" nil t nil nil)))
  (cdlatex-command-alist
   '(
     ("xx" "Times" "\\times ?" cdlatex-position-cursor nil nil t)
     ("bin" "Binomial" "\\binom{?}{}" cdlatex-position-cursor nil nil t)
     ("sec" "Insert Section" "\\section{}?" cdlatex-position-cursor nil t nil)
     ("secu" "Insert Starred Section" "\\section*{}?" cdlatex-position-cursor nil t nil)
     ("ssu" "Insert Starred subsection" "\\subsection*{?}" cdlatex-position-cursor nil t nil))))

;; (setq 'LaTeX-verbatim-macros-with-braces-local (remove 'href 'LaTeX-verbatim-macros-with-braces-local))
;; NOTE: does this still work after moving into after! block?
(after! latex
  (delete "href" LaTeX-verbatim-macros-with-braces-local))

;; Visuals
(add-hook 'LaTeX-mode-hook #'mixed-pitch-mode)
;; Make symbols betterer
(setq TeX-fold-math-spec-list
      `(;; missing/better symbols
        ("≤" ("le"))
        ("≥" ("ge"))
        ("≠" ("ne"))
        ;; conviniance shorts -- these don't work nicely ATM
        ;; ("‹" ("left"))
        ;; ("›" ("right"))
        ;; private macros
        ("ℝ" ("RR"))
        ("ℕ" ("NN"))
        ("ℤ" ("ZZ"))
        ("ℚ" ("QQ"))
        ("ℂ" ("CC"))
        ("ℙ" ("PP"))
        ("ℍ" ("HH"))
        ("𝔼" ("EE"))
        ("𝑑" ("dd"))
        ;; known commands
        ("" ("phantom"))
        (,(lambda (num den) (if (and (TeX-string-single-token-p num) (TeX-string-single-token-p den)))
            (concat num "／" den)
            (concat "❪" num "／" den "❫"))) ("frac")
        (,(lambda (arg) (concat "√" (TeX-fold-parenthesize-as-neccesary arg))) ("sqrt"))
        (,(lambda (arg) (concat "⭡" (TeX-fold-parenthesize-as-neccesary arg))) ("vec"))
        ("‘{1}’" ("text"))
        ;; private commands
        ("|{1}|" ("abs"))
        ("‖{1}‖" ("norm"))
        ("⌊{1}⌋" ("floor"))
        ("⌈{1}⌉" ("ceil"))
        ("⌊{1}⌉" ("round"))
        ("𝑑{1}/𝑑{2}" ("odv"))
        ("∂{1}/∂{2}" ("pdv"))
        ;; brackets
        ("[[1]" ("left["))
        ("]" ("right]"))
        ;; fancification
        ("{1}" ("mathrm"))
        (,(lambda (word) (string-offset-roman-chars 119743 word)) ("mathbf"))
        (,(lambda (word) (string-offset-roman-chars 119951 word)) ("mathcal"))
        (,(lambda (word) (string-offset-roman-chars 120003 word)) ("mathfrak"))
        (,(lambda (word) (string-offset-roman-chars 120055 word)) ("mathbb"))
        (,(lambda (word) (string-offset-roman-chars 120159 word)) ("mathsf"))
        (,(lambda (word) (string-offset-roman-chars 120367 word)) ("mathtt")))
      TeX-fold-macro-spec-list
      '(
        ;; (1 ("href"))
        ;; as the defaults
        ("[f]" ("footnote" "marginpar"))
        ("[c]" ("cite"))
        ("[l]" ("label"))
        ("[r]" ("ref" "pageref" "eqref"))
        ("[i]" ("index" "glossary"))
        ("..." ("dots"))
        ("{1}" ("emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt"
                "textbf" "textsc" "textup"))
        ;; tweaked defaults
        ("©" ("copyright"))
        ("®" ("textregistered"))
        ("™" ("texttrademark"))
        ("[1]:||►" ("item"))
        ("❡❡ {1}" ("part" "part*"))
        ("❡ {1}" ("chapter" "chapter*"))
        ("§ {1}" ("section" "section*"))
        ;; (cosmic/test ("section" "section*"))
        ("§§ {1}" ("subsection" "subsection*"))
        ("§§§ {1}" ("subsubsection" "subsubsection*"))
        ("¶ {1}" ("paragraph" "paragraph*"))
        ("¶¶ {1}" ("subparagraph" "subparagraph*"))
        ;; extra
        ("｢{1}" ("begin"))
        ("{1}｣" ("end"))))
(defun cosmic/test (input)
  (progn
    (message input)
    (concat "test" " 2 " input)))


(defun string-offset-roman-chars (offset word)
  "Shift the codepoint of each charachter in WORD by OFFSET with an extra -6 shift if the letter is lowercase"
  (apply 'string
         (mapcar (lambda (c)
                   (string-offset-apply-roman-char-exceptions
                    (+ (if (>= c 97) (- c 6) c) offset)))
                 word)))
(defvar string-offset-roman-char-exceptions
  '(;; lowercase serif
    (119892 . 8462)                     ; ℎ
    ;; lowercase caligraphic
    (119994 . 8495)                     ; ℯ
    (119996 . 8458)                     ; ℊ
    (120004 . 8500)                     ; ℴ
    ;; caligraphic
    (119965 . 8492)                     ; ℬ
    (119968 . 8496)                     ; ℰ
    (119969 . 8497)                     ; ℱ
    (119971 . 8459)                     ; ℋ
    (119972 . 8464)                     ; ℐ
    (119975 . 8466)                     ; ℒ
    (119976 . 8499)                     ; ℳ
    (119981 . 8475)                     ; ℛ
    ;; fraktur
    (120070 . 8493)                     ; ℭ
    (120075 . 8460)                     ; ℌ
    (120076 . 8465)                     ; ℑ
    (120085 . 8476)                     ; ℜ
    (120092 . 8488)                     ; ℨ
    ;; blackboard
    (120122 . 8450)                     ; ℂ
    (120127 . 8461)                     ; ℍ
    (120133 . 8469)                     ; ℕ
    (120135 . 8473)                     ; ℙ
    (120136 . 8474)                     ; ℚ
    (120137 . 8477)                     ; ℝ
    (120145 . 8484))                     ; ℤ

  "An alist of deceptive codepoints, and then where the glyph actually resides.")
(defun string-offset-apply-roman-char-exceptions (char)
  "Sometimes the codepoint doesn't contain the char you expect.
Such special cases should be remapped to another value, as given in `string-offset-roman-char-exceptions'."
  (if (assoc char string-offset-roman-char-exceptions)
      (cdr (assoc char string-offset-roman-char-exceptions))
    char))
(defun TeX-fold-parenthesize-as-neccesary (tokens optional suppress-left suppress-right)
  "Add ❪ ❫ parenthesis as if multiple LaTeX tokens appear to be present"
  (if (TeX-string-single-token-p tokens) tokens
    (concat (if suppress-left "" "❪")
            tokens
            (if suppress-right "" "❫"))))
(defun TeX-string-single-token-p (teststring)
  "Return t if TESTSTRING appears to be a single token, nil otherwise"
  (if (string-match-p "^\\\\?\\w+$" teststring) t nil))


;; Making \( \) less visible
;; (defface unimportant-latex-face
;;   '((t
;;      :inherit font-lock-comment-face :family "Overpass" :weight light))
;;   "Face used to make \\(\\), \\[\\] less visible."
;;   :group 'LaTeX-math)
;; (font-lock-add-keywords
;;  'latex-mode
;;  `((,(rx (and "\\" (any "()[]"))) 0 'unimportant-latex-face prepend))
;;  'end)
;; (font-lock-add-keywords
;;  'latex-mode
;;  `((,"\\\\[[:word:]]+" 0 'font-lock-keyword-face prepend))
;;  'end)
