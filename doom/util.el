;;; ../../.dotfiles/doom/util.el -*- lexical-binding: t; -*-

;;; =========================== Custom Utility Functions ==========================

(defun cosmic/sort-lines-by-length (reverse beg end)
  "Sort lines by length."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr reverse 'forward-line 'end-of-line nil nil
                   (lambda (l1 l2)
                     (apply #'< (mapcar (lambda (range) (- (cdr range) (car range)))
                                        (list l1 l2)))))))))


;; Open lookups in new window: https://github.com/hlissner/doom-emacs/issues/3397
;; (dolist (fn '(definition references documentation))
;;   (fset (intern (format "+lookup/%s-new-window" fn))
;;         (lambda (identifier &optional arg)
;;           "TODO"
;;           (interactive (list (doom-thing-at-point-or-region)
;;                              current-prefix-arg))
;;           (let ((pt (point)))
;;             (+evil-window-vsplit-a (get-buffer-window))
;;             (switch-to-buffer-other-window (current-buffer))
;;             (goto-char pt)
;;             (funcall (intern (format "+lookup/%s" fn)) identifier arg)))))

;; (map! :n "K" #'+lookup/documentation-new-window)
