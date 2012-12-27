;;; lob-c.el

(defun lob/on-c-mode-common-hook ()
  (define-key c-mode-map (kbd "C-c C-c") 'compile)
  (define-key c-mode-base-map (kbd "C-<return>") 'c-indent-new-comment-line)
  (define-key c-mode-base-map (kbd "C-M-h") 'backward-kill-word))

(eval-after-load 'cc-mode
  '(progn
     (add-hook 'c-mode-common-hook 'google-set-c-style)
     (add-hook 'c-mode-common-hook 'google-make-newline-indent)
     (add-hook 'c-mode-common-hook 'lob/on-c-mode-common-hook)))

(provide 'lob-c)
