;;; lob-c.el

(eval-when-compile (require 'cc-vars))
(eval-when-compile (require 'cc-cmds))
(eval-when-compile (require 'cc-mode))

(defun lob/on-c-opening-brace ()
  "Extend electric brace mode to insert the closing brace.

We first run the normal `c-electric-brace' function which inserts
the opening curly brace and all sorts of other black magic.
After it's done we check if the current line changed (to be sure
we're not like in a comment or something) and then we insert the
closing brace.

If you've highlight some text, it'll take special care to make
sure the text you've highlighted goes inside the newly created
braces.

Hopefully you'll also have yank ident advice thing thing enabled
from lobstermacs-misc.el which will indent the pasted text."
  (interactive)
  (if (region-active-p)
      (progn
        (call-interactively 'kill-region)
        (if (string-match "^[ \t]*}" (lob/text-from-point-to-eol))
            (call-interactively 'open-line))
        (call-interactively '__lob/on-c-opening-brace)
        (call-interactively 'yank)
        (indent-according-to-mode))
    (call-interactively '__lob/on-c-opening-brace)))

(defun lob/text-from-point-to-eol (&optional point)
  (let ((pos (or point (point))))
    (buffer-substring-no-properties pos (save-excursion
                                          (goto-char pos)
                                          (line-end-position)))))

(defun __lob/on-c-opening-brace ()
  (interactive)
  (let ((curline (line-number-at-pos)))
    (call-interactively 'c-electric-brace)
    (unless (equal curline (line-number-at-pos))
      (save-excursion
        (call-interactively 'lob/on-c-mode-enter-key)
        (call-interactively 'lob/on-c-closing-brace))
      (call-interactively 'c-indent-line-or-region))))

(defun lob/on-c-closing-brace ()
  (interactive)
  (insert "}")
  (call-interactively 'c-indent-line-or-region))

(defun lob/on-c-mode-enter-key ()
  (interactive)
  ;; get current line under cursor
  (let ((line (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position))))
    ;; does it look like a multi-line comment?
    (if (string-match "^[ \t]*/?\\*" line)
        ;; then we should insert the star when we press enter
        (c-indent-new-comment-line)
      ;; otherwise do a normal indent
      (newline-and-indent))))

(defun lob/on-c-mode-common-hook ()
  "Sets lobstermacs default settings for curly-braced languages"
  (define-key c-mode-base-map (kbd "RET") 'lob/on-c-mode-enter-key)
  (define-key c-mode-base-map (kbd "<return>") 'lob/on-c-mode-enter-key)
  (define-key c-mode-map (kbd "RET") 'lob/on-c-mode-enter-key)
  (define-key c-mode-map (kbd "<return>") 'lob/on-c-mode-enter-key)
  (define-key c-mode-map (kbd "C-c C-c") 'compile)
  (define-key c-mode-base-map (kbd "C-<return>") 'c-indent-new-comment-line)
  (add-to-list 'auto-mode-alist '("\\.def\\'" . c-mode))
  (add-to-list 'auto-mode-alist '("\\.rl\\'" . c-mode))

  ;; goto next line when i press `{`
  (c-toggle-auto-newline 1)
  (setq-default c-electric-pound-behavior '(alignleft))
  (define-key c-mode-base-map (kbd "{") 'lob/on-c-opening-brace)
  (define-key c-mode-base-map (kbd "}") 'lob/on-c-closing-brace)
  (define-key c-mode-base-map (kbd ";") 'self-insert-command)
  (setq c-hanging-semi&comma-criteria
	'(c-semi&comma-no-newlines-before-nonblanks
	  c-semi&comma-inside-parenlist))

  (define-key c-mode-base-map (kbd "C-M-h") 'backward-kill-word))

(eval-after-load 'cc-mode
  '(progn
     (add-to-list 'c-mode-common-hook 'lob/on-c-mode-common-hook)))

(eval-after-load 'cc-styles
  '(progn
     (c-add-style "justine"
                  '("linux"
                    (indent-tabs-mode . nil)
                    (c-basic-offset . 4)))
     (setq-default c-default-style
                   (list (cons 'c-mode "justine")
                         (cons 'c++-mode "justine")
                         (cons 'java-mode "java")
                         (cons 'awk-mode "awk")
                         (cons 'other "justine")))))

(provide 'lob-c)
