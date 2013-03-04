;;; lob-defuns.el

(eval-when-compile (require 'paredit))
(eval-when-compile (require 'company))

(defmacro lob/nevar-fail (primary failover)
  "Runs primary code.  If primary code fails, then executes
  failover code."
  `(condition-case exc
       ,primary
     ('error
      (message (format "Caught exception: [%s]" exc))
      ,failover)))

(defun lob/sudo (&optional path)
  (interactive)
  (find-alternate-file
   (concat "/sudo:root@localhost:" (or path buffer-file-name))))

(defun lob/unfill-paragraph ()
  "The opposite of fill-paragraph. Takes a multi-line paragraph
and makes it into a single line of text.  Thanks: Stefan Monnier
<foo at acm.org>"
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun lob/regen-autoloads (&optional force)
  "Regenerate the autoload definitions file if necessary and load it."
  (interactive "P")
  (when (or force (not (file-exists-p autoload-file)))
    (message "updating autoloads...")
    (let ((generated-autoload-file autoload-file))
      (eval (cons 'update-directory-autoloads lob/vendor-dirs))))
  (load autoload-file))

(defun lob/reload ()
  "Save the .emacs buffer if needed, then reload .emacs."
  (interactive)
  (let ((dot-emacs (concat dotfiles-dir "/init.el")))
    (and (get-file-buffer dot-emacs)
         (save-buffer (get-file-buffer dot-emacs)))
    (load-file dot-emacs))
  (message "Re-initialized!"))

(defun lob/recompile ()
  "Recompiles everything so emacs loads wicked fast"
  (interactive)
  (lob/regen-autoloads t)
  (byte-recompile-directory dotfiles-dir 0)
  (byte-recompile-directory (concat dotfiles-dir "/vendor") 0))

(defun lob/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

(defun lob/recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun lob/run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'lob/coding-hook))

(defun lob/pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun lob/local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun lob/turn-on-paredit ()
  (paredit-mode t))

(defun lob/turn-on-company ()
  (company-mode t))

(defun lob/turn-off-tool-bar ()
  (tool-bar-mode -1))

(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(defun lob/face-at-point ()
  "Tells me who is responsible for ugly color under cursor"
  (interactive)
  (message "%S: %s" (face-at-point)
           (face-documentation (face-at-point))))

(defun lob/paredit-close-parenthesis ()
  "How do you expect me to rebalance my parens if you won't let
  me type omg!"
  (interactive)
  (lob/nevar-fail (paredit-close-parenthesis)
                  (insert ")")))

(defun lob/paredit-close-parenthesis-and-newline ()
  "How do you expect me to rebalance my parens if you won't let
  me type omg!"
  (interactive)
  (lob/nevar-fail (paredit-close-parenthesis-and-newline)
                  (insert ")")))

(defun lob/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

;; (defun lob/remove-from-list-and-sublists (remove-item list)
;;   (loop for item in list
;;         if (not (equal item remove-item))
;;         collect (if (listp item)
;;                     (lob/remove-from-list-and-sublists remove-item item)
;;                   item)))

(provide 'lob-defuns)
