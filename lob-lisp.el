;;; lob-lisp.el

(eval-after-load 'lisp-mode
  '(progn
     (define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
     (define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
     (define-key lisp-mode-shared-map (kbd "RET")
       'reindent-then-newline-and-indent)
     (define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)
     (define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)
     (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
     (add-hook 'emacs-lisp-mode-hook 'lob/turn-on-company)
     (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
     (add-hook 'emacs-lisp-mode-hook 'lob/remove-elc-on-save)
     (add-hook 'emacs-lisp-mode-hook 'lob/run-coding-hook)))

(eval-after-load 'paredit
  '(progn
     ;; these bindings make paredit easier to use
     (define-key paredit-mode-map (kbd "C-<return>")
       'lob/paredit-close-parenthesis-and-newline)
     (define-key paredit-mode-map (kbd "C-c C-s") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "C-c C-b") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-c C-r") 'paredit-raise-sexp)
     (define-key paredit-mode-map (kbd "C-c C-l") 'paredit-split-sexp)
     (define-key paredit-mode-map (kbd "C-c C-j") 'paredit-join-sexps)

     ;; these bindings make paredit feel less buggy
     (define-key paredit-mode-map (kbd "C-d") 'paredit-forward-delete)
     (define-key paredit-mode-map (kbd "<DEL>") 'paredit-backward-delete)
     (define-key paredit-mode-map (kbd "C-M-h") 'paredit-backward-kill-word)
     (define-key paredit-mode-map (kbd ")") 'lob/paredit-close-parenthesis)))

(provide 'lob-lisp)
