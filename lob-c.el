;;; lob-c.el

(defun lob/on-c-mode-common-hook ()
  (define-key c-mode-base-map (kbd "C-c C-c") 'compile)
  (define-key c-mode-base-map (kbd "C-<return>") 'c-indent-new-comment-line)
  (define-key c-mode-base-map (kbd "C-M-h") 'backward-kill-word))

(defun lob/c++-mode-hook ()
  (font-lock-add-keywords
   nil
   '(;; complete some fundamental keywords
     ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
     ;; add the new C++11 keywords
     ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
     ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
     ;; PREPROCESSOR_CONSTANT
     ("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
     ;; hexadecimal numbers
     ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
     ;; integer/float/scientific numbers
     ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
     ;; user-defined types (customizable)
     ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(type\\|ptr\\)\\>" . font-lock-type-face)
     ;; some explicit typenames (customizable)
     ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face))))

(eval-after-load 'cc-mode
  '(progn
     (add-hook 'c-mode-common-hook 'google-set-c-style)
     (add-hook 'c-mode-common-hook 'google-make-newline-indent)
     (add-hook 'c-mode-common-hook 'lob/on-c-mode-common-hook)
     (add-hook 'c++-mode-hook 'lob/c++-mode-hook)))

(provide 'lob-c)
