;;; init.el

(when window-system
  (let ((myfont "DejaVu Sans Mono-7"))
    (set-frame-font myfont)
    (add-to-list 'default-frame-alist (cons 'font myfont))))

(global-set-key (kbd "C-v") 'pager-page-down)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-v") 'pager-page-up)
(global-set-key (kbd "M-Q") 'lob/unfill-paragraph)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "M-.") 'find-tag)
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-x f") 'lob/recentf-ido-find-file)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x t") 'transpose-lines)
(global-set-key (kbd "C-x C-t") 'other-window)
(global-set-key (kbd "C-x C-i") 'ido-imenu)
(global-set-key (kbd "C-x C-n") 'next-error)
(global-set-key (kbd "C-x C-p") 'previous-error)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x M-m") 'shell)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "C-c n") 'lob/cleanup-buffer)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-r") 'replace-string)
(global-set-key (kbd "C-x C-l") 'replace-regexp)
(global-set-key (kbd "C-x C-r") 'replace-string)
(global-set-key (kbd "C-x C-l") 'replace-regexp)
(global-set-key (kbd "<f1>") 'man)
(global-set-key (kbd "<f2>") 'lob/recompile)
(global-set-key (kbd "<f3>") 'lob/sudo)
(global-set-key (kbd "<f4>") 'lob/face-at-point)
(global-set-key (kbd "<f5>") 'toggle-truncate-lines)
(global-set-key (kbd "<f6>") 'gud-next)
(global-set-key (kbd "C-<f6>") 'gud-nexti)
(global-set-key (kbd "<f7>") 'gud-step)
(global-set-key (kbd "C-<f7>") 'gud-stepi)
(global-set-key (kbd "<f8>") 'gud-finish)
(global-set-key (kbd "C-<f8>") 'gud-cont)
(global-set-key (kbd "<f9>") 'gud-up)
(global-set-key (kbd "C-<f9>") 'gud-down)
(global-set-key (kbd "<f10>") 'compile)
(global-set-key (kbd "C-<f10>") 'gdb)
(global-set-key (kbd "C-x <f10>") 'jart/fix-gdb-gui)

(when window-system
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C-_") 'text-scale-decrease))

(define-key minibuffer-local-map (kbd "C-p") 'previous-history-element)
(define-key minibuffer-local-map (kbd "C-n") 'next-history-element)

(when (string-match "^jart\\|ows$" (getenv "USER"))
  (keyboard-translate ?\C-u ?\C-x)
  (keyboard-translate ?\C-x ?\C-u)
  ;; (global-set-key (kbd "C-u") ctl-x-map)
  ;; (global-set-key (kbd "C-x") 'universal-argument)
  (global-set-key (kbd "C-h") 'delete-backward-char)
  (global-set-key (kbd "M-h") 'backward-kill-word)
  (global-set-key (kbd "C-x C-h") 'help)
  (global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
  (global-set-key (kbd "C-x C-g") 'grep-find)
  (global-set-key (kbd "C-x b") 'ibuffer)
  (global-unset-key (kbd "C-/")))

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq lob/vendor-dirs (list dotfiles-dir
                            (concat dotfiles-dir "vendor")
                            (concat dotfiles-dir "vendor/magit")
                            (concat dotfiles-dir "vendor/yasnippet")
                            (concat dotfiles-dir "vendor/coffee-mode")))
(dolist (dir lob/vendor-dirs) (add-to-list 'load-path dir))
(add-to-list 'custom-theme-load-path (concat dotfiles-dir "themes"))

(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default
 indent-tabs-mode nil
 tab-width 2
 python-indent 2
 c-basic-offset 2
 c-file-style nil
 fill-column 79
 truncate-lines t
 save-place t
 css-indent-offset 2
 coffee-tab-width 2
 sh-basic-offset 2
 sh-indentation 2
 company-clang-modes '(c-mode c++-mode objc-mode)
 company-backends '(company-elisp company-nxml company-css company-eclim
                    company-semantic company-xcode company-oddmuse
                    company-files company-dabbrev
                    (company-gtags company-etags company-dabbrev-code
                     company-keywords)))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when window-system
  (mouse-wheel-mode t)
  (blink-cursor-mode -1)
  (add-hook 'before-make-frame-hook 'lob/turn-off-tool-bar))

(ido-mode t)
(setq ido-enable-prefix nil
      ido-ignore-extensions t
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)
(add-to-list 'completion-ignored-extensions ".d")  ;; "cc -MD" depends files

(setq frame-title-format '(buffer-file-name "%f - justinemacs" ("%b"))
      autoload-file (concat dotfiles-dir "loaddefs.el")
      custom-file (concat dotfiles-dir "custom.el")
      save-place-file (concat dotfiles-dir "places")
      transient-mark-mode t
      make-backup-files nil
      visible-bell nil
      gdb-many-windows t
      ring-bell-function 'ignore
      echo-keystrokes 0.1
      compilation-scroll-output 'first-error
      font-lock-maximum-decoration t
      inhibit-startup-message t
      column-number-mode t
      color-theme-is-global t
      shift-select-mode nil
      mouse-yank-at-point t
      diff-switches "-u"
      require-final-newline t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      lob/is-windows (not (null (memq system-type '(ms-dos windows-nt cygwin))))
      lob/is-unix (not (null (memq system-type
                                   '(gnu/linux darwin berkeley-unix cygwin))))
      lob/is-linux (not (null (memq system-type '(gnu/linux))))
      lob/is-mac (not (null (memq system-type '(darwin))))
      lob/is-colorful (>= (display-color-cells) 256))

(auto-fill-mode 1)
(setq comment-auto-fill-only-comments t)

(if (fboundp 'x-cut-buffer-or-selection-value)
    (setq x-select-enable-clipboard t
          interprogram-paste-function 'x-cut-buffer-or-selection-value))
(defalias 'yes-or-no-p 'y-or-n-p)
(random t)
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)

(show-paren-mode 1)
(recentf-mode 1)
(global-font-lock-mode t)
(auto-compression-mode t)
(delete-selection-mode 1)
(show-paren-mode 1)

;; Performance Improvement: I don't know who thought it was a good idea to ruin
;; the interactivity of this editor by doing a zillion disk seeks whenever I
;; try to open a file, just so it can put the current revision number in the
;; mode line.
(setq vc-handled-backends nil)

;; Performance Improvement: This is another not so great feature that makes
;; emacs slower by doing a zillion stat() calls every time I open a file.
(require 'files)
(defun dir-locals-find-file (file) nil)

;; Show me tabs, trailing whitespace, and when I overflow Fortran punchcards.
(require 'whitespace)
(setq whitespace-style '(face tabs tab-mark lines-tail trailing))
(setq whitespace-line-column 80)
(global-whitespace-mode 1)

(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

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

(lob/regen-autoloads)

(add-hook 'lob/coding-hook 'lob/pretty-lambdas)

(if window-system
    (load-theme 'zenburn t)
  (if (string= (getenv "TERM") "xterm-256color")
      (load-theme 'justine256 t)
    (progn
      (load-theme 'zenburn t)
      (warn "For much prettier colors run: TERM=xterm-256color emacs -nw"))))

(require 'yasnippet)
(yas-global-mode 1)
(setq yas/root-directory (list (concat dotfiles-dir "snippets")))

(setq rst-adornment-faces-alist (quote ((t . highlight-current-line)
                                        (t . font-lock-keyword-face)
                                        (1 . font-lock-keyword-face)
                                        (2 . font-lock-keyword-face)
                                        (3 . font-lock-keyword-face)
                                        (4 . font-lock-keyword-face)
                                        (5 . font-lock-keyword-face)
                                        (6 . font-lock-keyword-face))))

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.clj$" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))

(defun js-newline-and-indent ()
  (interactive)
  (if (= 0 (current-column))
      (newline)
    (if (eq font-lock-comment-face (face-at-point))
        (c-indent-new-comment-line)
      (newline-and-indent)))
  (end-of-line))

(let ((modes '((go-mode     go-mode-map          'newline-and-indent)
               (js          js-mode-map          'js-newline-and-indent)
               (python-mode python-mode-map      'newline-and-indent)
               (sh-script   sh-mode-map          'newline-and-indent)
               (yaml-mode   yaml-mode-map        'newline-and-indent)
               (lisp-mode   lisp-mode-shared-map 'reindent-then-newline-and-indent))))
  (while modes
    (eval-after-load (caar modes)
      `(progn
         (define-key ,(cadar modes) (kbd "<return>") ,(caddar modes))
         (define-key ,(cadar modes) (kbd "RET") ,(caddar modes))))
    (setq modes (cdr modes))))

(eval-after-load 'asm-mode
  '(progn
     (defun lob/asm-mode-hook ()
       (set (make-local-variable 'indent-tabs-mode) t)
       (set (make-local-variable 'tab-width) 8))
     (add-hook 'asm-mode-hook 'lob/asm-mode-hook)))

(eval-after-load 'make-mode
  '(progn
     (defun lob/makefile-mode-hook ()
       (define-key makefile-mode-map (kbd "C-c C-c") 'compile))
     (add-hook 'makefile-mode-hook 'lob/makefile-mode-hook)))

(eval-after-load 'compile
  '(progn
     (defun lob/compilation-mode-hook ()
       (setq truncate-lines nil))
     (add-hook 'compilation-mode-hook 'lob/compilation-mode-hook)))

(eval-after-load 'find-file
  '(progn
     (setq cc-search-directories
           (append cc-search-directories
                   (list "/usr/include/c++/*")))))

(eval-after-load 'calc
  '(progn
     (define-key calc-mode-map (kbd "C-x C-t") 'other-window)))

(eval-after-load 'cc-mode
  '(progn
     (defun lob/c-mode-common-hook ()
       (define-key c-mode-base-map (kbd "C-c C-c") 'compile)
       (define-key c-mode-base-map (kbd "C-c C-d") 'disaster)
       (define-key c-mode-base-map (kbd "C-c C-o") 'ff-find-other-file)
       (define-key c-mode-base-map (kbd "C-c C-h") 'includeme)
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


     (add-hook 'c-mode-common-hook 'google-set-c-style)
     (add-hook 'c-mode-common-hook 'google-make-newline-indent)
     (add-hook 'c-mode-common-hook 'lob/c-mode-common-hook)
     (add-hook 'c++-mode-hook 'lob/c++-mode-hook)))

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
     ;; These bindings make paredit easier to use.
     (define-key paredit-mode-map (kbd "C-<return>")
       'lob/paredit-close-parenthesis-and-newline)
     (define-key paredit-mode-map (kbd "C-c C-s") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "C-c C-b") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-c C-r") 'paredit-raise-sexp)
     (define-key paredit-mode-map (kbd "C-c C-l") 'paredit-split-sexp)
     (define-key paredit-mode-map (kbd "C-c C-j") 'paredit-join-sexps)
     (define-key paredit-mode-map (kbd "C-c C-i") 'paredit-reindent-defun)
     (define-key paredit-mode-map (kbd "C-c TAB") 'paredit-reindent-defun)
     (define-key paredit-mode-map (kbd "C-c C-w") 'paredit-wrap-round)
     (define-key paredit-mode-map (kbd "C-c C-p") 'paredit-splice-sexp)

     ;; These bindings make paredit feel less buggy.
     (define-key paredit-mode-map (kbd "C-d") 'paredit-forward-delete)
     (define-key paredit-mode-map (kbd "<DEL>") 'paredit-backward-delete)
     (define-key paredit-mode-map (kbd ")") 'lob/paredit-close-parenthesis)

     ;; Overload paredit with the stuff I overloaded in vanilla Emacs.
     (define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete)
     (define-key paredit-mode-map (kbd "C-M-h") 'paredit-backward-kill-word)))

(eval-after-load 'python
  '(progn
     (defun lob/python-check ()
       "Runs pyflakes and pep8 on current file"
       (interactive)
       (let ((path (file-name-nondirectory buffer-file-name)))
         (compile (format "pyflakes %s ; pep8 --repeat %s" path path))))

     (defun lob/python-check-dir ()
       "Same as `lob/python-check' but for all files in the
        current directory (as well as sub-directories.)"
       (interactive)
       (compile "pyflakes *.py ; pep8 --repeat *.py"))

     (defadvice python-calculate-indentation (around outdent-closing-brackets)
       "Handle lines beginning with a closing bracket and indent
        them so that they line up with the line containing the
        corresponding opening bracket."
       (save-excursion
         (beginning-of-line)
         (let ((syntax (syntax-ppss)))
           (if (and (not (eq 'string (syntax-ppss-context syntax)))
                    (python-continuation-line-p)
                    (cadr syntax)
                    (skip-syntax-forward "-")
                    (looking-at "\\s)"))
               (progn
                 (forward-char 1)
                 (ignore-errors (backward-sexp))
                 (setq ad-return-value (current-indentation)))
             ad-do-it))))

     (define-key python-mode-map (kbd "C-c c") 'lob/python-check)
     (define-key python-mode-map (kbd "C-c C") 'lob/python-check-dir)
     (define-key python-mode-map (kbd "C-c l") "lambda")
     (define-key python-mode-map (kbd "M-/") 'hippie-expand)
     (ad-activate 'python-calculate-indentation)
     (add-hook 'python-mode-hook 'lob/run-coding-hook)))

(require 'server)
(if (not (server-running-p))
    (server-start))

(when (string= (getenv "USER") "jart")
  (add-to-list 'load-path "/home/jart/includeme")
  (require 'includeme)
  (define-key c-mode-base-map (kbd "C-c C-h") 'includeme)

  (add-to-list 'load-path "/home/jart/disaster")
  (require 'disaster)
  (define-key c-mode-base-map (kbd "C-c C-d") 'disaster))
