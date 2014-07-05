;;; init.el --- justinemacs (Justine Tunney's .emacs File)

;; Copyright (C) 2003-2014 Justine Alexandra Roberts Tunney
;; License: MIT
;; Author: Justine Tunney <jtunney@gmail.com>
;; Version: 0.1
;; URL: http://github.com/jart/justinemacs

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.

;;; Commentary:
;;
;;   I hope you enjoy my lovely emacs configuration <3

;;; Code:

(when window-system
  (let ((myfont "DejaVu Sans Mono-7"))
    (set-frame-font myfont)
    (add-to-list 'default-frame-alist (cons 'font myfont))))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-Q") 'jart-unfill-paragraph)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "M-.") 'find-tag)
(global-set-key (kbd "M-,") 'pop-tag-mark)
;; (global-set-key (kbd "M-{") 'jart-backward-paragraph)
;; (global-set-key (kbd "M-}") 'jart-forward-paragraph)
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "C-x f") 'jart-recentf-ido-find-file)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x t") 'transpose-lines)
(global-set-key (kbd "C-x C-_") 'jart-note)
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
(global-set-key (kbd "C-c n") 'jart-cleanup-buffer)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-r") 'replace-string)
(global-set-key (kbd "C-x C-l") 'replace-regexp)
(global-set-key (kbd "C-x C-r") 'replace-string)
(global-set-key (kbd "C-x C-l") 'replace-regexp)
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
(global-set-key (kbd "C-x v g") 'git-gutter:toggle)
(global-set-key (kbd "<f1>") 'man)
(global-set-key (kbd "<f3>") 'jart-sudo)
(global-set-key (kbd "<f4>") 'jart-face-at-point)
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
(global-set-key (kbd "C-x <f10>") 'jart-fix-gdb-gui)
(define-key minibuffer-local-map (kbd "C-p") 'previous-history-element)
(define-key minibuffer-local-map (kbd "C-n") 'next-history-element)

(when window-system
  (global-set-key (kbd "C-+") 'text-scale-increase)
  (global-set-key (kbd "C-_") 'text-scale-decrease))

(when (string-match "^jart\\|ows$" (getenv "USER"))
  (keyboard-translate ?\C-u ?\C-x)
  (keyboard-translate ?\C-x ?\C-u)
  (global-set-key (kbd "C-h") 'delete-backward-char)
  (global-set-key (kbd "M-h") 'backward-kill-word)
  (global-set-key (kbd "C-x C-h") 'help)
  (global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
  (global-set-key (kbd "C-x C-g") 'grep-find)
  (global-set-key (kbd "C-x b") 'ibuffer)
  (global-unset-key (kbd "C-/")))

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name))
      jart-vendor-dirs (list "~/go/src/github.com/nsf/gocode/emacs"
                             "~/go/src/github.com/dougm/goflymake"))
(dolist (dir jart-vendor-dirs) (add-to-list 'load-path dir))
(add-to-list 'custom-theme-load-path (concat dotfiles-dir "themes"))

(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (byte-recompile-directory (expand-file-name "~/.emacs.d/elpa") 0)
(package-initialize)

(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when window-system
  (mouse-wheel-mode t)
  (blink-cursor-mode -1)
  (add-hook 'before-make-frame-hook 'jart-turn-off-tool-bar))

(setq-default
 ac-auto-show-menu 0.01
 ac-candidate-menu-min 1
 ac-trigger-key "C-i"
 ac-use-fuzzy nil
 coffee-tab-width 2
 color-theme-is-global t
 column-number-mode t
 comment-auto-fill-only-comments t
 compilation-scroll-output 'first-error
 css-indent-offset 2
 custom-file (concat dotfiles-dir "custom.el")
 diff-switches "-u"
 echo-keystrokes 0.1
 ediff-window-setup-function 'ediff-setup-windows-plain
 fill-column 79
 flycheck-completion-system 'ido
 flycheck-display-errors-delay 0.8
 flycheck-idle-change-delay 0.2
 font-lock-maximum-decoration t
 frame-title-format '(buffer-file-name "%f - justinemacs" ("%b"))
 gdb-many-windows t
 git-gutter:lighter " GG"
 ido-create-new-buffer 'always
 ido-enable-flex-matching t
 ido-enable-prefix nil
 ido-ignore-extensions t
 ido-max-prospects 10
 ido-use-filename-at-point 'guess
 indent-tabs-mode nil
 inhibit-startup-message t
 ispell-extra-args '("-m")
 ispell-silently-savep t
 jart-is-colorful (>= (display-color-cells) 256)
 jart-is-linux (not (null (memq system-type '(gnu/linux))))
 jart-is-mac (not (null (memq system-type '(darwin))))
 jart-is-unix (not (null (memq system-type '(gnu/linux darwin berkeley-unix cygwin))))
 jart-is-windows (not (null (memq system-type '(ms-dos windows-nt cygwin))))
 make-backup-files nil
 mouse-yank-at-point t
 python-indent 2
 require-final-newline t
 ring-bell-function 'ignore
 save-place t
 save-place-file (concat dotfiles-dir "places")
 shift-select-mode nil
 tab-width 2
 transient-mark-mode t
 truncate-lines t
 truncate-partial-width-windows nil
 uniquify-buffer-name-style 'forward
 visible-bell nil
 whitespace-line-column 80
 whitespace-style '(face tabs tab-mark lines-tail trailing)
 yas/root-directory (list (concat dotfiles-dir "snippets")))

(if (fboundp 'x-cut-buffer-or-selection-value)
    (setq x-select-enable-clipboard t
          interprogram-paste-function 'x-cut-buffer-or-selection-value))
(defalias 'yes-or-no-p 'y-or-n-p)
(random t)
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)
(add-to-list 'completion-ignored-extensions ".d")  ;; "cc -MD" depends files

(ido-mode t)
(auto-fill-mode 1)
(show-paren-mode 1)
(recentf-mode 1)
(global-font-lock-mode t)
(auto-compression-mode t)
(delete-selection-mode 1)
(show-paren-mode 1)
(global-git-gutter-mode +1)
(global-auto-revert-mode t)
(global-whitespace-mode 1)
(ac-config-default)
(yas-global-mode 1)
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'pager)
(require 'pager-default-keybindings)

(if window-system
    (load-theme 'zenburn t)
  (if (string= (getenv "TERM") "xterm-256color")
      (load-theme 'justine256 t)
    (progn
      (load-theme 'zenburn t)
      (warn "For much prettier colors run: TERM=xterm-256color emacs -nw"))))

;; Performance Improvement: I don't know who thought it was a good idea to ruin
;; the interactivity of this editor by doing a zillion disk seeks whenever I
;; try to open a file, just so it can put the current revision number in the
;; mode line.
(setq vc-handled-backends nil)

;; Performance Improvement: This is another not so great feature that makes
;; emacs slower by doing a zillion stat() calls every time I open a file.
(require 'files)
(defun dir-locals-find-file (file) nil)

(defun jart-sudo (&optional path)
  (interactive)
  (find-alternate-file
   (concat "/sudo:root@localhost:" (or path buffer-file-name))))

(defun jart-forward-paragraph ()
  "Same as `forward-paragraph', but temporarily resets the
paragraph separators to \"\\n\\n\". This is necessary because
many modes (e.g. java-mode) will redefine the definition of a
paragraph so that special bodies of text (e.g. javadoc @sections)
can be filled correctly."
  (interactive)
  (let ((paragraph-start "\n\n")
        (paragraph-end "\n\n"))
    (forward-paragraph)))

(defun jart-backward-paragraph ()
  "Opposite of `jart-forward-paragraph'."
  (interactive)
  (let ((paragraph-start "\n\n")
        (paragraph-end "\n\n"))
    (backward-paragraph)))

(defun jart-unfill-paragraph ()
  "The opposite of fill-paragraph. Takes a multi-line paragraph
and makes it into a single line of text.  Thanks: Stefan Monnier
<foo at acm.org>"
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun jart-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

(defun jart-recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun jart-run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'jart-coding-hook))

(defun jart-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun jart-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun jart-turn-on-paredit ()
  (paredit-mode t))

(defun jart-turn-off-tool-bar ()
  (tool-bar-mode -1))

(defun jart-face-at-point ()
  "Tells me who is responsible for ugly color under cursor"
  (interactive)
  (message "%S: %s" (face-at-point)
           (face-documentation (face-at-point))))

(defmacro jart-nevar-fail (primary failover)
  "Runs primary code.  If primary code fails, then executes
  failover code."
  `(condition-case exc
       ,primary
     ('error
      (message (format "Caught exception: [%s]" exc))
      ,failover)))

(defun jart-paredit-close-parenthesis ()
  "How do you expect me to rebalance my parens if you won't let
  me type omg!"
  (interactive)
  (jart-nevar-fail (paredit-close-parenthesis)
                   (insert ")")))

(defun jart-paredit-close-parenthesis-and-newline ()
  "How do you expect me to rebalance my parens if you won't let
  me type omg!"
  (interactive)
  (jart-nevar-fail (paredit-close-parenthesis-and-newline)
                   (insert ")")))

(defun jart-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(defun jart-note ()
  (interactive)
  (find-file "~/notes.org")
  (goto-char (point-min))
  (org-insert-heading)
  (insert (concat "<" (format-time-string "%Y-%m-%dT%H:%M:%S%z") "> ")))

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.clj$" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'jart-coding-hook 'jart-pretty-lambdas)
(add-hook 'text-mode-hook 'flyspell-mode)

(let ((modes '((go-mode     go-mode-map          'newline-and-indent)
               (python-mode python-mode-map      'newline-and-indent)
               (js2-mode    js2-mode-map         'js2-line-break)
               (sh-script   sh-mode-map          'newline-and-indent)
               (yaml-mode   yaml-mode-map        'newline-and-indent)
               (lisp-mode   lisp-mode-shared-map 'reindent-then-newline-and-indent))))
  (while modes
    (eval-after-load (caar modes)
      `(progn
         (define-key ,(cadar modes) (kbd "<return>") ,(caddar modes))
         (define-key ,(cadar modes) (kbd "RET") ,(caddar modes))))
    (setq modes (cdr modes))))

(eval-after-load 'js2-mode
  '(progn
     ;;(require 'cache-table)
     ;;(require 'skewer-mode)
     (setq js2-basic-offset 4
           js2-bounce-indent-p nil
           js2-enter-indents-newline t
           js2-global-externs '("goog" "occu" "JSON" "console" "gapi")
           ac-js2-evaluate-calls t
           ac-js2-external-libraries
           '("~/justinetunney.com/assets/closure/closure/goog/base.js"
             "~/justinetunney.com/assets/closure/closure/goog/deps.js"))
     ;;(add-hook 'js2-mode-hook 'skewer-mode)
     ;;(add-hook 'js2-mode-hook 'ac-js2-mode)
     ;;(add-hook 'js2-mode-hook 'flyspell-prog-mode)
     (define-key js2-mode-map (kbd "M-/") 'auto-complete)))

(eval-after-load 'markdown-mode
  '(progn
     (defun jart-markdown-updated-timestamp ()
       (interactive)
       (when (or (eq major-mode 'markdown-mode)
                 (eq major-mode 'yaml-mode))
         (message "hello")
         (save-excursion
           (goto-char (point-min))
           (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
             (when (search-forward-regexp "^updated: [-+: TZ0-9]+$" nil t)
               (replace-match (concat "updated: " timestamp)))
             (when (search-forward-regexp "^modified: [-+: TZ0-9]+$" nil t)
               (replace-match (concat "modified: " timestamp)))))))
     (add-hook 'before-save-hook 'jart-markdown-updated-timestamp)
     (add-hook 'markdown-mode-hook 'flyspell-mode)))

(eval-after-load 'go-mode
  '(progn
     (require 'go-flymake)
     (require 'go-flycheck)
     (require 'go-autocomplete)
     (require 'go-errcheck)
     (define-key ac-mode-map (kbd "M-/") 'auto-complete)
     (define-key go-mode-map (kbd "M-.") 'godef-jump)
     (define-key go-mode-map (kbd "C-c C-a") 'go-import-add)
     (define-key go-mode-map (kbd "C-c C-d") 'godef-describe)
     (define-key go-mode-map (kbd "C-c C-j") 'godef-jump)
     (define-key go-mode-map (kbd "C-c C-r") 'go-remove-unused-imports)
     (add-hook 'before-save-hook 'gofmt-before-save)
     (add-hook 'go-mode-hook 'flyspell-prog-mode)))

(eval-after-load 'asm-mode
  '(progn
     (defun jart-asm-mode-hook ()
       (set (make-local-variable 'indent-tabs-mode) t)
       (set (make-local-variable 'tab-width) 8))
     (add-hook 'asm-mode-hook 'jart-asm-mode-hook)))

(eval-after-load 'make-mode
  '(progn
     (defun jart-makefile-mode-hook ()
       (define-key makefile-mode-map (kbd "C-c C-c") 'compile))
     (add-hook 'makefile-mode-hook 'jart-makefile-mode-hook)))

(eval-after-load 'compile
  '(progn
     (defun jart-compilation-mode-hook ()
       (set (make-local-variable 'truncate-lines) nil))
     (add-hook 'compilation-mode-hook 'jart-compilation-mode-hook)))

(eval-after-load 'find-file
  '(progn
     (setq cc-search-directories
           (append cc-search-directories
                   (list "/usr/include/c++/*")))))

(eval-after-load 'calc
  '(progn
     (define-key calc-mode-map (kbd "C-x C-t") 'other-window)))

(eval-after-load 'org
  '(progn
     (defun jart-org-mode-hook ()
       (set (make-local-variable 'whitespace-line-column) 1000))
     (add-hook 'org-mode-hook 'jart-org-mode-hook)))

(eval-after-load 'sh-script
  '(progn
     (defun jart-sh-mode-hook ()
       ;; Disable lines-tail in whitespace-style so we can have long lines.
       (set (make-local-variable 'whitespace-style)
            (let (new (old whitespace-style))
              (while old
                (unless (eq (car old) 'lines-tail)
                  (setq new (cons (car old) new)))
                (setq old (cdr old)))
              new)))
     (setq sh-basic-offset 2
           sh-indentation 2)
     (add-hook 'sh-mode-hook 'jart-sh-mode-hook)))

(eval-after-load 'cc-mode
  '(progn
     (defun jart-c-mode-common-hook ()
       (define-key c-mode-base-map (kbd "C-c C-c") 'compile)
       (define-key c-mode-base-map (kbd "C-c C-d") 'disaster)
       (define-key c-mode-base-map (kbd "C-c C-o") 'ff-find-other-file)
       (define-key c-mode-base-map (kbd "C-c C-h") 'includeme)
       (define-key c-mode-base-map (kbd "C-<return>") 'c-indent-new-comment-line)
       (define-key c-mode-base-map (kbd "C-M-h") 'backward-kill-word))
     (defun jart-c++-mode-hook ()
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
     (setq c-basic-offset 2
           c-file-style nil)
     (add-hook 'c-mode-common-hook 'google-set-c-style)
     (add-hook 'c-mode-common-hook 'google-make-newline-indent)
     (add-hook 'c-mode-common-hook 'jart-c-mode-common-hook)
     (add-hook 'c++-mode-hook 'jart-c++-mode-hook)
     (add-hook 'c++-mode-hook 'flyspell-prog-mode)))

(eval-after-load 'lisp-mode
  '(progn
     (define-key read-expression-map (kbd "TAB") 'completion-at-point)
     (define-key lisp-mode-shared-map (kbd "C-c l") "lambda")
     (define-key lisp-mode-shared-map (kbd "RET")
       'reindent-then-newline-and-indent)
     (define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)
     (define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)
     (define-key lisp-mode-shared-map (kbd "M-.") 'find-function)
     (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
     (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
     (add-hook 'emacs-lisp-mode-hook 'jart-remove-elc-on-save)
     (add-hook 'emacs-lisp-mode-hook 'jart-run-coding-hook)
     (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)))

(eval-after-load 'paredit
  '(progn
     ;; These bindings make paredit easier to use.
     (define-key paredit-mode-map (kbd "C-<return>")
       'jart-paredit-close-parenthesis-and-newline)
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
     (define-key paredit-mode-map (kbd ")") 'jart-paredit-close-parenthesis)
     ;; Overload paredit with the stuff I overloaded in vanilla Emacs.
     (define-key paredit-mode-map (kbd "C-h") 'paredit-backward-delete)
     (define-key paredit-mode-map (kbd "C-M-h") 'paredit-backward-kill-word)))

(eval-after-load 'python
  '(progn
     (defun jart-python-check ()
       "Runs pyflakes and pep8 on current file"
       (interactive)
       (let ((path (file-name-nondirectory buffer-file-name)))
         (compile (format "pyflakes %s ; pep8 --repeat %s" path path))))
     (defun jart-python-check-dir ()
       "Same as `jart-python-check' but for all files in the
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
     (define-key python-mode-map (kbd "C-c c") 'jart-python-check)
     (define-key python-mode-map (kbd "C-c C") 'jart-python-check-dir)
     (define-key python-mode-map (kbd "C-c l") "lambda")
     (define-key python-mode-map (kbd "M-/") 'hippie-expand)
     (ad-activate 'python-calculate-indentation)
     (add-hook 'python-mode-hook 'jart-run-coding-hook)))

(require 'server)
(if (not (server-running-p))
    (server-start))

;; (when (string= (getenv "USER") "jart")
;;   (add-to-list 'load-path "/home/jart-includeme")
;;   (require 'includeme)
;;   (define-key c-mode-base-map (kbd "C-c C-h") 'includeme)
;;   (add-to-list 'load-path "/home/jart/disaster")
;;   (require 'disaster)
;;   (define-key c-mode-base-map (kbd "C-c C-d") 'disaster))

(load "/home/jart/occupywallst.org/assets/closure-library/closure/bin/labs/code/closure.el")
(load "/home/jart/occupywallst.org/assets/closure-library/closure/bin/labs/code/closure_test.el")

;;; init.el ends here
;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
