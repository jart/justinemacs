;;; init.el --- justinemacs (Justine Tunney's .emacs File)

;; Copyright (C) 2003-2018 Justine Alexandra Roberts Tunney
;; License: MIT
;; Author: Justine Tunney <jtunney@gmail.com>
;; Version: 0.1
;; URL: http://github.com/jart/justinemacs
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.

;;; Commentary:
;;
;; I hope you enjoy my lovely emacs configuration <3
;;
;; Special care was taken to not add to startup cost.  This was accomplished by
;; making liberal use of `eval-after-load'.

;;; Code:

(defun jart-sudo (&optional path)
  (interactive)
  (find-alternate-file
   (concat "/sudo:root@localhost:" (or path buffer-file-name))))

(defun jart-unfill-paragraph ()
  "Take a multi-line paragraph and make it into a single line.

Thanks: Stefan Monnier <foo@acm.org>"
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

(defun jart-pretty-lambdas ()
  "Make lambda render with the unicode symbol."
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun jart-turn-off-tool-bar ()
  "Callback to turn off ugly toolbar."
  (tool-bar-mode -1))

(defun jart-face-at-point ()
  "Tell me who is responsible for ugly color under cursor."
  (interactive)
  (message "%S: %s" (face-at-point)
           (face-documentation (face-at-point))))

(defun jart-paredit-close-parenthesis ()
  "Make the ')' key in `paredit-mode' more resilient to failure."
  (interactive)
  (condition-case nil
      (paredit-close-parenthesis)
    ('error
     (insert ")"))))

(defun jart-paredit-close-parenthesis-and-newline ()
  "How do you expect me to rebalance my parens if you won't let
  me type omg!"
  (interactive)
  (condition-case nil
      (paredit-close-parenthesis-and-newline)
    ('error
     (insert ")"))))

(defun jart-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(defun jart-note ()
  "Open a new note entry in my notes file."
  (interactive)
  (find-file "~/notes.org")
  (goto-char (point-min))
  (org-insert-heading)
  (insert (concat "<" (format-time-string "%Y-%m-%dT%H:%M:%S%z") "> ")))

(defun jart-yas-expand ()
  "Expands a yasnippet macro.  Should be bound over to \\[transpose-chars]."
  (interactive)
  (condition-case nil
      (yas-expand)
    ('error
     (transpose-chars nil))))

(defmacro jart-normal-paragraphs (body)
  "Set paragraph delimiters back to normal for duration of BODY."
  `(let ((paragraph-start "^L\\|[ \t]*$")
         (paragraph-separate "[ \t^L]*$")
         (c-paragraph-start "^L\\|[ \t]*$"))
     ,body))

(defun jart-sort-paragraph ()
  "Sort lines within paragraph under cursor."
  (interactive)
  (jart-normal-paragraphs
   (let ((start (point))
         (beg (progn (backward-paragraph) (point)))
         (end (progn (forward-paragraph) (point))))
     (sort-lines nil beg end)
     (goto-char start))))

(defun jart-sane-forward-paragraph ()
  "Move to next blank line."
  (interactive)
  (jart-normal-paragraphs
   (forward-paragraph)))

(defun jart-sane-backward-paragraph ()
  "Move to previous blank line."
  (interactive)
  (jart-normal-paragraphs
   (backward-paragraph)))

(defun jart-sane-} ()
  "Insert } with correct indentation."
  (interactive)
  (insert "}")
  (indent-for-tab-command))

(defun jart-open-url (&optional url)
  "Open URL under cursor."
  (interactive)
  (let ((link (or url (thing-at-point-url-at-point))))
    (when (not (jart--url-exists link))
      (error "does not exist"))
    (when (not (string-match "/\\([^/]+\\)$" link))
      (error "oh no"))
    (let ((basename (match-string 1 link)))
      (shell-command-to-string
       (format "wget -qO /tmp/%s %s" basename link))
      (find-file (format "/tmp/%s" basename)))))

(defun jart--url-exists (url)
  "Return nil if URL does not return a 200 OK response."
  (string-match "^HTTP.*\\b200\\b"
                (shell-command-to-string
                 (format "curl -sIL %s" url))))

(defun jart-mirror-url (&optional url)
  "Mirror URL under cursor."
  (interactive)
  (let ((link (or url (thing-at-point-url-at-point))))
    (message (format "%s" (async-shell-command
                           (format "bzmirror %s" link))))))

(defun jart-mirror-check ()
  "Check mirror URLs in current buffer."
  (interactive)
  (save-buffer)
  (async-shell-command
   (format "check-mirror-urls %s" (buffer-file-name))))

(defun jart-copy-checksum-url (&optional url)
  "Download URL and put its sha256 on the kill ring."
  (interactive)
  (let* ((link (or url (thing-at-point-url-at-point)))
         (sha256 (format "%s" (shell-command-to-string
                               (format "summy %s" link)))))
    (jart--replace-nearest-sha256 sha256)
    (kill-new sha256)
    (message sha256)))

(defun jart-change-version (&optional url)
  "Show list of git tags associated with URL."
  (interactive)
  (let* ((link (or url (thing-at-point-url-at-point)))
         (old (jart--extract-revision link))
         (tag (ido-completing-read
               "Pick a tag: " (jart--git-ls-remote-tags
                               (jart--as-github-uri link))))
         (new (if (equal "v" (substring tag 0 1))
                  (substring tag 1)
                tag)))
    (let* ((spot (point))
           (bounds (if (use-region-p)
                       (cons (region-beginning) (region-end))
                     (cons (save-excursion (re-search-backward "^$"))
                           (save-excursion (re-search-forward "^$")))))
           (text (buffer-substring-no-properties (car bounds) (cdr bounds))))
      (delete-region (car bounds) (cdr bounds))
      (insert (replace-regexp-in-string (regexp-quote old) new text))
      (goto-char spot)
      (when (jart--url-exists (thing-at-point-url-at-point))
        (jart-copy-checksum-url)))))

(defun jart--as-github-uri (text)
  "Return list of tags associated with git URI."
  (when (not (string-match "github.com[/:]\\([^/]+\\)/\\([^/.]+\\)[/.]" text))
    (error "oh no"))
  (concat "git://github.com/"
          (match-string 1 text) "/"
          (match-string 2 text) ".git"))

(defun jart--git-ls-remote-tags (uri)
  "Return list of tags associated with git URI."
  (reverse
   (sort (split-string
          (shell-command-to-string
           (concat "git ls-remote --tags " uri
                   " | grep -Po '(?<=refs/tags/)[^^]+'")))
         'jart--semver<)))

(defun jart--extract-revision (url)
  "Return version tag from URL or dies."
  (when (not (string-match "[0-9]+\\.[0-9]+\\.[0-9]+" url))
    (error (format "no version found: %s" url)))
  (match-string 0 url))

(defun jart--semver< (a b &optional op)
  "Return t if semantic version A is less than B."
  (condition-case exc
      (jart--list< (jart--semver-split a)
                   (jart--semver-split b))
    ('error
     (error (format "%s on %s vs. %s aka %S vs. %S"
                    exc a b (jart--semver-split a)
                   (jart--semver-split b))))))

(defun jart--semver-split (v)
  "Split V in a semver fancy way."
  (mapcar (lambda (x)
            (if (string-match "^[0-9]+$" x)
                (string-to-number x)
              x))
          (split-string v "[v.-]" t)))

(defun jart--list< (a b)
  "Return t if A < B as flat lists of strings and numbers."
  (cond ((null a) (not (null b)))
        ((null b) nil)
        ((stringp (car a))
         (cond ((not (stringp (car b))) t)
               ((string= (car a) (car b)) (jart--list< (cdr a) (cdr b)))
               (t (string< (car a) (car b)))))
        ((stringp (car b)) t)
        ((= (car a) (car b)) (jart--list< (cdr a) (cdr b)))
        (t (< (car a) (car b)))))

(defun jart--replace-nearest-sha256 (sha256)
  (interactive)
  (let ((current (line-number-at-pos))
        (forward (save-excursion
                   (when (jart--search-sha256)
                     (line-number-at-pos))))
        (backward (save-excursion
                    (when (jart--search-sha256 t)
                      (line-number-at-pos)))))
    (when (or forward backward)
      (save-excursion
        (jart--search-sha256
         (if (and forward backward)
             (<= (- current backward)
                 (- forward current))
           backward))
        (kill-word nil)
        (insert sha256)))))

(defun jart--search-sha256 (&optional backwards)
  (when (eval (cons (if backwards
                        'search-backward-regexp
                      'search-forward-regexp)
                    '("\"[0-9a-f]\\{64\\}\"" nil t)))
    (if backwards
        (forward-char)
      (backward-word))
    (point)))

(defun jart-python-fill-paragraph ()
  (interactive)
  (end-of-line)
  (if (jart--is-in-python-docstring-section-paragraph)
      (fill-region (save-excursion
                     (search-backward-regexp "^ +[a-zA-Z0-9_]+:" nil t)
                     (when (looking-at " +\\(Returns\\|Yields\\):")
                       (forward-line))
                     (search-forward-regexp "[a-zA-Z0-9_]" nil t)
                     (- (point) 1))
                   (save-excursion
                     (search-forward-regexp
                      "^ +[a-zA-Z0-9_]+:\\|^ +\"\"\"" nil t)
                     (beginning-of-line)
                     (goto-char (- (point) 1))
                     (point)))
    (fill-paragraph)))

(defun jart--is-in-python-docstring-section-paragraph ()
  (when (eq (face-at-point) 'font-lock-doc-face)
    (let* ((docstring-point
            (save-excursion
              (when (search-backward-regexp "\"\"\"" nil t)
                (point))))
           (docstring-offset
            (save-excursion
              (when docstring-point
                (goto-char docstring-point)
                (beginning-of-line)
                (- docstring-point (point)))))
           (paragraph-offset
            (save-excursion
              (end-of-line)
              (when (and docstring-point
                         (search-backward-regexp "^ +[a-zA-Z0-9_]+:" nil t)
                         (> (progn
                              (when (looking-at " +\\(Returns\\|Yields\\):")
                                (forward-line))
                              (search-forward-regexp "[a-zA-Z0-9_]" nil t)
                              (goto-char (- (point) 1))
                              (point))
                            docstring-point))
                (- (point) (progn (beginning-of-line) (point)))))))
      (and paragraph-offset
           docstring-offset
           (> paragraph-offset docstring-offset)))))

(defun jart-show-long-lines ()
  "Customize behavior of long line highlighting."
  (let ((column (cdr (assoc major-mode '((java-mode . 100)
                                         (web-mode . 100)
                                         (js2-mode . 80)
                                         (c-mode . 80)
                                         (c++-mode . 80)
                                         (python-mode . 80)
                                         (markdown-mode . 80)
                                         (text-mode . 80)
                                         (emacs-lisp-mode . 80)
                                         (rst-mode . 80))))))
    (when column
      (column-marker-1 column))))
(add-hook 'after-change-major-mode-hook 'jart-show-long-lines)

(defun jart-js2-fix-paragraph-skipping ()
  "Fix a bug with `js2-mode' that maintainers can't fix."
  (defvar js2--fill-mode nil)
  (defun js2--setup-paragraph-filling (&optional separator)
    (setq c-paragraph-start (or separator js2-paragraph-start))
    (let ((c-buffer-is-cc-mode t))
      (make-local-variable 'paragraph-start)
      (make-local-variable 'paragraph-separate)
      (make-local-variable 'paragraph-ignore-fill-prefix)
      (make-local-variable 'adaptive-fill-mode)
      (make-local-variable 'adaptive-fill-regexp)
      (c-setup-paragraph-variables)))
  (defun js2--should-advise ()
    (or (eq major-mode 'js2-mode)
        (eq major-mode 'java-mode)))
  (defadvice forward-paragraph (before js2--forward-paragraph-before activate)
    (when (and (js2--should-advise)
               (not js2--fill-mode))
      (js2--setup-paragraph-filling "\n\n")))
  (defadvice forward-paragraph (after js2--forward-paragraph-after activate)
    (when (and (js2--should-advise)
               (not js2--fill-mode))
      (js2--setup-paragraph-filling)))
  (defadvice backward-paragraph (before js2--backward-paragraph-before activate)
    (when (and (js2--should-advise)
               (not js2--fill-mode))
      (js2--setup-paragraph-filling "\n\n")))
  (defadvice backward-paragraph (after js2--backward-paragraph-after activate)
    (when (and (js2--should-advise)
               (not js2--fill-mode))
      (js2--setup-paragraph-filling)))
  (defadvice fill-paragraph (before js2--fill-paragraph-before activate)
    (when (js2--should-advise)
      (setq js2--fill-mode t)))
  (defadvice fill-paragraph (after js2--fill-paragraph-after activate)
    (when (js2--should-advise)
      (setq js2--fill-mode nil))))

(defun jart-markdown-updated-timestamp ()
  "Update modified timestamp in Jekyll or Markdown file."
  (interactive)
  (when (or (eq major-mode 'markdown-mode)
            (eq major-mode 'yaml-mode))
    (save-excursion
      (goto-char (point-min))
      (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
        (when (search-forward-regexp "^updated: [-+: TZ0-9]+$" nil t)
          (replace-match (concat "updated: " timestamp)))
        (when (search-forward-regexp "^modified: [-+: TZ0-9]+$" nil t)
          (replace-match (concat "modified: " timestamp)))))))

(defun jart-prev-blank-line ()
  "Set point to previous blank line."
  (interactive)
  (when (looking-at "^$")
    (previous-line))
  (search-backward-regexp "^$" nil t))

(defun jart-next-blank-line ()
  "Set point to next blank line."
  (interactive)
  (when (looking-at "^$")
    (forward-line))
  (search-forward-regexp "^$" nil t))

(defun jart-require-packages (packages)
  (dolist (package packages)
    (when (not (package-installed-p package))
      (package-install package))))

(defun jart-save-word ()
  "Adds word under cursor to personal dictionary."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (word (buffer-substring-no-properties (car bounds)
                                               (cdr bounds))))
    (ispell-send-string (concat "*" word "\n"))
    (add-to-list 'ispell-buffer-session-localwords word)
    (when (fboundp 'flyspell-unhighlight-at)
      (flyspell-unhighlight-at (car bounds)))
    (ispell-pdict-save t t)
    (flyspell-buffer)))

(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-t") 'jart-yas-expand)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-Q") 'jart-unfill-paragraph)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "M-.") 'find-tag)
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "M-{") 'jart-prev-blank-line)
(global-set-key (kbd "M-}") 'jart-next-blank-line)
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
(global-set-key (kbd "C-c a") 'jart-save-word)
(global-set-key (kbd "C-c n") 'jart-cleanup-buffer)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c C-o") 'jart-open-url)
(global-set-key (kbd "C-c m") 'jart-mirror-url)
(global-set-key (kbd "C-c s") 'jart-copy-checksum-url)
(global-set-key (kbd "C-c v") 'jart-change-version)
(global-set-key (kbd "C-c C-y") 'magit-blame-mode)
(global-set-key (kbd "C-c S") 'jart-sort-paragraph)
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

;; Justine's special key-bindings you probably don't want.
(keyboard-translate ?\C-u ?\C-x)
(keyboard-translate ?\C-x ?\C-u)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-x C-h") 'help)
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-g") 'grep-find)
(global-set-key (kbd "C-x b") 'ibuffer)
(global-unset-key (kbd "C-/"))

;; Fix cosmetics quickly and reliably.
(random t)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(defalias 'yes-or-no-p 'y-or-n-p)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'x-cut-buffer-or-selection-value)
    (setq x-select-enable-clipboard t
          interprogram-paste-function 'x-cut-buffer-or-selection-value))
(condition-case exc
    (progn
      (add-to-list 'custom-theme-load-path
                   (concat user-emacs-directory "themes"))
      (if window-system
          (progn
            (mouse-wheel-mode t)
            (blink-cursor-mode 1)
            (add-hook 'before-make-frame-hook 'jart-turn-off-tool-bar)
            (add-to-list 'default-frame-alist '(height . 70))
            (add-to-list 'default-frame-alist '(width . 120))
            ;; (let ((myfont "DejaVu Sans Mono-7"))
            ;;   (set-frame-font myfont)
            ;;   (add-to-list 'default-frame-alist (cons 'font myfont)))
            (load-theme 'zenburn t))
        (if (string= (getenv "TERM") "xterm-256color")
            (load-theme 'justine256 t)
          (progn
            (load-theme 'zenburn t)
            (warn "For prettier colors: TERM=xterm-256color emacs -nw")))))
  ('error
   (warn (format "Caught exception: [%s]" exc))))

;; Initialise package manager, which takes time, especially on first run.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(when (not (package-installed-p 'pager))
  (package-refresh-contents)
  (jart-require-packages
   '(pager
     pager-default-keybindings
     paredit
     web-mode
     markdown-mode
     magit
     js2-mode)))

(setq magit-last-seen-setup-instructions "1.4.0")

;; Custom settings.
(custom-set-variables
 '(ac-auto-show-menu 0.01)
 '(ac-candidate-menu-min 1)
 '(ac-dictionary-directories nil)
 '(ac-dictionary-files nil)
 '(ac-js2-evaluate-calls nil)
 '(ac-trigger-key "C-i")
 '(ac-use-fuzzy nil)
 '(c-basic-offset 2)
 '(c-file-style nil)
 '(c-font-lock-extra-types
   (quote
    ("\\sw+_t" "bool" "complex" "imaginary" "FILE" "lconv" "tm" "va_list" "jmp_buf" "Lisp_Object" "u?int[136]?[862]" "complex64" "complex128")))
 '(coffee-tab-width 2)
 '(color-theme-is-global t)
 '(column-number-mode t)
 '(comment-auto-fill-only-comments t)
 '(compilation-scroll-output 'first-error)
 '(css-indent-offset 2)
 '(custom-file (concat user-emacs-directory "custom.el"))
 '(diff-switches "-u")
 '(echo-keystrokes 0.1)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(fill-column 79)
 '(flycheck-completion-system 'ido)
 '(flycheck-display-errors-delay 0.5)
 '(flycheck-idle-change-delay 0.8)
 '(flycheck-locate-config-file-functions
   '(flycheck-locate-config-file-absolute-path
     flycheck-locate-config-file-home))
 '(font-lock-maximum-decoration t)
 '(frame-title-format '(buffer-file-name "%f - justinemacs" ("%b")))
 '(gdb-many-windows t)
 '(git-gutter:lighter " GG")
 '(ido-create-new-buffer 'always)
 '(ido-enable-flex-matching t)
 '(ido-enable-prefix nil)
 '(ido-ignore-extensions t)
 '(ido-max-prospects 10)
 '(ido-use-filename-at-point 'guess)
 '(indent-tabs-mode nil)
 '(inhibit-startup-message t)
 '(ispell-extra-args '("--encoding=utf-8" "--master=en_US" "--sug-mode=ultra"))
 '(ispell-silently-savep t)
 '(jart-is-colorful (>= (display-color-cells) 256))
 '(jart-is-linux (not (null (memq system-type '(gnu/linux)))))
 '(jart-is-mac (not (null (memq system-type '(darwin)))))
 '(jart-is-unix (not (null (memq system-type '(gnu/linux darwin berkeley-unix cygwin)))))
 '(jart-is-windows (not (null (memq system-type '(ms-dos windows-nt cygwin)))))
 '(js2-basic-offset 2)
 '(js2-closure-whitelist
   '(

     "goog.testing.asserts"
     "goog.testing.jsunit"

     ))
 '(js2-bounce-indent-p nil)
 '(js2-enter-indents-newline t)
 '(js2-global-externs
   '(

     ;; Miscellaneous.
     "$"
     "Dygraph"
     "JSON"
     "TestCase"
     "angular"
     "bazel"
     "console"
     "exports"
     "fail"
     "gapi"
     "goog"
     "guestbook"
     "io"
     "jart"
     "jstestdriver"
     "learning"
     "occu"
     "phantom"
     "proto"
     "registry"
     "require"
     "tb"
     "tf"
     "third_party"
     "vz"

     ;; Closure Library testing functions.
     "assert"
     "assertArrayEquals"
     "assertContains"
     "assertElementsEquals"
     "assertElementsRoughlyEqual"
     "assertEquals"
     "assertEvaluatesToFalse"
     "assertEvaluatesToTrue"
     "assertFalse"
     "assertHTMLEquals"
     "assertHashEquals"
     "assertNaN"
     "assertNonEmptyString"
     "assertNotContains"
     "assertNotEquals"
     "assertNotNaN"
     "assertNotNull"
     "assertNotNullNorUndefined"
     "assertNotThrows"
     "assertNotUndefined"
     "assertNull"
     "assertObjectEquals"
     "assertObjectNotEquals"
     "assertObjectRoughlyEquals"
     "assertRegExp"
     "assertRoughlyEquals"
     "assertSameElements"
     "assertThrows"
     "assertTrue"
     "assertUndefined"

     ))
 '(js2-indent-switch-body t)
 '(js2-strict-trailing-comma-warning nil)
 '(magit-last-seen-setup-instructions "1.4.0")
 '(magit-stage-all-confirm nil)
 '(magit-unstage-all-confirm nil)
 '(make-backup-files nil)
 '(mmm-global-mode 'maybe)
 '(mouse-yank-at-point t)
 '(python-indent 2)
 '(require-final-newline t)
 '(ring-bell-function 'ignore)
 '(save-place t)
 '(save-place-file (concat user-emacs-directory "places"))
 '(sh-basic-offset 2)
 '(sentence-end-double-space nil)
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(shift-select-mode nil)
 '(tab-width 2)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(truncate-partial-width-windows nil)
 '(typescript-indent-level 2)
 '(uniquify-buffer-name-style 'forward)
 '(vc-handled-backends nil)
 '(visible-bell nil)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-engines-alist '(("liquid" . "\\.\\(html\\|xml\\)\\'")
                            ("angular" . "\\.ng\\'")))
 '(web-mode-tag-auto-close-style 1)
 '(whitespace-line-column 80)
 '(whitespace-style '(face tabs tab-mark lines-tail trailing))
 '(yas-snippet-dirs (list (concat user-emacs-directory "snippets"))))

;; UI enhancements.
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)
(add-to-list 'completion-ignored-extensions ".d")  ;; "cc -MD" depends files

;; Enable global modes.
(ido-mode t)
(auto-fill-mode 1)
(show-paren-mode 1)
(recentf-mode 1)
(global-font-lock-mode t)
(auto-compression-mode t)
(delete-selection-mode 1)
(show-paren-mode 1)
;; (global-git-gutter-mode +1)
;; (global-auto-revert-mode 1)
;; (global-whitespace-mode 1)
;; (ac-config-default)
;; (yas-global-mode 1)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; (add-hook 'text-mode-hook 'flyspell-mode)

;; Configure Spelling
(setq ispell-program-name
      (or (executable-find "aspell")
          (executable-find "~/homebrew/bin/aspell")))

;; Load some libraries.
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'pager)
(require 'pager-default-keybindings)

;; File extension to mode mappings.
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.gss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.bzl$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.\\(html\\|xml\\|soy\\|css\\|ng\\)$" . web-mode))
(add-to-list 'auto-mode-alist '("BAZEL$" . python-mode))
(add-to-list 'auto-mode-alist '("BUILD$" . python-mode))
(add-to-list 'auto-mode-alist '("BUILD\\.OPENSOURCE$" . python-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE$" . python-mode))
(setq web-mode-engines-alist '(("angular" . "\\.\\(html\\|xml\\)\\'")
                               ("angular" . "\\.ng\\'")))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.rl$" . mmm-mode))

(eval-after-load 'markdown-mode
  '(progn
     (define-key markdown-mode-map (kbd "M-{") 'jart-sane-backward-paragraph)
     (define-key markdown-mode-map (kbd "M-}") 'jart-sane-forward-paragraph)
     (add-hook 'markdown-mode-hook 'flyspell-mode)))

;; Performance Improvement: This is another not so great feature that makes
;; emacs slower by doing a zillion stat() calls every time I open a file.
(require 'files)
(defun dir-locals-find-file (file)
  "Override default function to do nothing with FILE."
  nil)

;; (require 'cl)

;; Make return auto-indent and work inside comments.
(let ((modes '((cc-mode     java-mode-map        'c-indent-new-comment-line)
               (go-mode     go-mode-map          'newline-and-indent)
               (js2-mode    js2-mode-map         'js2-line-break)
               (lisp-mode   lisp-mode-shared-map 'reindent-then-newline-and-indent)
               (python-mode python-mode-map      'newline-and-indent)
               (sh-script   sh-mode-map          'newline-and-indent)
               (yaml-mode   yaml-mode-map        'newline-and-indent))))
  (while modes
    (eval-after-load (caar modes)
      `(progn
         (define-key ,(car (cdar modes)) (kbd "<return>") ,(cadr (cdar modes)))
         (define-key ,(car (cdar modes)) (kbd "RET") ,(cadr (cdar modes)))))
    (setq modes (cdr modes))))

(add-to-list 'completion-ignored-extensions ".test")
(defadvice completion--file-name-table (after
                                        ignoring-backups-f-n-completion
                                        activate)
  "Filter out results when they match `completion-ignored-extensions'."
  (let ((res ad-return-value))
    (if (and (listp res)
             (stringp (car res))
             (cdr res))                 ; length > 1, don't ignore sole match
        (setq ad-return-value
              (completion-pcm--filename-try-filter res)))))

(eval-after-load 'web-mode
  '(progn
     (defun jart-web-mode-hook ()
       (interactive)
       (when (string= web-mode-engine "closure") ;; soy templates
         (set (make-local-variable 'whitespace-line-column) 100)))
     (add-hook 'web-mode-hook 'jart-web-mode-hook)
     (add-hook 'web-mode-hook 'flyspell-prog-mode)))

(eval-after-load 'js2-mode
  '(progn
     (defconst js2-jsdoc-empty-tag-regexp
       (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
               (regexp-opt
                '("author"
                  "class"
                  "const"
                  "constant"
                  "constructor"
                  "constructs"
                  "deprecated"
                  "desc"
                  "description"
                  "event"
                  "example"
                  "exec"
                  "export"
                  "fileoverview"
                  "final"
                  "function"
                  "hidden"
                  "ignore"
                  "implicitCast"
                  "inheritDoc"
                  "inner"
                  "interface"
                  "license"
                  "ngInject"
                  "noalias"
                  "noshadow"
                  "notypecheck"
                  "override"
                  "owner"
                  "preserve"
                  "preserveTry"
                  "private"
                  "protected"
                  "public"
                  "static"
                  "supported"
                  "typedef"
                  "addon"))
               "\\)\\)\\s-*")
       "Matches empty jsdoc tags.")
     (define-key js2-mode-map (kbd "M-/") 'auto-complete)
     (remove-hook 'before-save-hook 'js2-closure-save-hook)
     (jart-js2-fix-paragraph-skipping)))

(eval-after-load 'markdown-mode
  '(progn
     (add-hook 'before-save-hook 'jart-markdown-updated-timestamp)
     (add-hook 'markdown-mode-hook 'flyspell-mode)))

(eval-after-load 'go-mode
  '(progn
     (add-to-list 'load-path "~/go/src/github.com/nsf/gocode/emacs")
     (add-to-list 'load-path "~/go/src/github.com/dougm/goflymake")
     (require 'go-flymake)
     ;;(require 'go-flycheck)
     (require 'go-autocomplete)
     (require 'go-errcheck)
     (define-key ac-mode-map (kbd "M-/") 'auto-complete)
     (define-key go-mode-map (kbd "M-.") 'godef-jump)
     (define-key go-mode-map (kbd "C-c C-a") 'go-import-add)
     (define-key go-mode-map (kbd "C-c C-r") 'go-remove-unused-imports)
     (define-key go-mode-map (kbd "C-c C-d") 'godef-describe)
     (define-key go-mode-map (kbd "C-c C-j") 'godef-jump)
     (define-key go-mode-map (kbd "C-c C-r") 'go-remove-unused-imports)
     (defun jart--go-mode-hook ()
       (flycheck-mode -1)
       (set (make-local-variable 'whitespace-line-column) 10000))
     (defun jart--gofmt-before-save ()
       ;; Don't run gofmt on Ragel files.
       (unless (string-match "\\.rl$" (buffer-name))
         (gofmt-before-save)))
     (add-hook 'go-mode-hook 'jart--go-mode-hook)
     (add-hook 'before-save-hook 'jart--gofmt-before-save)))

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
     (add-hook 'sh-mode-hook 'jart-sh-mode-hook)
     (add-hook 'sh-mode-hook 'flyspell-prog-mode)))

(eval-after-load 'coffee-mode
  '(progn
     (define-key coffee-mode-map (kbd "C-M-h") 'backward-kill-word)))

(eval-after-load 'cc-mode
  '(progn
     (defun jart-c-mode-common-hook ()
       (define-key c-mode-base-map (kbd "C-c C-c") 'compile)
       (define-key c-mode-base-map (kbd "C-c C-d") 'disaster)
       (define-key c-mode-base-map (kbd "C-c C-o") 'ff-find-other-file)
       (define-key c-mode-base-map (kbd "C-c C-h") 'includeme)
       (define-key c-mode-base-map (kbd "C-<return>") 'c-indent-new-comment-line)
       (define-key c-mode-base-map (kbd "C-M-h") 'backward-kill-word)
       (define-key c-mode-base-map (kbd "M-{") 'jart-sane-backward-paragraph)
       (define-key c-mode-base-map (kbd "M-}") 'jart-sane-forward-paragraph))
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
     (define-key lisp-mode-shared-map (kbd "M-.") 'find-function-at-point)
     (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
     (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
     (add-hook 'emacs-lisp-mode-hook 'jart-remove-elc-on-save)
     (add-hook 'emacs-lisp-mode-hook 'jart-pretty-lambdas)
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
     (defun jart-python-mode-hook ()
       (interactive)
       (flycheck-mode -1))
     (setq python-indent-offset 2)
     (define-key python-mode-map (kbd "C-c c") 'jart-python-check)
     (define-key python-mode-map (kbd "C-c C") 'jart-python-check-dir)
     (define-key python-mode-map (kbd "C-c l") "lambda")
     (define-key python-mode-map (kbd "M-/") 'hippie-expand)
     (ad-activate 'python-calculate-indentation)
     (add-hook 'python-mode-hook 'jart-pretty-lambdas)
     (add-hook 'python-mode-hook 'jart-python-mode-hook)
     (add-hook 'python-mode-hook 'flyspell-prog-mode)))

(eval-after-load 'autorevert
  '(progn
     (defun jart-auto-revert-mode-hook ()
       "Refresh syntax highlighting after buffer refreshes on new contents."
       (interactive)
       (font-lock-fontify-buffer))
     (add-hook 'auto-revert-mode-hook 'jart-auto-revert-mode-hook)))

;; (require 'mmm-mode)
;; (mmm-add-group
;;  'ragel
;;  '((ragel-block
;;     :submode ragel-mode
;;     :front "%%{"
;;     :back "}%%"
;;     :include-front t
;;     :include-back t
;;     :insert ((?{ ragel-block nil @ "%%{" @ "\n" _ "\n" @ "}%%" @)))
;;    (ragel-line
;;     :submode ragel-mode
;;     :front "%% "
;;     :back "\n"
;;     :include-front t
;;     :insert ((?\  ragel-block nil @ "%% " @ "" _ "" @ "\n" @)))))
;; (mmm-add-mode-ext-class 'go-mode nil 'ragel)

;; (define-generic-mode 'ragel-mode
;;   '(?#) ;; Comments
;;   '(
;;     ;; Keywords
;;     "machine" "action" "access" "context" "include" "import" "export"
;;     "prepush" "postpop" "when" "inwhen" "outwhen" "err" "lerr" "eof" "from"
;;     "to" "alphtype" "getkey" "write"
;;     ;; Rules
;;     "any" "ascii" "extend" "alpha" "digit" "alnum" "lower" "upper"
;;     "xdigit" "cntrl" "graph" "print" "punct" "space" "zlen" "empty"
;;     ;; Inline code matching
;;     "fpc" "fc" "fcurs" "fbuf" "fblen" "ftargs" "fstack"
;;     "fhold" "fgoto" "fcall" "fret" "fentry" "fnext" "fexec" "fbreak"
;;     )
;;   '(
;;     ;; Literals
;;     ;;("\\([^\\)]*\\)" . font-lock-constant-face)
;;     ;;("\\[[[^\\]]*\\]" . font-lock-constant-face)
;;     ("\(\"\\?'\"\'|\\?\"'\|'[^']*'\|\"[^\"]*\"\)" . font-lock-constant-face)
;;     ;; Numbers
;;     ("\\<[0-9][0-9]*\\>" . font-lock-constant-face)
;;     ("\\<0x[0-9a-fA-F][0-9a-fA-F]*\\>" . font-lock-constant-face)
;;     ;; Operators
;;     ("[>$%@]" . font-lock-constant-face)
;;     ("<>\|<" . font-lock-constant-face)
;;     ;;("[>\<$%@][!\^/*~]" . font-lock-constant-face)
;;     ;;("[>$%]?" . font-lock-constant-face)
;;     ;;("<>[!\^/*~]" . font-lock-constant-face)
;;     ("=>" . font-lock-constant-face)
;;     ("->" . font-lock-constant-face)
;;     (":>" . font-lock-constant-face)
;;     (":>>" . font-lock-constant-face)
;;     ("<:" . font-lock-constant-face)
;;     )
;;   nil ;'(".rl\\'")
;;   nil
;;   "Generic mode for mmm-mode editing .rl files.")

(server-start)

;;; init.el ends here
