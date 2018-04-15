;;; bazel-workspace.el --- Bazel WORKSPACE management tools

;; Copyright 2018 Google LLC

;; Author: Justine Tunney <jart@google.com>
;; Version: 0.1.0
;; License: Apache 2.0
;; Keywords: bazel

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;    http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:
;;
;; This tool makes it easy to manage Bazel third party repository definitions,
;; such as the ones in TensorFlow's workspace.bzl file.

;;; Installation:
;;
;; You can put the following in your .emacs.d/init.el file:
;;
;;     (require 'bazel-workspace)
;;     (global-set-key (kbd "C-c C-o") 'jart-url-open)
;;     (global-set-key (kbd "C-c j") 'jart-url-exists-all)
;;     (global-set-key (kbd "C-c m") 'jart-url-mirror)
;;     (global-set-key (kbd "C-c s") 'jart-url-update-sha256)
;;     (global-set-key (kbd "C-c v") 'jart-version-change)
;;
;; Please note the `bzmirror` command may need to specified manually, since it
;; relies on access to a particular GCS bucket that mirrors files.

;;; Code:

(defun jart-url-open (&optional url)
  "Open URL under cursor.

This command makes it possible, with a few keystrokes, to see
what's inside a tarball, so its licensing and structure can be
easily determined."
  (interactive)
  (let* ((url (or url (thing-at-point-url-at-point)
                  (user-error "No URL at point")))
         (path (concat "/tmp/" (file-name-nondirectory url))))
    (unless (jart--url-download url path)
      (error "URL does not exist: %s" url))
    (find-file path)))

(defun jart-url-mirror (&optional url)
  "Mirror URL under cursor.

This invokes the `bzmirror` command script, so the URL can be
mirrored to something like a GCS bucket."
  (interactive)
  (if (executable-find "bzmirror")
      (let* ((url (or url (thing-at-point-url-at-point)
                      (user-error "No URL at point")))
             (process (format "bzmirror %s" url))
             (buffer (format "*Mirror %s*" url)))
        (start-process process buffer "bzmirror" url)
        (display-buffer buffer '(nil (allow-no-window . t))))
    (warn "bzmirror program not found")))

(defun jart-url-exists (&optional url)
  "Return nil if URL does not lead to a 200 OK response.

This function will follow redirects. It should not need to
download the entire contents of URL in order to determine that it
exists."
  (interactive)
  (setq url (or url (thing-at-point-url-at-point)
                (user-error "No URL at point")))
  (with-temp-buffer
    (and (= 0 (call-process
               "curl" nil (list (current-buffer) nil) nil
               "-sIL" url))
         (or (search-backward-regexp "^HTTP.*\\b200\\b" nil t)
             (and (search-forward-regexp "^HTTP.*\\b403\\b" nil t)
                  (jart--url-exists-expensive url))))))

(defun jart--url-exists-expensive (url)
  (interactive)
  (let (result (path (make-temp-file "jart-url-exists")))
    (setq result (jart--url-download url path))
    (delete-file path nil)
    result))

(defvar jart--url-sha256-cache nil)

(defun jart-url-sha256 (url)
  "Returns sha256 of URL

This invokes the `bzmirror` command script, so the URL can be
mirrored to something like a GCS bucket."
  (let* ((key (intern url))
         (cache (assq key jart--url-sha256-cache)))
    (if cache
        (cdr cache)
      (let* ((path (make-temp-file "jart-copy-checksum-url"))
             (sha256 (or (and (not (string-match "TODO" url))
                              (jart-url-exists url)
                              (jart--url-download url path)
                              (jart--file-sha256 path))
                         (make-string 64 ?f))))
        (delete-file path nil)
        (setq jart--url-sha256-cache
              (cons (cons key sha256)
                    jart--url-sha256-cache))
        sha256))))

(defun jart-url-exists-all ()
  "Check that all URLs in current buffer exist.

This function runs `jart-url-exists' on each url. If a mirror URL
turns out to not exist, then a prompt will ask if it should be
mirrored using `jart-url-mirror'. If normal URLs don't exist,
then a warning is displayed for each one."
  (interactive)
  (let (urls)
    (save-excursion
      (goto-char 0)
      (while (search-forward-regexp "https?://" nil t)
        (let ((url (thing-at-point-url-at-point)))
          (when url
            (setq urls (cons url urls))))))
    (dolist (url urls)
      (message "Checking %s..." url)
      (redisplay)
      (unless (jart-url-exists url)
        (if (string-match "mirror\\.bazel\\.build/\\([^\"'),]+\\)" url)
            (let ((original (concat "http://" (match-string 1 url))))
              (if (and (jart-url-exists original)
                       (not (equal (substring original -1) "/")))
                  (when (yes-or-no-p (format "Want to mirror %s? " original))
                    (jart-url-mirror original))
                (warn "Evil URL in codebase: %s" url)))
          (warn "Bad URL: %s" url))))
    (message "Done checking URLs!")))

(defun jart--check-mirror-urls (path)
  (async-shell-command
   (format "check-mirror-urls %s" (shell-quote-argument path))))

(defun jart-url-update-sha256 (&optional url)
  "Replace nearest sha256 in buffer with body of URL.

This downloads the full contents of the URL under cursor (if not
specified), computes its sha256 value, and intelligently tries to
find the nearest sha256 value in the current buffer. It then
updates that value.

This is useful when changing the versions of dependencies in
Bazel WORKSPACE file. Please note `jart-version-change' is
sometimes more useful for doing this."
  (interactive)
  (let* ((url (or url (thing-at-point-url-at-point)))
         (path (make-temp-file "jart-copy-checksum-url"))
         (sha256 (and (jart--url-download url path)
                      (jart--file-sha256 path))))
    (delete-file path nil)
    (unless sha256
      (error "Could not download and sha256: %s" url))
    (jart--replace-nearest-sha256 sha256)
    (kill-new sha256)
    (message sha256)))

(defun jart--url-download (url path)
  "Download URL to PATH or return nil."
  (when (and url path)
    (= 0 (cond ((executable-find "curl")
                (call-process
                 "curl" nil `((:file ,(file-truename path)) nil) nil
                 "-sfL" url))
               ((executable-find "wget")
                (call-process
                 "wget" nil `((:file ,(file-truename path)) nil) nil
                 "-qO" "-" url))
               (t (error "Could not find curl or wget"))))))

(defun jart--file-sha256 (path)
  "Return sha256 checksum of contents of PATH or nil."
  (when path
    (let ((cmd (executable-find "sha256sum")))
      (unless cmd
        (unless (setq cmd (executable-find "shasum"))
          (error "Could not find sha256sum or shasum"))
        (setq cmd (concat cmd " -a 256")))
      (let ((output (shell-command-to-string
                     (format "%s %s" cmd (shell-quote-argument path)))))
        (when (string-match "\\`\\([0-9a-f]\\{64\\}\\) " output)
          (match-string 1 output))))))

(defun jart--replace-nearest-sha256 (sha256)
  "Replace sha256 nearest to point with SHA256."
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

(defun jart-version-change ()
  "Upgrade Bazel workspace definition around URL at point.

This function only currently works on GitHub tag releases. It
fetches the full list of release tags from the repository under
the cursor, and presents an IDO menu so one can be interactively
chosen.

Once a tag is chosen, the version number strings within the block
of code around cursor will all be replaced. The checksum of the
url under cursor will also be computed, so the sha256 attribute
can be updated."
  ;; TODO: Add support for upgrading GitHub URLs with a SHA.
  (interactive)
  (let* ((url (or (thing-at-point-url-at-point)
                  (user-error "No URL at point")))
         (old (or (jart-extract-version url)
                  (and (string-match "/master\\." url)
                       "master")
                  (user-error "No version found: %s" url)))
         (tag (ido-completing-read
               "Pick a tag: "
               (jart-git-ls-remote-tags
                (or (jart-extract-github-uri url)
                    (user-error "No GitHub URI in: %s" url)))))
         (new (jart-remove-prefixes tag '("v"))))
    (let* ((spot (point))
           (bounds (if (use-region-p)
                       (cons (region-beginning) (region-end))
                     (cons (save-excursion (re-search-backward "^$"))
                           (save-excursion (re-search-forward "^$")))))
           (text (buffer-substring-no-properties (car bounds) (cdr bounds))))
      (delete-region (car bounds) (cdr bounds))
      (insert (replace-regexp-in-string (regexp-quote old) new text))
      (goto-char spot)
      (when (jart-url-exists (thing-at-point-url-at-point))
        (jart-url-update-sha256)))))

(defun jart-remove-prefixes (string prefixes)
  "Return STRING with PREFIXES removed in order."
  (when string
    (let ((p 0))
      (dolist (prefix prefixes)
        (when (and (>= (- (length string) p) (length prefix))
                   (equal prefix (substring string p (+ p (length prefix)))))
          (setq p (+ p (length prefix)))))
      (substring string p))))

(defun jart-remove-suffixes (string suffixes)
  "Return STRING with SUFFIXES removed in order."
  (when string
    (let ((p (length string)))
      (dolist (suffix suffixes)
        (when (and (>= p (length suffix))
                   (equal suffix (substring string (- p (length suffix)))))
          (setq p (- p (length suffix)))))
      (substring string 0 p))))

(defconst jart-version-semver-regexp
  (concat "\\(0\\|[1-9][0-9]*\\)\\."  ;; 1. major
          "\\(0\\|[1-9][0-9]*\\)\\."  ;; 2. minor
          "\\(0\\|[1-9][0-9]*\\)"     ;; 3. patch
          "\\(?:-\\("                 ;; 4. pre-release
          "\\(?:0\\|[1-9][0-9]*\\|[0-9]*[a-zA-Z-][0-9a-zA-Z-]*\\)"
          "\\(?:\\.(0\\|[1-9][0-9]*\\|[0-9]*[a-zA-Z-][0-9a-zA-Z-]*)\\)*"
          "\\)\\)?"
          "\\(?:\\+\\("               ;; 5. build metadata
          "[0-9a-zA-Z-]+"
          "\\(\\.[0-9a-zA-Z-]+\\)*"
          "\\)\\)?")
  "Regular expression matching Semantic Version strings.")

(defconst jart-version-pep440-regexp
  (concat "\\(?:"
          "\\(?:\\(?6:[0-9]+\\)!\\)?"            ;; 6. epoch
          "\\(?1:0\\|[1-9][0-9]*\\)\\."          ;; 1. major
          "\\(?2:0\\|[1-9][0-9]*\\)"             ;; 2. minor
          "\\(?:\\.\\(?3:0\\|[1-9][0-9]*\\)\\)?" ;; 3. micro
          "\\(?:[-_.]?"                          ;; 7. pre-release category
          "\\(?7:a\\|b\\|c\\|rc\\|alpha\\|beta\\|pre\\|preview\\)"
          "\\(?:[-_.]?\\(?4:[0-9]+\\)?\\)"       ;; 4. pre-release number
          "\\)?"
          "\\(?:"                                ;; 8. post release category
          "\\(?:-\\(?9:[0-9]+\\)\\)\\|"          ;; 9. post release number
          "\\(?:[-_.]?"
          "\\(?8:post\\|rev\\|r\\)"
          "[-_.]?\\(?9:[0-9]+\\)?\\)"
          "\\)?"
          "\\(?:[-_.]?"                          ;; 10. dev release word
          "\\(?10:dev\\)[-_.]?\\(?11:[0-9]+\\)?" ;; 11. dev release number
          "\\)?"
          "\\)"
          "\\(?:\\+"                             ;; 5. local version
          "\\(?5:[a-z0-9]+\\(?:[-_.][a-z0-9]+\\)*\\)"
          "\\)?")
  "Regular expression matching Python version strings.")

(defcustom jart-version-ignore-suffixes
  '(".zip"
    ".gz"
    ".bz2"
    ".xz"
    ".tar")
  "List of strings to chop from versions."
  :type '(repeat string)
  :group 'jart)

(defun jart-extract-version (string)
  "Return version from STRING or returns nil."
  (jart-remove-suffixes
   (cond ((string-match jart-version-semver-regexp string)
          (match-string 0 string))
         ((string-match jart-version-pep440-regexp string)
          (match-string 0 string)))
   jart-version-ignore-suffixes))

(defun jart-extract-github-uri (text)
  "Return list of tags associated with git URI or returns nil."
  (when text
    (when (string-match "github.com[/:]\\([^/]+\\)/\\([^/.]+\\)[/.]" text)
      (concat "git://github.com/"
              (match-string 1 text) "/"
              (match-string 2 text) ".git"))))

(defun jart-git-ls-remote-tags (uri)
  "Return sorted list of tags associated with git URI.

The returned list is reverse ordered. Priority is given to tags
meeting Emacs' definition of versioning. All other strings, e.g.
jpeg9a, get reverse `string<' ordered at the bottom of the list."
  (sort (split-string
         (shell-command-to-string
          (concat "git ls-remote --tags " (shell-quote-argument uri)
                  " | perl -nle 'print $& if m{(?<=refs/tags/)[^^]+}'")))
        (lambda (a b)
          (let ((la (ignore-errors
                      (version-to-list (jart-remove-prefixes a '("v")))))
                (lb (ignore-errors
                      (version-to-list (jart-remove-prefixes b '("v"))))))
            (if (and la lb)
                (not (version-list-< la lb))
              (if (or la lb)
                  la
                (not (string< a b))))))))

(defun jart--remove-non-numeric-prefix-chars (s)
  "Remove anything that isn't a digit from beginning of S."
  (replace-regexp-in-string "\\`[^0-9]*" "" s))

(provide 'bazel-workspace)

;;; bazel-workspace.el ends here
