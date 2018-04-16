;;; buildifier.el --- Bazel build file formatter

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
;; This module takes a simple efficient approach to formatting BUILD files on
;; each save.

;;; Installation:
;;
;; You can put the following in your .emacs.d/init.el file:
;;
;;     (require 'buildifier)
;;
;; Please note the `buildifier-bin' command may need to specified manually.
;; It's also possible to customize `buildifier-path-regex'.

;;; Code:

(defcustom buildifier-bin "buildifier"
  "Location of the buildifier binary."
  :type 'string
  :group 'buildifier)

(defcustom buildifier-path-regex
  "BUILD\\|WORKSPACE\\|BAZEL"
  "Regular expression describing base paths that need buildifier."
  :type 'string
  :group 'buildifier)

(defun buildifier ()
  "Run buildifier on current buffer."
  (interactive)
  (when (and (string-match buildifier-path-regex
                           (file-name-nondirectory
                            (buffer-file-name)))
             (executable-find buildifier-bin))
    (let ((p (point))
          (tmp (make-temp-file "buildifier")))
      (write-region nil nil tmp)
      (let ((result (with-temp-buffer
                      (cons (call-process buildifier-bin tmp t nil)
                            (buffer-string)))))
        (if (= (car result) 0)
            (save-excursion
              (erase-buffer)
              (insert (cdr result)))
          (warn "%s failed: %s" buildifier-bin (cdr result)))
        (goto-char p)
        (delete-file tmp nil)))))

(add-hook 'before-save-hook 'buildifier)

(provide 'buildifier)

;;; buildifier.el ends here
