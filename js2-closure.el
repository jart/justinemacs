;;; js2-closure.el --- js2-closure (Google Closure dependency manager)

;; Copyright (C) 2014 Justine Alexandra Roberts Tunney
;; License: MIT
;; Author: Justine Tunney <jart@google.com>
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
;;   I hope this software makes your life happier <3

;; Installation:
;;
;; First you need to be using `js2-mode' and Google Closure. The next step is
;; to run the script that crawls all your JavaScript sources for provide
;; statements. For example:
;;
;;   ~/.emacs.d/js2-closure-provides.sh \
;;     ~/justinetunney.com/assets/closure/closure/goog \
;;     ~/justinetunney.com/assets/js/jart
;;
;; That will generate an index file in the same directory which should have the
;; same path as `js2-closure-provides-file'. You have to regenerate this file
;; occasionally by hand. Each time you run the script, you should also run
;; M-x `js2-closure-reload' inside emacs.
;;
;; When editing JavaScript files, run M-x `js2-closure-fix' to regenerate the
;; list of goog.require statements.
;;
;; If you want the magic to happen automatically on save, then add the
;; following to your .emacs file:
;;
;;   (eval-after-load 'js2-mode
;;     '(add-hook 'before-save-hook 'js2-closure-save-hook))

;; Notes:
;;
;; This tool was written under the assumption that you're following Google's
;; JavaScript style guide.
;;
;; Pretty much all the algorithms being used in this file are O(n). This should
;; be sufficiently fast, since all comparisons are being performed on interned
;; atoms. If you actually have a JavaScript codebase large enough that this
;; ends up being a problem, feel free to email me.

;;; Code:

(require 'js2-mode)

(defcustom js2-closure-remove-unused t
  "Determines if unused goog.require statements should be auto-removed.
You might want to consider using `js2-closure-whitelist' rather than
disabling this feature."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-closure-whitelist
  '((goog testing asserts))
  "List of goog.require statements that should never be removed.
Each namespace should be specified as an ordered list of atoms,
rather than a dotted string."
  :type 'sexp
  :group 'js2-mode)

(defcustom js2-closure-provides-file
  (concat (file-name-directory
           (or (buffer-file-name) load-file-name))
          "js2-closure-provides.el")
  "Filename of generated elisp file listing all provided namespaces."
  :type 'file
  :group 'js2-mode)

(defvar js2-closure-provides nil
  "List of all closure provided namespaces.")

(defvar js2-closure-globals nil
  "List of all top level labels, derived from `js2-closure-provides'.")

(defun js2-closure--make-identifier (node &optional names)
  "Turn a NODE (or string) into an an ordered list of interned NAMES."
  (cond ((js2-prop-get-node-p node)
         (js2-closure--make-identifier
          (js2-prop-get-node-left node)
          (js2-closure--make-identifier
           (js2-prop-get-node-right node)
           names)))
        ((js2-node-p node)
         (cons (intern (js2-prop-node-name node)) names))
        ((stringp node)
         (let (result)
           (nreverse
            (dolist (label (split-string node "\\.") result)
              (setq result (cons (intern label) result))))))))

(defun js2-closure--identifier-to-string (identifier)
  "Convert IDENTIFIER into a dotted string."
  (let (labels)
    (mapconcat 'identity
               (nreverse
                (dolist (label identifier labels)
                  (setq labels (cons (symbol-name label) labels))))
               ".")))

(defun js2-closure--crawl (on-call on-identifier)
  "Crawl `js2-mode' buffer AST and invoke callbacks on nodes.

ON-CALL will be invoked for all `js2-call-node' nodes, passing
the node itself as the first argument.

ON-IDENTIFIER is invoked for all identifiers, passing as an
argument the last `js2-prop-get-node' in the chain of labels
making up that identifier."
  (let (last)
    (js2-visit-ast-root
     js2-mode-ast
     (lambda (node endp)
       (unless endp
         (when (js2-call-node-p node)
           (funcall on-call node))
         (cond ((and (js2-prop-get-node-p node)
                     (or (not last)
                         (eq last (js2-prop-get-node-left node))))
                (setq last node))
               ((and (not (js2-prop-get-node-p node))
                     last)
                (funcall on-identifier last)
                (setq last nil)))
         t)))
    (when last
      (funcall on-call last))))

(defun js2-closure--determine-closure-requires ()
  "Return a sorted list of closure namespaces that should be imported."
  (let (provides requires references)
    (let ((on-call
           (lambda (node)
             (let ((funk (js2-closure--make-identifier
                          (js2-call-node-target node)))
                   (arg1 (car (js2-call-node-args node))))
               (cond ((and (equal funk '(goog provide))
                           (js2-string-node-p arg1))
                      (let ((item (js2-closure--make-identifier
                                   (js2-string-node-value arg1))))
                        (when (not (member item provides))
                          (setq provides (cons item provides)))))
                     ((and (equal funk '(goog require))
                           (js2-string-node-p arg1))
                      (let ((item (js2-closure--make-identifier
                                   (js2-string-node-value arg1))))
                        (when (not (member item requires))
                          (setq requires (cons item requires)))))))))
          (on-identifier
           (lambda (node)
             (let ((item (js2-closure--make-identifier node)))
               (when (memq (car item) js2-closure-globals)
                 (while item
                   (cond ((member item provides)
                          (setq item nil))
                         ((member item requires)
                          (when (not (member item references))
                            (setq references (cons item references)))
                          (setq item nil))
                         ((member item js2-closure-provides)
                          (when (not (member item references))
                            (setq references (cons item references)))
                          (setq item nil)))
                   (setq item (reverse (cdr (reverse item))))))))))
      (js2-closure--crawl on-call on-identifier))
    (sort (let (result)
            (dolist (item requires)
              (when (or (not js2-closure-remove-unused)
                        (member item js2-closure-whitelist)
                        (member item references))
                (let ((namespace (js2-closure--identifier-to-string item)))
                  (setq result (cons namespace result)))))
            (dolist (item references result)
              (when (member item references)
                (let ((namespace (js2-closure--identifier-to-string item)))
                  (when (not (member namespace result))
                    (setq result (cons namespace result)))))))
          'string<)))

(defun js2-closure--replace-closure-requires (namespaces)
  "Replace the current list of requires with NAMESPACES.

This assumes that all the requires are in one place and sorted,
without indentation or blank lines.  If you don't have any
requires, they'll be added after your provide statements.  If you
don't have those, then this routine will fail.

Effort was also made to avoid needlessly modifying the buffer,
since syntax coloring might take some time to kick back in."
  (save-excursion
    (goto-char 0)
    (if (search-forward-regexp "^goog.require(" nil t)
        (beginning-of-line)
      (progn (search-forward-regexp "^goog.provide(")
             (search-forward-regexp "^$")
             (open-line 1)))
    (while (and namespaces (search-forward-regexp
                            "^goog.require('\\([^']+\\)');" nil t))
      (when (not (string= (match-string 1) (car namespaces)))
        (if (not (string= (match-string 1) (cadr namespaces)))
            (replace-match (car namespaces) t t nil 1)
          (progn (beginning-of-line)
                 (insert (format "goog.require('%s');\n" (car namespaces))))))
      (setq namespaces (cdr namespaces)))
    (forward-line)
    (while (looking-at "^goog.require(")
      (delete-region (point) (save-excursion
                               (forward-line)
                               (point))))
    (while namespaces
      (insert (format "goog.require('%s');\n" (car namespaces)))
      (setq namespaces (cdr namespaces)))))

;;;###autoload
(defun js2-closure-fix ()
  "Fix the goog.require statements for the current buffer."
  (interactive)
  (unless js2-closure-provides
    (js2-closure-reload))
  (js2-closure--replace-closure-requires
   (js2-closure--determine-closure-requires)))

;;;###autoload
(defun js2-closure-reload ()
  "Load precomputed list of provided namespaces into memory."
  (interactive)
  (load js2-closure-provides-file)
  (setq js2-closure-globals nil)
  (dolist (item js2-closure-provides js2-closure-globals)
    (when (not (memq (car item) js2-closure-globals))
      (setq js2-closure-globals (cons (car item) js2-closure-globals))))
  (dolist (item js2-global-externs)
    (when (not (memq (intern item) js2-closure-globals))
      (setq js2-closure-globals (cons (intern item) js2-closure-globals)))))

;;;###autoload
(defun js2-closure-save-hook ()
  "Global save hook to invoke `js2-closure-fix' if in `js2-mode'."
  (interactive)
  (when (eq major-mode 'js2-mode)
    (js2-closure-fix)))

(provide 'js2-closure)

;;; js2-closure.el ends here
