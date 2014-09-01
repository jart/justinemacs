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

;;; Installation:
;;
;; First you need to be using `js2-mode' and Google Closure.  The next step is
;; to run the script that crawls all your JavaScript sources for provide
;; statements.  For example:
;;
;;   js2-closure-provides.sh \
;;     ~/justinetunney.com/assets/closure/closure/goog \
;;     ~/justinetunney.com/assets/js/jart \
;;     >~/.emacs.d/js2-closure-provides.sh
;;
;; That will generate an index file in the same directory which should have the
;; same path as `js2-closure-provides-file'.  You have to regenerate this file
;; occasionally by hand.  Each time you run the script, you should also run M-x
;; `js2-closure-reload' inside emacs.
;;
;; To use this, you simply run M-x `js2-closure-fix' inside your `js2-mode'
;; buffer.  This will regenerate the list of goog.require statements by
;; crawling your source code to see which identifiers are being used.
;;
;; If you want the magic to happen automatically each time you save the suffer,
;; then add the following to your .emacs file:
;;
;;   (eval-after-load 'js2-mode
;;     '(add-hook 'before-save-hook 'js2-closure-save-hook))
;;
;; Alternatively, you can use a key binding as follows:
;;
;;   (eval-after-load 'js2-mode
;;     '(define-key js2-mode-map (kbd "C-c C-c") 'js2-closure-fix))

;;; Notes:
;;
;; This tool was written under the assumption that you're following Google's
;; JavaScript style guide (http://goo.gl/Ny5WxZ). See: http://goo.gl/

;;; Commentary:
;;
;; I hope this software makes your life happier <3

;;; Code:

(require 'js2-mode)

(defcustom js2-closure-remove-unused t
  "Determines if unused goog.require statements should be auto-removed.
You might want to consider using `js2-closure-whitelist' rather than
disabling this feature."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-closure-whitelist
  '("goog.testing.asserts")
  "List of goog.require namespaces that should never be removed."
  :type '(repeat string)
  :group 'js2-mode)

(defcustom js2-closure-provides-file
  (concat user-emacs-directory "js2-closure-provides.el")
  "Filename of generated elisp file listing all provided namespaces."
  :type 'file
  :group 'js2-mode)

(defvar js2-closure-provides nil
  "Hierarchy of all closure provided namespaces.")

(defun js2-closure--make-tree (list)
  "Turn a sorted LIST of identifiers into a tree."
  (let (result)
    (while list
      (let (sublist
            (name (caar list))
            (is-leaf (null (cdar list))))
        (while (eq name (caar list))
          (let ((item (pop list)))
            (when (cdr item)
              (push (cdr item) sublist))))
        (let ((subtree (js2-closure--make-tree (nreverse sublist))))
          (push (cons name (cons is-leaf subtree)) result))))
    (nreverse result)))

(defun js2-closure--member-tree (identifier tree)
  "Return t if IDENTIFIER is a member of TREE."
  (let ((branch (assq (car identifier) tree)))
    (if (and branch (cdr identifier))
        (js2-closure--member-tree (cdr identifier) (cddr branch))
      (cadr branch))))

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
         (mapcar 'intern (split-string node "\\.")))))

(defun js2-closure--identifier-to-string (identifier)
  "Convert IDENTIFIER into a dotted string."
  (mapconcat 'symbol-name identifier "."))

(defun js2-closure--crawl (ast on-call on-identifier)
  "Crawl `js2-mode' AST and invoke callbacks on nodes.

ON-CALL will be invoked for all `js2-call-node' nodes, passing
the node itself as the first argument.

ON-IDENTIFIER is invoked for all identifiers, passing as an
argument the last `js2-prop-get-node' in the chain of labels
making up that identifier."
  (let (last)
    (js2-visit-ast
     ast
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

(defun js2-closure--determine-requires (ast)
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
                          (push item provides))))
                     ((and (equal funk '(goog require))
                           (js2-string-node-p arg1))
                      (let ((item (js2-closure--make-identifier
                                   (js2-string-node-value arg1))))
                        (when (not (member item requires))
                          (push item requires))))))))
          (on-identifier
           (lambda (node)
             (let ((item (js2-closure--make-identifier node)))
               (while item
                 (cond ((member item provides)
                        (setq item nil))
                       ((member item requires)
                        (when (not (member item references))
                          (push item references))
                        (setq item nil))
                       ((js2-closure--member-tree item js2-closure-provides)
                        (when (not (member item references))
                          (push item references))
                        (setq item nil)))
                 (setq item (butlast item)))))))
      (js2-closure--crawl ast on-call on-identifier))
    (sort (let (result)
            (dolist (item requires)
              (when (or (not js2-closure-remove-unused)
                        (member (js2-closure--identifier-to-string item)
                                js2-closure-whitelist)
                        (member item references))
                (let ((namespace (js2-closure--identifier-to-string item)))
                  (push namespace result))))
            (dolist (item references result)
              (when (member item references)
                (let ((namespace (js2-closure--identifier-to-string item)))
                  (when (not (member namespace result))
                    (push namespace result))))))
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
      (insert (format "goog.require('%s');\n" (pop namespaces))))))

;;;###autoload
(defun js2-closure-fix ()
  "Fix the goog.require statements for the current buffer."
  (interactive)
  (unless js2-closure-provides
    (js2-closure-reload))
  (js2-closure--replace-closure-requires
   (js2-closure--determine-requires js2-mode-ast)))

;;;###autoload
(defun js2-closure-reload ()
  "Load precomputed list of provided namespaces into memory."
  (interactive)
  (load js2-closure-provides-file)
  (setq js2-closure-provides (js2-closure--make-tree js2-closure-provides)))

;;;###autoload
(defun js2-closure-save-hook ()
  "Global save hook to invoke `js2-closure-fix' if in `js2-mode'."
  (interactive)
  (when (eq major-mode 'js2-mode)
    (js2-closure-fix)))

(provide 'js2-closure)

;;; js2-closure.el ends here
