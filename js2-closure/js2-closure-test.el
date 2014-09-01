;;; js2-closure-test.el --- Unit tests for js2-closure
;;; Commentary:
;;; Code:

(require 'js2-closure)

(defun make-ast (source)
  "Extract `js2-mode' abstract syntax tree from SOURCE."
  (with-temp-buffer
    (insert source)
    (js2-mode)
    (should (null js2-mode-buffer-dirty-p))
    js2-mode-ast))

(ert-deftest make-tree ()
  (should (equal (js2-closure--make-tree
                  '((goog dom)
                    (goog dom classlist)
                    (goog events)
                    (goog events EventManager)
                    (goog events EventTarget)
                    (goog net XhrIo)))
                 '((goog nil . ((dom t . ((classlist t)))
                                (events t . ((EventManager t)
                                             (EventTarget t)))
                                (net nil . ((XhrIo t)))))))))

(ert-deftest member-tree ()
  (let ((tree (js2-closure--make-tree
               '((goog dom)
                 (goog dom classlist)
                 (goog events)
                 (goog events EventManager)
                 (goog events EventTarget)
                 (goog net XhrIo)))))
    (should (js2-closure--member-tree '(goog dom) tree))
    (should (js2-closure--member-tree '(goog dom classlist) tree))
    (should (js2-closure--member-tree '(goog events EventManager) tree))
    (should (js2-closure--member-tree '(goog events EventTarget) tree))
    (should (js2-closure--member-tree '(goog net XhrIo) tree))
    (should (not (js2-closure--member-tree '(goog) tree)))
    (should (not (js2-closure--member-tree '(goog net) tree)))
    (should (not (js2-closure--member-tree '(blob) tree)))
    (should (not (js2-closure--member-tree '(blob) nil)))
    (should (not (js2-closure--member-tree nil tree)))
    (should (not (js2-closure--member-tree nil nil)))))

(ert-deftest determine-requires--empty-buffer--returns-empty ()
  (should (not (js2-closure--determine-requires (make-ast "")))))

(ert-deftest determine-requires--remove-unused ()
  (let ((js2-closure-remove-unused t)
        (ast (make-ast "goog.require('foo');")))
    (should (not (js2-closure--determine-requires ast)))))

(ert-deftest determine-requires--whitelist ()
  (let ((js2-closure-remove-unused t)
        (js2-closure-whitelist '("foo"))
        (ast (make-ast "goog.require('foo');")))
    (should (equal (js2-closure--determine-requires ast)
                   '("foo")))))

(ert-deftest determine-requires--dont-remove-unused ()
  (let ((js2-closure-remove-unused nil)
        (ast (make-ast "goog.require('foo');")))
    (should (equal (js2-closure--determine-requires ast)
                   '("foo")))))

(ert-deftest determine-requires--function-call ()
  (let ((js2-closure-remove-unused t)
        (js2-closure-provides (js2-closure--make-tree
                               '((goog dom))))
        (ast (make-ast "goog.dom.getElement('foo');")))
    (should (equal (js2-closure--determine-requires ast)
                   '("goog.dom")))))

(ert-deftest determine-requires--reference ()
  (let ((js2-closure-remove-unused t)
        (js2-closure-provides (js2-closure--make-tree
                               '((goog dom))))
        (ast (make-ast "foo = goog.dom.getElement;")))
    (should (equal (js2-closure--determine-requires ast)
                   '("goog.dom")))))

(ert-deftest determine-requires--already-required ()
  (let ((js2-closure-remove-unused t)
        (js2-closure-provides (js2-closure--make-tree
                               '((goog dom)
                                 (goog events EventManager))))
        (ast (make-ast "
goog.provide('foo.Bar');
goog.require('goog.dom');
goog.require('goog.events');
foo.Bar = function() {
  goog.dom.getElement('bog');
  new goog.events.EventManager();
};
")))
    (should (equal (js2-closure--determine-requires ast)
                   '("goog.dom"
                     "goog.events.EventManager")))))

(ert-deftest determine-requires--provide-is-ignored ()
  (let ((js2-closure-remove-unused t)
        (js2-closure-provides (js2-closure--make-tree
                               '((foo Bar))))
        (ast (make-ast "
goog.provide('foo.Bar');
new foo.Bar();
")))
    (should (not (js2-closure--determine-requires ast)))))

(ert-deftest determine-requires--dont-include-parent-namespaces ()
  (let ((js2-closure-remove-unused t)
        (js2-closure-provides (js2-closure--make-tree
                               '((goog events)
                                 (goog events EventManager))))
        (ast (make-ast "new goog.events.EventManager();")))
    (should (equal (js2-closure--determine-requires ast)
                   '("goog.events.EventManager")))))

(ert-deftest replace-closure-requires ()
  (let (contents)
    (with-temp-buffer
      (insert "
goog.provide('jart.lol');
goog.provide('jart.lol.Hog');\n
goog.require('black.angel');
goog.require('not.insane');\n\n
jart.lol.funk = function() {};
")
      (js2-closure--replace-closure-requires
       '("a.cat"
         "black.angel"
         "down.with.bill.gates"))
      (setq contents (buffer-substring-no-properties
                      (point-min) (point-max))))
    (should (equal contents "
goog.provide('jart.lol');
goog.provide('jart.lol.Hog');\n
goog.require('a.cat');
goog.require('black.angel');
goog.require('down.with.bill.gates');\n\n
jart.lol.funk = function() {};
"))))

(ert-deftest replace-closure-requires--no-requires--insert-after-provides ()
  (let (contents)
    (with-temp-buffer
      (insert "
goog.provide('jart.lol.Hog');
goog.provide('jart.lol.Mog');\n\n\n
jart.lol.Hog = function() {};
")
      (js2-closure--replace-closure-requires
       '("a.cat"
         "black.angel"
         "down.with.bill.gates"))
      (setq contents (buffer-substring-no-properties
                      (point-min) (point-max))))
    (should (equal contents "
goog.provide('jart.lol.Hog');
goog.provide('jart.lol.Mog');\n
goog.require('a.cat');
goog.require('black.angel');
goog.require('down.with.bill.gates');\n\n\n
jart.lol.Hog = function() {};
"))))

(ert-deftest replace-closure-requires--no-requires-or-provides--fail ()
  :expected-result :failed
  (with-temp-buffer
    (js2-closure--replace-closure-requires '("a.cat"))))

;;; js2-closure-test.el ends here
