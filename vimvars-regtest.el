;;; vimvars-regtest.el --- Regression tests for vimvars.el

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: James Youngman <youngman@google.com>
;; Maintainer: James Youngman <youngman@google.com>

;; vimvars is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; vimvars is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with vimvars.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl)
(require 'cc-mode)

(defun kill-buffers-visiting (filename)
  (while
      (let (existing (get-file-buffer filename))
           (when existing (kill-buffer existing)))))

(defmacro run-checks-for-file-body (suffix content &rest checks)
  "Write a temporary file whose name has suffix SUFFIX, having content CONTENT.
Visit it with find-file, and evaluate CHECKS.

Returns the result of the final item in CHECKS."
  (let ((fname (make-symbol "filename")))
    `(let ((,fname (make-temp-file "foo" nil ,suffix)))
       (unwind-protect
              (progn
                (with-temp-file ,fname (insert ,content))
                (kill-buffers-visiting ,fname)
                (find-file ,fname)
                ,@checks)
            (progn
              ;; Kill the buffer and delete the temporary file.
              (kill-buffer)
              (delete-file ,fname))))))


(defmacro run-checks-for-text-file (content &rest checks)
  "Calls run-checks-for-file-body with SUFFIX set to .txt."
  `(run-checks-for-file-body ".txt" ,content ,@checks))


(defmacro check (name description &rest body)
  "Run a test case, verbosely.
Evaluate BODY, printing progress messages.   Returns nil if an error occurred."
  (declare (debug t) (indent 1))
  (let ((error-var (make-symbol "errordetail")))
    `(condition-case ,error-var
         (progn
           (princ (format "Running check %s (%s)" ,name ,description))
           (let ((test-name ,name))
             ,@body)
           (princ ": passed.\n")
           t)
       (cl-assertion-failed
        (princ (message "FAIL: Test %s (%s) FAILED: %s\n"
                        ,name ,description error-var))
        nil))))


(defun assert-equal (varname expected-value)
  (let* ((value (symbol-value varname))
         (failmsg (format "Expected %s to be %s, but it is %s"
                          varname expected-value value)))
    (assert (equal value expected-value) t failmsg)))


(defun assert-true (varname)
  (let* ((value (symbol-value varname))
         (failmsg (format "Expected %s to be true, but it is nil" varname)))
    (assert (not (not value)) t failmsg)))


(defun assert-false (varname)
  (let* ((value (symbol-value varname))
         (failmsg (format "Expected %s to be nil, but it is %s"
                          varname value)))
    (assert (not value) t failmsg)))


(defmacro with-temp-default (varname temp-value &rest body)
  "Execute BODY with the default value of VAR set to VALUE.

The value of the final expression in BODY is returned."
  (let ((oldval-var (make-symbol "previous-default")))
    `(let ((,oldval-var ,varname))
       (setq-default ,varname ,temp-value)
       (unwind-protect
              (progn ,@body)
            (setq-default ,varname ,oldval-var)))))


(defmacro run-tests (description &rest tests)
  "Run the regression tests"
  `(let
      ((old-tab-width (default-value 'tab-width))
       (old-fill-column (default-value 'fill-column))
       (old-find-file-hook find-file-hook)
       (old-vimvars-check-lines vimvars-check-lines))
    ;; Set up a standard testing environment.
    (setq-default tab-width 14
               fill-column 40)
    (setq find-file-hook '(vimvars-obey-vim-modeline)
          vimvars-check-lines 5)
    ;; Run the tests and restore the old environment.
    (princ (format "*** Test Case \"%s\"\n" ,description))
    (unwind-protect
        (prog1
            ,@tests
          (princ (format "Test case \"%s\" is complete.\n\n" ,description))
    (setq-default tab-wdith old-tab-width
               fill-column old-fill-column)
    (setq find-file-hook old-find-file-hook
          vimvars-check-lines old-vimvars-check-lines)))))


(defmacro define-test-suite (name docstring &rest body)
  `(defun ,name () ,docstring
     (run-tests
      ,docstring
      ,@body)))

(define-test-suite unit-test-accept-tag
  "Unit test for vimvars-accept-tag"

  (check "test-vim-bol"
    "Check we accept 'vim:' at beginning of line"
    (assert (vimvars-accept-tag "" "vim")))

  (check "test-vim-not-bol"
    "Check we accept 'vim:' after the beginning of line"
    (assert (vimvars-accept-tag " " "vim")))

  (check "test-vi-bol"
    "Check we accept 'vi:' at beginning of line"
    (assert (vimvars-accept-tag "" "vi")))

  (check "test-vi-not-bol"
    "Check we accept 'vi:' after the beginning of line"
    (assert (vimvars-accept-tag " " "vi")))

  (check "test-ex-bol"
    "Check we accept 'ex:' at beginning of line"
    (assert (not (vimvars-accept-tag "" "ex"))))

  (check "test-ex-not-bol"
    "Check we accept 'ex:' after the beginning of line"
    (assert (vimvars-accept-tag " " "ex")))

  (check "test-foo-bol"
    "Check we do not accept 'foo:' at beginning of line"
    (assert (not (vimvars-accept-tag "" "foo"))))

  (check "test-foo-not-bol"
    "Check we do not accept 'foo:' after beginning of line"
    (assert (not (vimvars-accept-tag " " "foo")))))


(define-test-suite unit-test-vimvars-should-obey-modeline
  "Unit test for vimvars-should-obey-modeline"

  (check "test-vimvars-enabled-nil"
    "Verify that we obey vimvars-enabled when it is nil"
    (let ((vimvars-enabled nil)
          (file-local-variables-alist nil)
          (vimvars-ignore-mode-line-if-local-variables-exist nil))
      (assert (not (vimvars-should-obey-modeline))
                   t "case 1")))

  (check "test-no-local-variables"
    "Verify that we obey vi mode lines in the absence of local variables"
    (let ((vimvars-enabled t)
          (file-local-variables-alist nil)
          (vimvars-ignore-mode-line-if-local-variables-exist nil))
      (assert (vimvars-should-obey-modeline)
              t "case 2")))

  (check "test-with-local-variables"
    "Verify that we ignore vi mode lines if there are local variables"
    (let ((vimvars-enabled t)
          (file-local-variables-alist '((tab-width . 8)))
          (vimvars-ignore-mode-line-if-local-variables-exist t))
      (assert (not (vimvars-should-obey-modeline))
              t "case 3")))

  (check "test-with-both"
    "Verify that we use both vi mode lines and local variables if vimvars-ignore-mode-line-if-local-variables-exist is nil"
    (let ((vimvars-enabled t)
          (file-local-variables-alist '((tab-width . 8)))
          (vimvars-ignore-mode-line-if-local-variables-exist nil))
      (assert (vimvars-should-obey-modeline)
              t "case 4"))))


(define-test-suite unit-test-vimvars-expand-option-name
  "Unit test for vimvars-expand-option-name"

  (check "test-expansions"
    "Check expansion of ro, sts, sw, ts, tw"
    (assert (equal (vimvars-expand-option-name "ro") "readonly"))
    (assert (equal (vimvars-expand-option-name "sts") "softtabstop"))
    (assert (equal (vimvars-expand-option-name "sw") "shiftwidth"))
    (assert (equal (vimvars-expand-option-name "ts") "tabstop"))
    (assert (equal (vimvars-expand-option-name "tw") "textwidth")))

  (check "test-nonexpansions"
    "Check we don't 'expand' something that's not an abbreviation"
    (assert (equal (vimvars-expand-option-name "blehbleh") "blehbleh"))))


(define-test-suite test-basics
  "Basic tests"

  (check "test-disable"
    "Check that setting vimvars-enabled to nil turns vimvars-obey-vim-modeline off."
    (let ((vimvars-enabled nil))
      (run-checks-for-text-file
       "This is a text file.\n# vim: set ts=18 :\n"
       (assert-equal 'tab-width 14))))

  (check "test-basic-success-case"
    "Check that even one item actually works."
    (run-checks-for-text-file
     "# vi: set ts=18 :\n"
     (assert-equal 'tab-width 18))))


(define-test-suite test-modeline-format
  "modeline format"

  (defun check-recognise-modeline (description modeline)
    (check
        description
      (concat "Verify that we recognise the modeline " modeline)
      (let ((actual-modeline (concat modeline "\n")))
        (run-checks-for-text-file
         actual-modeline
         (assert-equal 'tab-width 18)))))

  (check-recognise-modeline "test-accept-vim-set"
                            "# vim: set ts=18 :\n")

  (check-recognise-modeline "test-accept-vi-set"
                            "# vi: set ts=18 :\n")

  (check-recognise-modeline "test-accept-ex-set"
                            "# ex: set ts=18 :\n")

  (check-recognise-modeline "test-accept-vim-se"
                            "# vim: se ts=18 :\n")

  (check-recognise-modeline "test-accept-vi-se"
                            "# vi: se ts=18 :\n")

  (check-recognise-modeline "test-accept-ex-se"
                            "# ex: se ts=18 :\n")

  (check-recognise-modeline "test-accept-vim-setlocal"
                            "# vim: setlocal ts=18 :\n")

  (check-recognise-modeline "test-accept-vi-setlocal"
                            "# vi: setlocal ts=18 :\n")

  (check-recognise-modeline "test-accept-ex-setlocal"
                            "# ex: setlocal ts=18 :\n")

  (check "test-require-trailing-colon"
    "Check that we ignore modelines with no trailing colon."
    (run-checks-for-text-file
     "This is a text file.\n# vim: set ts=18 \n"
     (assert-equal 'tab-width 14))) ; i.e. unchanged from default.

  ;; Check modelines work even without a space before the final colon.
  (check-recognise-modeline "test-accept-no-space-before-final-colon"
                            "# vi: set ts=18:\n")

  ;; Check extra spaces before vi: are accepted
  (check-recognise-modeline "test-accept-extra-spaces"
                            "#   vi: set ts=18 :\n")

  ;; Check tabs before vi: are accepted.
  (check-recognise-modeline "test-accept-tab"
                            "#\tvi: set ts=18 :\n")

  ;; Check vi: at the start of the line is accepted
  (check-recognise-modeline "test-accept-vi-at-start" "vi: set ts=18 :\n")

  (check "test-reject-ex-at-start"
    "Check ex: at the start of the line is rejected."
    (run-checks-for-text-file
     "ex: set ts=18 :\n"
     (assert-equal 'tab-width 14)))

  (check "test-modeline-too-far-from-top"
    "Check we ignore mode lines more than `vimvars-check-lines' into the file."
    (run-checks-for-text-file
     "Mode lines at line 6 should be ignored\n\n\n\n\n# vim: set ts=6 :\n\n\n\n\n\n\n"
     (assert-equal 'tab-width 14))

    (run-checks-for-text-file
     "Mode lines at line 5 should be accepted.\n\n\n\n# vim: set ts=10 :\n"
     (assert-equal 'tab-width 10)))

  (check "test-modeline-too-far-from-bottom"
    "Check we only accept mode lines within `vimvars-check-lines' of EOF."
    (run-checks-for-text-file
     "Mode lines 5 lines from EOF should be accepted.\n\n\n\n\n\n
# vim: set ts=10 :\n\n\n\n\n"
     (assert-equal 'tab-width 10))

    (run-checks-for-text-file
     "Mode lines 6 lines from EOF should be ignored.\n\n\n\n\n\n
# vim: set ts=10 :\n\n\n\n\n\n\n"
     (assert-equal 'tab-width 14))
    ))


(define-test-suite test-misc
  "test makeprg, ignorecase, noignorecase, wrap, nowrap, textwidth"

  (check "makeprg"
    "Check makeprg=blah works."
    (run-checks-for-text-file
     "This is a text file.\n# vim: set makeprg=gmake :\n"
     (assert-equal 'compile-command "gmake")))

  (with-temp-default
   case-fold-search t
   (check "noignorecase"
     "Verify that we obey 'set noignorecase'"
     (run-checks-for-text-file
      "This is a text file.\n# vim: set noignorecase :\n"
      (assert-false 'case-fold-search))))

  (with-temp-default
   case-fold-search nil
   (check "ignorecase"
     "Verify that we obey 'set ignorecase'"
     (run-checks-for-text-file
      "This is a text file.\n# vim: set ignorecase :\n"
      (assert-true 'case-fold-search))))

  (with-temp-default
   truncate-lines t
   (check "wrap"
     "Verify that we obey 'set wrap'"
     (run-checks-for-text-file
      "This is a text file.\n# vim: set wrap :\n"
      (assert-false 'truncate-lines))))

  (with-temp-default
   truncate-lines nil
   (check "nowrap"
     "Verify that we obey 'set nowrap'"
     (run-checks-for-text-file
      "This is a text file.\n# vim: set nowrap :\n"
      (assert-true 'truncate-lines))))

  (check "test-textwidth-87"
     "Verify that we obey 'set tw=N'"
    (run-checks-for-text-file
     "This is a text file.\n# vim: set tw=87 :\n"
     (assert-equal 'tab-width 14)
     (assert-equal 'fill-column 87))))


(define-test-suite test-readonly
  "test readonnly, noreadonly, write, nowrite"

  (check "readonly"
    "Verify that we obey 'set readonly'"
    (run-checks-for-text-file
     "This is a text file.\n# vim: set readonly :\n"
     (assert-true 'buffer-read-only)))

  ;; This test doesn't really do anything useful, since
  ;; not read-only is the default anyway.
  (check "noreadonly"
    "Verify that we obey 'set noreadonly'"
    (run-checks-for-text-file
     "This is a text file.\n# vim: set noreadonly :\n"
     (assert-false 'buffer-read-only)))

  (check "write"
    "Verify that we obey 'set write'"
    (run-checks-for-text-file
     "This is a text file.\n# vim: set write :\n"
     (assert-false 'buffer-read-only)))

  (check "nowrite"
    "Verify that we obey 'set nowrite'"
    (run-checks-for-text-file
     "This is a text file.\n# vim: set nowrite :\n"
     (assert-true 'buffer-read-only))))




(define-test-suite test-shiftwidth
  "Check sw=N works."

  (check "sw-2"
    "Verify 'set sw=2' works in C source files."
    (run-checks-for-file-body
     ".c"
     "/* This is a C source file.\n vim: set sw=2 :\n*/\n"
     (assert-equal 'c-basic-offset 2)))

  ;; Do a different check with a distinct c-basic-offset so that
  ;; we can tell for sure we're actually changing it.
  (check "sw-4"
    "Verify 'set sw=4' works in C source files."
    (run-checks-for-file-body
     ".c"
     "/* This is a C source file.\n vim: set sw=4 :\n*/\n"
     (assert-equal 'c-basic-offset 4))))


(define-test-suite test-tabstop
  "tab stop and tab expansion"
  (check "test-ts-18"
    "Check ts=X works."
    (run-checks-for-text-file
     "This is a text file.\n# vim: set ts=18 :\n"
     (assert-equal 'tab-width 18)
     (assert-equal 'fill-column 40)))

  (check "test-expandtab"
    "Check expandtabs works."
    (with-temp-default
     indent-tabs-mode t
     (run-checks-for-text-file
      "# vim: set expandtab :\n"
      (assert-false 'indent-tabs-mode))))

  (check "test-noexpandtab"
    "Check noexpandtabs works."
    (with-temp-default
     indent-tabs-mode nil
     (run-checks-for-text-file
      "# vim: set noexpandtab :\n"
      (assert-true 'indent-tabs-mode)))))

(define-test-suite test-local-vars-interaction
  "checks for interactions with Emacs local variables"

  (let ((filebody       "# vim: set ts=18 :
Some random text in the middle of the file.

Local Variables:
tab-width: 11
fill-column: 59
End:
"))
    (check "test-ignore-mode-line"
      "Check that we ignore modelines when `vimvars-ignore-mode-line-if-local-variables-exist'."
      (let ((vimvars-ignore-mode-line-if-local-variables-exist t))
        (run-checks-for-text-file
         filebody
         (assert-equal 'tab-width 11)
         (assert-equal 'fill-column 59)))

      (let ((vimvars-ignore-mode-line-if-local-variables-exist nil))
        (run-checks-for-text-file
         filebody
         (assert-equal 'tab-width 18)
         (assert-equal 'fill-column 59))))))


(let
    ((debug-on-error nil))
  (with-output-to-temp-buffer "*Unit Test Results*"
    (unit-test-accept-tag)
    (unit-test-vimvars-should-obey-modeline)
    (unit-test-vimvars-expand-option-name)
    (test-basics)
    (test-modeline-format)
    (test-tabstop)
    (test-misc)
    (test-readonly)
    (test-shiftwidth)
    (test-local-vars-interaction)))
