(ert-deftest vimvars-test-vim-bol ()
  "Check we accept 'vim:' at beginning of line"
  (should (vimvars-accept-tag "" "vim")))

(ert-deftest vimvars-test-vim-not-bol ()
  "Check we accept 'vim:' after the beginning of line"
  (should (vimvars-accept-tag " " "vim")))

(ert-deftest vimvars-test-vi-bol ()
  "Check we accept 'vi:' at beginning of line"
  (should (vimvars-accept-tag "" "vi")))

(ert-deftest vimvars-test-vi-not-bol ()
  "Check we accept 'vi:' after the beginning of line"
  (should (vimvars-accept-tag " " "vi")))

(ert-deftest vimvars-test-ex-bol ()
  "Check we accept 'ex:' at beginning of line"
  (should (not (vimvars-accept-tag "" "ex"))))

(ert-deftest vimvars-test-ex-not-bol ()
  "Check we accept 'ex:' after the beginning of line"
  (should (vimvars-accept-tag " " "ex")))

(ert-deftest vimvars-test-foo-bol ()
  "Check we do not accept 'foo:' at beginning of line"
  (should (not (vimvars-accept-tag "" "foo"))))

(ert-deftest vimvars-test-foo-not-bol ()
  "Check we do not accept 'foo:' after beginning of line"
  (should (not (vimvars-accept-tag " " "foo"))))

(ert-deftest vimvars-test-vimvars-enabled-nil ()
  "Verify that we obey vimvars-enabled when it is nil"
  (let ((vimvars-enabled nil)
        (file-local-variables-alist nil)
        (vimvars-ignore-mode-line-if-local-variables-exist nil))
    (should (not (vimvars-should-obey-modeline)))))

(ert-deftest vimvars-test-no-local-variables ()
  "Verify that we obey vi mode lines in the absence of local variables"
  (let ((vimvars-enabled t)
        (file-local-variables-alist nil)
        (vimvars-ignore-mode-line-if-local-variables-exist nil))
    (should (vimvars-should-obey-modeline))))

(ert-deftest vimvars-test-with-local-variables ()
  "Verify that we ignore vi mode lines if there are local variables"
  (let ((vimvars-enabled t)
        (file-local-variables-alist '((tab-width . 8)))
        (vimvars-ignore-mode-line-if-local-variables-exist t))
    (should (not (vimvars-should-obey-modeline)))))


(ert-deftest vimvars-test-with-both ()
  "Verify that we use both vi mode lines and local variables if vimvars-ignore-mode-line-if-local-variables-exist is nil"
  (let ((vimvars-enabled t)
        (file-local-variables-alist '((tab-width . 8)))
        (vimvars-ignore-mode-line-if-local-variables-exist nil))
    (should (vimvars-should-obey-modeline))))

(ert-deftest vimvars-test-expansions ()
  "Check expansion of ro, sts, sw, ts, tw"
  (should (equal (vimvars-expand-option-name "ro") "readonly"))
  (should (equal (vimvars-expand-option-name "sts") "softtabstop"))
  (should (equal (vimvars-expand-option-name "sw") "shiftwidth"))
  (should (equal (vimvars-expand-option-name "ts") "tabstop"))
  (should (equal (vimvars-expand-option-name "tw") "textwidth")))

(ert-deftest vimvars-test-nonexpansions ()
  "Check we don't 'expand' something that's not an abbreviation"
  (should (equal (vimvars-expand-option-name "blehbleh") "blehbleh")))

(defun kill-buffers-visiting (filename)
  (while
      (let (existing (get-file-buffer filename))
           (when existing (kill-buffer existing)))))

(defmacro run-checks-for-file-body (suffix content &rest checks)
  "Write a temporary file whose name has suffix SUFFIX, having content CONTENT.
Visit it with find-file, and evaluate CHECKS.

Returns the result of the final item in CHECKS."
  (let ((fname (make-symbol "filename")))
    `(let ((,fname (make-temp-file "foo" nil ,suffix))
	   (old-find-file-hooks find-file-hooks))
       (unwind-protect
              (progn
		;; Guidance for ERT (in "Tests and Their Environment")
		;; is to avoid using find-file in tests, so we should
		;; find a way to perform these tests without using it.
                (with-temp-file ,fname (insert ,content))
		(add-hook 'find-file-hook 'vimvars-obey-vim-modeline)
                (kill-buffers-visiting ,fname)
                (find-file ,fname)
                ,@checks)
            (progn
              ;; Kill the buffer and delete the temporary file.
              (kill-buffer)
              (delete-file ,fname)
	      (setq find-file-hooks old-find-file-hooks))))))

(defmacro run-checks-for-text-file (content &rest checks)
  "Calls run-checks-for-file-body with SUFFIX set to .txt."
  `(run-checks-for-file-body ".txt" ,content ,@checks))

(ert-deftest vimvars-test-sw-2 ()
  "Verify 'set sw=2' works in C source files."
  (run-checks-for-file-body
   ".c"
   "/* This is a C source file.\n vim: set sw=2 :\n*/\n"
   (should (equal c-basic-offset 2))))

;; Do a different check with a distinct c-basic-offset so that
;; we can tell for sure we're actually changing it.
(ert-deftest vimvars-test-sw-4 ()
  "Verify 'set sw=4' works in C source files."
  (message "find-file-hooks is %s" find-file-hooks)
  (run-checks-for-file-body
   ".c"
   "/* This is a C source file.\n vim: set sw=4 :\n*/\n"
   (should (equal c-basic-offset 4))))

(ert-deftest vimvars-test-disable ()
  "Check that setting vimvars-enabled to nil turns vimvars-obey-vim-modeline off."
  (let ((vimvars-enabled nil))
    (run-checks-for-text-file
     "This is a text file.\n# vim: set ts=18 :\n"
     (should (equal tab-width 14)))))

(ert-deftest vimvars-test-basic-success-case ()
    "Check that even one item actually works."
    (run-checks-for-text-file
     "# vi: set ts=18 :\n"
     (should (equal tab-width 18))))

(ert-deftest vimvars-test-require-trailing-colon ()
  "Check that we ignore modelines with no trailing colon."
  (run-checks-for-text-file
   "This is a text file.\n# vim: set ts=18 \n"
   (should (equal tab-width 14)))) ; i.e. unchanged from default.

(ert-deftest test-reject-ex-at-start ()
  "Check ex: at the start of the line is rejected."
  (run-checks-for-text-file
   "ex: set ts=18 :\n"
   (should (equal tab-width 14))))

(ert-deftest vimvars-test-modeline-too-far-from-top ()
  "Check we ignore mode lines more than `vimvars-check-lines' into the file."
  (run-checks-for-text-file
   "Mode lines at line 6 should be ignored\n\n\n\n\n# vim: set ts=6 :\n\n\n\n\n\n\n"
   (should (equal tab-width 14)))

  (run-checks-for-text-file
   "Mode lines at line 5 should be accepted.\n\n\n\n# vim: set ts=10 :\n"
   (assert-equal 'tab-width 10)))

(ert-deftest vimvars-test-modeline-too-far-from-bottom ()
  "Check we only accept mode lines within `vimvars-check-lines' of EOF."
  (run-checks-for-text-file
   "Mode lines 5 lines from EOF should be accepted.\n\n\n\n\n\n
# vim: set ts=10 :\n\n\n\n\n"
   (should (equal tab-width 10)))

  (run-checks-for-text-file
   "Mode lines 6 lines from EOF should be ignored.\n\n\n\n\n\n
# vim: set ts=10 :\n\n\n\n\n\n\n"
   (should (equal tab-width 14))))

(ert-deftest vimvars-test-makeprg ()
  "Check makeprg=blah works."
  (run-checks-for-text-file
   "This is a text file.\n# vim: set makeprg=gmake :\n"
   (should (equal compile-command "gmake"))))


(defun vimvars-test-check-recognise-modeline (description modeline)
  (let ((actual-modeline (concat modeline "\n")))
    (run-checks-for-text-file
     actual-modeline
     (should (equal tab-width 18)))))


(ert-deftest vimvars-test-modeline-accept-vim-set ()
    (vimvars-test-check-recognise-modeline
     "test-accept-vim-set"
     "# vim: set ts=18 :\n"))

(ert-deftest vimvars-test-modeline-accept-vi-set ()
  (vimvars-test-check-recognise-modeline
   "test-accept-vi-set"
   "# vi: set ts=18 :\n"))

(ert-deftest vimvars-test-modeline-accept-ex-set ()
  (vimvars-test-check-recognise-modeline
   "test-accept-ex-set"
   "# ex: set ts=18 :\n"))

(ert-deftest vimvars-test-modeline-vim-se ()
  (vimvars-test-check-recognise-modeline
   "test-accept-vim-se"
   "# vim: se ts=18 :\n"))

(ert-deftest vimvars-test-modeline-accept-vi-se ()
  (vimvars-test-check-recognise-modeline
   "test-accept-vi-se"
   "# vi: se ts=18 :\n"))

(ert-deftest vimvars-test-modeline-accept-ex-se ()
  (vimvars-test-check-recognise-modeline
   "test-accept-ex-se"
   "# ex: se ts=18 :\n"))

(ert-deftest vimvars-test-modeline-accept-vim-setlocal ()
  (vimvars-test-check-recognise-modeline
   "test-accept-vim-setlocal"
   "# vim: setlocal ts=18 :\n"))

(ert-deftest vimvars-test-modeline-accept-vi-setlocal ()
  (vimvars-test-check-recognise-modeline
   "test-accept-vi-setlocal"
   "# vi: setlocal ts=18 :\n"))

(ert-deftest vimvars-test-modeline-accept-ex-setlocal ()
  (vimvars-test-check-recognise-modeline
   "test-accept-ex-setlocal"
   "# ex: setlocal ts=18 :\n"))

(ert-deftest vimvars-test-modeline-accept-no-space-before-final-colon ()
  "Check modelines work even without a space before the final colon."
  (vimvars-test-check-recognise-modeline
   "test-accept-no-space-before-final-colon"
   "# vi: set ts=18:\n"))

(ert-deftest vimvars-test-modeline-accept-extra-spaces ()
  "Check extra spaces before vi: are accepted"
  (vimvars-test-check-recognise-modeline
   "test-accept-extra-spaces"
   "#   vi: set ts=18 :\n"))

(ert-deftest vimvars-test-modeline-accept-tab ()
  "Check tabs before vi: are accepted."
  (vimvars-test-check-recognise-modeline
   "test-accept-tab"
   "#\tvi: set ts=18 :\n"))

(ert-deftest vimvars-test-modeline-accept-vi-at-start ()
  "Check vi: at the start of the line is accepted"
  (vimvars-test-check-recognise-modeline
   "test-accept-vi-at-start"
   "vi: set ts=18 :\n"))
