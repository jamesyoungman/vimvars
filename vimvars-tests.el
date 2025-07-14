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
  (assert (equal (vimvars-expand-option-name "blehbleh") "blehbleh")))
