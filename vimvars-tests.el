;;; vimvars-tests.el --- Tests for vimvars.el

;; Copyright (C) 2011,2025 Free Software Foundation, Inc.

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

(ert-deftest vimvars-test-modeline-accept-vim-vi-bol ()
  "Check we accept 'vim:' or 'vi:' at beginning of line.

   VIM does this for compatibility with VIM version 3.0."
  ;; Verified by comparison with VIM 9.0, Jul 16 2025.
  (should (vimvars--accept-tag "" "vim"))
  (should (vimvars--accept-tag "" "vi")))

(ert-deftest vimvars-test-modeline-ignore-ex-bol ()
  "Check we do not accept 'ex:' at beginning of line."
  ;; Verified by comparison with VIM 9.0, Jul 16 2025.
  (should (not (vimvars--accept-tag "" "ex"))))

(ert-deftest vimvars-test-modeline-accept-vim-vi-ex-not-bol ()
  "Check we accept 'vim:', 'vi' and 'ex:' after the beginning of line"
  ;; Verified by comparison with VIM 9.0, Jul 16 2025.
  (should (vimvars--accept-tag " " "vim"))
  (should (vimvars--accept-tag " " "vi"))
  (should (vimvars--accept-tag " " "ex"))
  (should (vimvars--accept-tag "\t" "vim"))
  (should (vimvars--accept-tag "\t" "vi"))
  (should (vimvars--accept-tag "\t" "ex")))

(ert-deftest vimvars-test-modeline-ignore-foo-bol ()
  "Check we do not accept 'foo:' at beginning of line (as a modeline)."
  ;; Verified by comparison with VIM 9.0, Jul 16 2025.
  (should (not (vimvars--accept-tag "" "foo")))
  (should (not (vimvars--accept-tag " " "foo"))))

(ert-deftest vimvars-test-vimvars-enabled-nil ()
  "Verify that we obey vimvars-enabled when it is nil"
  (let ((vimvars-enabled nil)
        (file-local-variables-alist nil)
        (vimvars-ignore-mode-line-if-local-variables-exist nil))
    (should (not (vimvars--should-obey-modeline)))))

(ert-deftest vimvars-test-no-local-variables ()
  "Verify that we obey vi mode lines in the absence of local variables"
  (let ((vimvars-enabled t)
        (file-local-variables-alist nil)
        (vimvars-ignore-mode-line-if-local-variables-exist nil))
    (should (vimvars--should-obey-modeline))))

(ert-deftest vimvars-test-with-local-variables ()
  "Verify that we ignore vi mode lines if there are local variables"
  (let ((vimvars-enabled t)
        (file-local-variables-alist '((tab-width . 8)))
        (vimvars-ignore-mode-line-if-local-variables-exist t))
    (should (not (vimvars--should-obey-modeline)))))


(ert-deftest vimvars-test-with-both ()
  "Verify that we use both vi mode lines and local variables if vimvars-ignore-mode-line-if-local-variables-exist is nil"
  (let ((vimvars-enabled t)
        (file-local-variables-alist '((tab-width . 8)))
        (vimvars-ignore-mode-line-if-local-variables-exist nil))
    (should (vimvars--should-obey-modeline))))

(ert-deftest vimvars-test-expansions ()
  "Check expansion of ro, sts, sw, ts, tw"
  (should (equal (vimvars--expand-option-name "ro") "readonly"))
  (should (equal (vimvars--expand-option-name "sts") "softtabstop"))
  (should (equal (vimvars--expand-option-name "sw") "shiftwidth"))
  (should (equal (vimvars--expand-option-name "ts") "tabstop"))
  (should (equal (vimvars--expand-option-name "tw") "textwidth")))

(ert-deftest vimvars-test-nonexpansions ()
  "Check we don't 'expand' something that's not an abbreviation"
  (should (equal (vimvars--expand-option-name "blehbleh") "blehbleh")))

(defun kill-buffers-visiting (filename)
  (while
      (let (existing (get-file-buffer filename))
           (when existing (kill-buffer existing)))))

(defmacro vimvars-tests--run-checks-for-file-body (suffix content &rest checks)
  "Write a temporary file whose name has suffix SUFFIX, having content CONTENT.
Visit it with find-file, and evaluate CHECKS.

Returns the result of the final item in CHECKS."
  (let ((fname (make-symbol "filename")))
    `(let ((,fname (make-temp-file "foo" nil ,suffix))
	   (old-vimvars-check-lines vimvars-check-lines)
	   (old-default-fill-column (default-value 'fill-column))
	   (old-tab-width (default-value 'tab-width))
	   (old-find-file-hooks find-file-hooks))
       (unwind-protect
              (progn
		;; Set up a standard testing environment (choosing
		;; unusual values so that it's obvious when there is a
		;; problem).
		(setq-default fill-column 40
			      tab-width 14)
		(setq vimvars-check-lines 5)
		;; Guidance for ERT (in "Tests and Their Environment")
		;; is to avoid using find-file in tests, so we should
		;; find a way to perform these tests without using it.
		(add-hook 'find-file-hook 'vimvars-obey-vim-modeline)
                (with-temp-file ,fname (insert ,content))
                (kill-buffers-visiting ,fname)
                (find-file ,fname)
                ,@checks)
            (progn
              ;; Kill the buffer and delete the temporary file.
              (kill-buffer)
              (delete-file ,fname)
	      ;; Restore defaults etc.
	      (setq-default fill-column old-default-fill-column
			    tab-width old-tab-width)
	      (setq vimvars-check-lines old-vimvars-check-lines
		    find-file-hooks old-find-file-hooks))))))

(defmacro vimvars-tests--run-checks-for-text-file (content &rest checks)
  "Calls vimvars-tests--run-checks-for-file-body with SUFFIX set to .txt."
  `(vimvars-tests--run-checks-for-file-body ".txt" ,content ,@checks))

(ert-deftest vimvars-test-sw-2 ()
  "Verify 'set sw=2' works in C source files."
  (vimvars-tests--run-checks-for-file-body
   ".c"
   "/* This is a C source file.\n vim: set sw=2 :\n*/\n"
   (should (equal c-basic-offset 2))))

;; Do a different check with a distinct c-basic-offset so that
;; we can tell for sure we're actually changing it.
(ert-deftest vimvars-test-sw-4 ()
  "Verify 'set sw=4' works in C source files."
  (vimvars-tests--run-checks-for-file-body
   ".c"
   "/* This is a C source file.\n vim: set sw=4 :\n*/\n"
   (should (equal c-basic-offset 4))))

(ert-deftest vimvars-test-disable ()
  "Check that setting vimvars-enabled to nil turns vimvars-obey-vim-modeline off."
  (let ((vimvars-enabled nil))
    (vimvars-tests--run-checks-for-text-file
     "This is a text file.\n# vim: set ts=18 :\n"
     (should (equal tab-width 14)))))

(ert-deftest vimvars-test-basic-success-case ()
    "Check that even one item actually works."
    (vimvars-tests--run-checks-for-text-file
     "# vi: set ts=18 :\n"
     (should (equal tab-width 18))))

(ert-deftest vimvars-test-require-trailing-colon ()
  "Check that we ignore modelines with no trailing colon."
  (vimvars-tests--run-checks-for-text-file
   "This is a text file.\n# vim: set ts=18 \n"
   (should (equal tab-width 14)))) ; i.e. unchanged from default.

(ert-deftest test-reject-ex-at-start ()
  "Check ex: at the start of the line is rejected."
  (vimvars-tests--run-checks-for-text-file
   "ex: set ts=18 :\n"
   (should (equal tab-width 14))))

(ert-deftest vimvars-test-modeline-too-far-from-top ()
  "Check we ignore mode lines more than `vimvars-check-lines' into the file."
  (vimvars-tests--run-checks-for-text-file
   "Mode lines at line 6 should be ignored\n\n\n\n\n# vim: set ts=6 :\n\n\n\n\n\n\n"
   (should (equal tab-width 14)))

  (vimvars-tests--run-checks-for-text-file
   "Mode lines at line 5 should be accepted.\n\n\n\n# vim: set ts=10 :\n"
   (should (equal tab-width 10))))

(ert-deftest vimvars-test-modeline-too-far-from-bottom ()
  "Check we only accept mode lines within `vimvars-check-lines' of EOF."
  (vimvars-tests--run-checks-for-text-file
   "Mode lines 5 lines from EOF should be accepted.\n\n\n\n\n\n
# vim: set ts=10 :\n\n\n\n\n"
   (should (equal tab-width 10)))

  (vimvars-tests--run-checks-for-text-file
   "Mode lines 6 lines from EOF should be ignored.\n\n\n\n\n\n
# vim: set ts=10 :\n\n\n\n\n\n\n"
   (should (equal tab-width 14))))

(ert-deftest vimvars-test-makeprg ()
  "Check makeprg=blah works."
  (vimvars-tests--run-checks-for-text-file
   "This is a text file.\n# vim: set makeprg=gmake :\n"
   (should (equal compile-command "gmake"))))


(defun vimvars-test-check-recognise-modeline (description modeline)
  (let ((actual-modeline (concat modeline "\n")))
    (vimvars-tests--run-checks-for-text-file
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

(defmacro vimvars-tests--with-temp-default (varname temp-value &rest body)
  "Execute BODY with the default value of VAR set to VALUE.

The value of the final expression in BODY is returned."
  (let ((oldval-var (make-symbol "previous-default")))
    `(let ((,oldval-var ,varname))
       (setq-default ,varname ,temp-value)
       (unwind-protect
              (progn ,@body)
            (setq-default ,varname ,oldval-var)))))

(ert-deftest vimvars-test-obey-set-noignorecase ()
  (vimvars-tests--with-temp-default
   case-fold-search t
   (vimvars-tests--run-checks-for-text-file
    "This is a text file.\n# vim: set noignorecase :\n"
    (should (not case-fold-search)))))

(ert-deftest vimvars-test-obey-set-ignorecase ()
  (vimvars-tests--with-temp-default
   case-fold-search nil
   (vimvars-tests--run-checks-for-text-file
    "This is a text file.\n# vim: set ignorecase :\n"
    (should case-fold-search))))

(ert-deftest vimvars-test-obey-set-wrap ()
  (vimvars-tests--with-temp-default
   truncate-lines t
   (vimvars-tests--run-checks-for-text-file
    "This is a text file.\n# vim: set wrap :\n"
    (should (not truncate-lines)))))

(ert-deftest vimvars-test-obey-set-nowrap ()
  (vimvars-tests--with-temp-default
   truncate-lines nil
   (vimvars-tests--run-checks-for-text-file
    "This is a text file.\n# vim: set nowrap :\n"
    (should truncate-lines))))

(ert-deftest vimvars-test-obey-set-textwidth-87 ()
  (vimvars-tests--run-checks-for-text-file
   "This is a text file.\n# vim: set tw=87 :\n"
   (should (equal tab-width 14))
   (should (equal fill-column 87))))

(ert-deftest vimvars-test-ts-18 ()
  (vimvars-tests--run-checks-for-text-file
   "This is a text file.\n# vim: set ts=18 :\n"
   (should (equal tab-width 18))
   (should (equal fill-column 40))))

(ert-deftest vimvars-test-set-readonly ()
  ;; Verified by comparison with VIM 9.0, Jul 16 2025.
  (vimvars-tests--run-checks-for-text-file
   "This is a text file.\n# vim: set readonly :\n"
   (should buffer-read-only)))

(ert-deftest vimvars-test-set-noreadonly ()
  "This test doesn't really do anything useful, since
  not read-only is the default anyway."
  (vimvars-tests--run-checks-for-text-file
   "This is a text file.\n# vim: set noreadonly :\n"
   (should (not buffer-read-only))))

(ert-deftest vimvars-test-set-write ()
  (vimvars-tests--run-checks-for-text-file
   "This is a text file.\n# vim: set write :\n"
   (should (not buffer-read-only))))

(ert-deftest vimvars-test-set-nowrite ()
  (vimvars-tests--run-checks-for-text-file
   "This is a text file.\n# vim: set nowrite :\n"
   (should buffer-read-only)))

(ert-deftest vimvars-test-ts-19 ()
  (vimvars-tests--run-checks-for-text-file
   "This is a text file.\n# vim: set ts=19 :\n"
   (should (equal tab-width 19))
   (should (equal fill-column 40))))

(ert-deftest vimvars-test-expandtab ()
  (vimvars-tests--with-temp-default
   indent-tabs-mode t
   (vimvars-tests--run-checks-for-text-file
    "# vim: set expandtab :\n"
    (should (not indent-tabs-mode)))))

(ert-deftest vimvars-test-noexpandtab ()
  (vimvars-tests--with-temp-default
   indent-tabs-mode t
   (vimvars-tests--run-checks-for-text-file
    "# vim: set noexpandtab :\n"
    (should indent-tabs-mode))))

(ert-deftest vimvars-test-ignore-mode-line-when-local-variables ()
  "Check that we ignore vim modelines when `vimvars-ignore-mode-line-if-local-variables-exist'."
  (let
      ((old-vimvars-ignore-mode-line-if-local-variables-exist
	vimvars-ignore-mode-line-if-local-variables-exist)
       (filebody       "# vim: set ts=18 :
Some random text in the middle of the file.

Local Variables:
tab-width: 11
fill-column: 59
End:
"))
    (unwind-protect
	(progn
	  (setq vimvars-ignore-mode-line-if-local-variables-exist t)
	  (vimvars-tests--run-checks-for-text-file
	   filebody
	   ;; Because the mode line was ignored, the tab width and
	   ;; fill column should have the settings from the Emacs
	   ;; local variables.
	   (should (equal tab-width 11))
	   (should (equal fill-column 59))))
      (progn
	(setq vimvars-ignore-mode-line-if-local-variables-exist old-vimvars-ignore-mode-line-if-local-variables-exist)) )))

(ert-deftest vimvars-test-no-ignore-mode-line-when-local-variables ()
  "Check that we ignore vim modelines when `vimvars-ignore-mode-line-if-local-variables-exist' is nil."
  (let
      ((old-vimvars-ignore-mode-line-if-local-variables-exist
	vimvars-ignore-mode-line-if-local-variables-exist)
       (filebody       "# vim: set ts=18 :
Some random text in the middle of the file.

Local Variables:
tab-width: 11
fill-column: 59
End:
"))
    (unwind-protect
	(progn
	  (setq vimvars-ignore-mode-line-if-local-variables-exist nil)
	  (vimvars-tests--run-checks-for-text-file
	   filebody
	   ;; Because the mode line was ignored, the tab width and
	   ;; fill column should have the settings from the Emacs
	   ;; local variables.
	   (should (equal tab-width 18))
	   (should (equal fill-column 59))))
      (progn
	(setq vimvars-ignore-mode-line-if-local-variables-exist old-vimvars-ignore-mode-line-if-local-variables-exist)) )))
