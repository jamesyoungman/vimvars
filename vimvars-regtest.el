;;; vimvars-regtest.el --- Regression tests for vimvars.el 

;; Copyright (C) 2010 James Youngman.

;; Author: James Youngman <james@youngman.org>
;; Maintainer: James Youngman

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
	   (message "Running test %s (%s)..." ,name ,description)
	   ,@body
	   (message "Test %s passed." ,name)
	   t)
       (cl-assertion-failed 
	(message "Test %s FAILED: %s" ,name error-var)
	nil))))

(defun assert-equal (varname expected-value)
  (let* ((value (symbol-value varname))
	(failmsg (format "Expected %s to be %d, but it is %d"
			 varname expected-value value)))
    (assert (equal value expected-value) t failmsg)))
	    

(defmacro with-temp-default (varname temp-value &rest body)
  "Execute BODY with the default value of VAR set to VALUE.

The value of the final expression in BODY is returned."
  (let ((oldval-var (make-symbol "previous-default")))
    `(let ((,oldval-var ,varname))
       (setq-default ,varname temp-value
       (unwind-protect
	   (progn body)
	 (setq-default ,varname ,oldval-var))))))
	   
  
(defun tests ()
  "Regression tests for vimvars."

  (check "test-require-trailing-colon" 
    "Check that we ignore modelines with no trailing colon."
    (run-checks-for-text-file 
     "This is a text file.\n# vim: set ts=18 \n" 
     (assert-equal 'tab-width 14)
     (assert-equal 'fill-column 40)))
  
  (check "test-ts-18" 
    "Check ts=X works."
    (run-checks-for-text-file 
     "This is a text file.\n# vim: set ts=18 :\n" 
     (assert-equal 'tab-width 18)
     (assert-equal 'fill-column 40)))
  
  (check "test-disable" 
    "Check that setting vimvars-enabled to nil turns vimvars-obey-vim-modeline off."
    (let ((vimvars-enabled nil))
      (run-checks-for-text-file
       "This is a text file.\n# vim: set ts=18 :\n" 
       (assert-equal 'tab-width 14))))

  (check "test-modeline-too-far" 
    "Check we ignore mode lines more than `vimvars-check-lines' into the file."
    (run-checks-for-text-file
     "Mode lines at line 6 should be ignored\n\n\n\n\n# vim: set ts=6 :\n" 
     (assert-equal 'tab-width 14))
  
    (run-checks-for-text-file
     "Mode lines at line 5 should be accepted.\n\n\n\n# vim: set ts=10 :\n" 
     (assert-equal 'tab-width 10)))
  
  (check "test-accept-ex-modeline" 
    "Check ex modelines work."
    (run-checks-for-text-file
     "# ex: set ts=18 :\n" 
     (assert-equal 'tab-width 18)))
  
  (check "test-accept-vi-modeline" 
    "Check vi (as opposed to vim) modelines work."
    (run-checks-for-text-file
     "# vi: set ts=18 :\n" 
     (assert-equal 'tab-width 18)))
  
  (check "test-tw-87" 
    "Check tw=X works."
    (run-checks-for-text-file
     "This is a text file.\n# vim: set tw=87 :\n" 
     (assert-equal 'tab-width 14)
     (assert-equal 'fill-column 87)))
  
)
  


(defun run-all-tests ()
  "Run the vimvars regression tests"
  (let
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
    (unwind-protect
        (prog1 (tests) (message "All tests for vimvars are done.")))
    (setq-default tab-wdith old-tab-width
	    fill-column old-fill-column)
    (setq find-file-hook old-find-file-hook
          vimvars-check-lines old-vimvars-check-lines)))


(run-all-tests)
