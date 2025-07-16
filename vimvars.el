;;; vimvars.el --- Emacs support for VI modelines

;; Copyright (C) 2010,2011,2025 Free Software Foundation, Inc.

;; Author: James Youngman <youngman@google.com>
;; Maintainer: James Youngman <youngman@google.com>
;; Keywords: local-variables, vi, vim, emulations

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

;;; Commentary:

;; Defines a function `vimvars-obey-vim-modeline' which is suitable for
;; a hook which checks for a VI-style in the current buffer and sets
;; various Emacs buffer-local variables accordingly.

;;; Code:

(defgroup vimvars nil
  "Support for VIM mode lines."
  :group 'find-file)


(defcustom vimvars-enabled t
  "If nil, VIM mode lines will be ignored."
  :type 'boolean
  :group 'vimvars)
(make-variable-buffer-local 'vimvars-enabled)


(defcustom vimvars-check-lines 5
  "The number of lines in the top or bottom of a file that we will search for VIM settings (VIM itself checks 5)."
  :type 'integer
  :group 'vimvars)


(defcustom vimvars-ignore-mode-line-if-local-variables-exist t
  "If non-nil, VIM mode lines are ignored in files that have Emacs local variables."
  :type 'boolean
  :group 'vimvars)

;; It appears that real VIM accepts backslash-escaped characters (for
;; example \\| inside makeprg).
;;
;; Also, VIM accepts vi: and vim: at start-of line (but not ex:)
(defconst vimvars-modeline-re
  "\\(^\\|[ \t]\\)\\(ex\\|vim?\\):[\t ]?\\(set\\|setlocal\\|se\\)? \\([^:]+\\):"
  "Regex matching a VIM modeline.")

(defun vimvars-obey-vim-modeline ()
  "Check the top and bottom of a file for VIM-style settings, and obey them.
Only the first and last `vimvars-check-lines' lines of the file
are checked for VIM variables.   You can use this in `find-file-hook'."
  (when (vimvars--should-obey-modeline)
    (save-excursion
      (or (vimvars--obey-top-modeline)
          (vimvars--obey-bottom-modeline)))))

;;; Implementation (you probably don't need to call these functions
;;; directly).

(defun vimvars--should-obey-modeline ()
  "Return non-nil if a VIM modeline should be obeyed in this file."
  ;; Always return nil if vimvars-enabled is nil.
  ;; Otherwise, if there are Emacs local variables for this file,
  ;; return nil unless vimvars-ignore-mode-line-if-local-variables-exist
  ;; is also nil.
  (when vimvars-enabled
    (if file-local-variables-alist
        (not vimvars-ignore-mode-line-if-local-variables-exist)
      t)))


(defun vimvars--accept-tag (leader tag)
  "Return non-nil if LEADER followed by TAG should be accepted as a modeline."
  (cond
   ((equal "vim" tag) t)
   ((equal "vi" tag) t)
   ;; Accept "ex:" only when it is not at the beginning of a line.
   ((equal "ex" tag) (not (equal 0 (length leader))))
   (t nil)))


(defun vimvars--obey-this-vim-modeline (settings-start settings-end)
  "Obey the VIM modeline which exists between SETTINGS-START and SETTINGS-END."
  ;; We ignore the local suffix, since for Emacs
  ;; most settings will be buffer-local anyway.
  (goto-char settings-start)
  ;; Look for something like this: vi: set sw=4 ts=4:
  ;; We should look for it in a comment, but for now
  ;; we won't worry about the syntax of the major mode.
  (while (re-search-forward
          " *\\([^= ]+\\)\\(=\\([^ :]+\\)\\)?" settings-end t)
    (let ((variable (vimvars--expand-option-name (match-string 1))))
      (if (match-string 3)
          (vimvars--assign variable (match-string 3))
        (vimvars--enable-feature variable))))
  t)


(defun vimvars--obey-top-modeline ()
  "Check for, and if found, obey a VIM modeline at the top of the file.
This function moves point."
  (goto-char (point-min))
  (if (and
       (re-search-forward vimvars-modeline-re
                    (line-end-position vimvars-check-lines) t)
       (vimvars--accept-tag (match-string 1) (match-string 2)))
      (vimvars--obey-this-vim-modeline (match-beginning 4) (match-end 4))))


(defun vimvars--obey-bottom-modeline ()
  "Check for, and if found, obey a VIM modeline at the botom of the file.
This function moves point."
  (goto-char (point-max))
  (if (and
       (re-search-backward vimvars-modeline-re
                     (line-beginning-position
                (- 1 vimvars-check-lines)) t)
       (vimvars--accept-tag (match-string 1) (match-string 2)))
      (vimvars--obey-this-vim-modeline (match-beginning 4) (match-end 4))))


(defun vimvars--set-indent (indent)
  "Set the amount of indentation caused by tab to INDENT in a mode-aware way."
  (when (equal major-mode 'c-mode) (setq c-basic-offset indent)))


(defun vimvars--expand-option-name (option)
  "Expand the abbreviated VIM :set variable OPTION to its full name."
  (let ((expansion
         (assoc option
                '(("ro" "readonly")
                  ("sts" "softtabstop")
                  ("sw" "shiftwidth")
                  ("ts" "tabstop")
                  ("tw" "textwidth")))))
    (if expansion (cadr expansion) option)))


;;; Not supported:
;;; comments/com (comment leader), because it's not language-specific in VIM.
(defun vimvars--assign (var val)
  "Emulate VIM's :set VAR=VAL."
  (message "Setting VIM option %s to %s in %s" var val (buffer-name))
  (cond
   ((equal var "makeprg") (setq compile-command val))
   ((equal var "shiftwidth") (vimvars--set-indent (string-to-number val)))
   ((equal var "softtabstop") t) ; Ignore.
   ((equal var "tabstop") (setq tab-width (string-to-number val)))
   ((equal var "textwidth") (set-fill-column (string-to-number val)))
   (t (message "Don't know how to emulate VIM variable %s" var))))


;; These features are not supported, but in principle they could be:
;;
;; fileencoding, encoding could be useful but likely too hairy
;; fileformat
;; tags
;; textmode (but this is obsolete in VIM, replaced by fileformat)
;;
;; Deliberately not supported:
;;
;; bomb/nobomd (byte order mark control), because I don't expect it is
;; comonly enough used to justify the maintenance burden.
(defun vimvars--enable-feature (var)
  "Emulate VIM's :set VAR for variables that are just boolean."
  (message "Enabling VIM option %s in %s" var (buffer-name))
  (cond
   ((equal var "expandtab") (setq indent-tabs-mode nil))
   ((equal var "ignorecase") (setq case-fold-search t))
   ((equal var "readonly") (toggle-read-only 1))
   ((equal var "wrap") (setq truncate-lines nil))
   ((equal var "write") (toggle-read-only -1)) ; Similar, not the same.

   ((equal var "noexpandtab") (setq indent-tabs-mode t))
   ((equal var "noignorecase") (setq case-fold-search nil))
   ((equal var "noreadonly") (toggle-read-only -1))
   ((equal var "nowrap") (setq truncate-lines t))
   ((equal var "nowrite") (toggle-read-only 1)) ; Similar, not the same

   (t (message "Don't know how to emulate VIM feature %s" var))))

;; If you want to manually test vimvars, one way to do this is
;; temporarily to create an Emacs Lisp file (which we will call
;; setup.el) in a convenient location, like this:
;;
;; (load-file "~/vimvars.el")
;; (add-hook 'find-file-hook 'vimvars-obey-vim-modeline)
;;
;; Then, you can test it like this:
;; emacs -q -nw -l setup.el my-test-file-name

(provide 'vimvars)
;;; vimvars.el ends here
