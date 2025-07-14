# Vimvars

The accompanying code is intended to provide support for VI-style
mode lines in Emacs.  The idea is that using this code, you can make
your Emacs editor obey the vi-oriented settings your collaborators put
in the file, so that when you start working on their project, your
code is correctly laid out from the start.

## How To Use Vimvars

To use this library, just put vimvars.el in a directory on your load-path
and add this to your .emacs file:

;;; Basic VIM tolerance
(require 'vimvars)
(add-hook 'find-file-hook 'vimvars-obey-vim-modeline)

## How To Test The Code

This code is currently in the middle of a transition from a custom
test suite to the use of the standard ERT testing system.  The
already-converted tests are in `vimvars-tests.el` and the old-style
tests are in `vimvars-regtest.el`.

This means that for the time being, you need a long cumbersome
command-line to run the tests:

emacs -batch -l ert -l vimvars.el -l vimvars-tests.el -l vimvars-regtest.el -f ert-run-tests-batch-and-exit
