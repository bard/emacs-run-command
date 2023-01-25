;;; run-command-runner-term.el --- term-mode runner -*- lexical-binding: t -*-

;; Copyright (C) 2020-2023 Massimiliano Mirra

;; Author: Massimiliano Mirra <hyperstruct@gmail.com>
;; URL: https://github.com/bard/emacs-run-command
;; Version: 0.1.0
;; Keywords: processes

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Leave Emacs less.  Relocate those frequent shell commands to configurable,
;; dynamic, context-sensitive lists, and run them at a fraction of the
;; keystrokes with autocompletion.

;;; Code:

(require 'run-command-core)
(require 'nadvice)

(declare-function term-mode "ext:term")
(declare-function term-emulate-terminal "ext:term")
(defvar term-set-terminal-size)
(defvar term-raw-map)

(eval-when-compile
  (require 'cl-lib))

(defun run-command-runner-term (command-line buffer-base-name output-buffer)
  "Command execution backend for when run method is `term'.

Executes `COMMAND-LINE' in buffer `OUTPUT-BUFFER', naming it `BUFFER-BASE-NAME'."
  (with-current-buffer output-buffer
    (run-command-runner-term--run-in-current-buffer
     command-line
     buffer-base-name)))

(defun run-command-runner-term/noninteractive
    (command-line buffer-base-name output-buffer)
  "Command execution backend for when run method is `term'.

Executes `COMMAND-LINE' in buffer `OUTPUT-BUFFER', naming it `BUFFER-BASE-NAME'."
  (with-current-buffer output-buffer
    (run-command-runner-term--run-in-current-buffer
     command-line
     buffer-base-name)
    ;; Work around term.el bug causing spurious output when term in
    ;; is line mode, and the process clears the screen or part of it
    ;; repeatedly (e.g. test runners, installers).  Since it does
    ;; not happen in char mode, we trick the process filter into
    ;; thinking it is in char mode.  See
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=48716
    (set-process-filter
     (get-buffer-process (current-buffer))
     (lambda (proc str)
       (cl-letf (((symbol-function 'current-local-map)
                  (lambda () term-raw-map)))
         (term-emulate-terminal proc str))))
    (run-command-runner-term-minor-mode)))

(defun run-command-runner-term--run-in-current-buffer
    (command-line buffer-base-name)
  "Run given command in a term mode buffer.

Command is specified by `COMMAND-LINE' and resulting process is
named `BUFFER-BASE-NAME'."
  (let ((term-set-terminal-size t))
    (term-mode)
    (term-exec
     (current-buffer)
     buffer-base-name
     shell-file-name
     nil
     (list "-c" command-line))
    (term-char-mode)
    (run-command-runner-term-minor-mode)))

(define-minor-mode run-command-runner-term-minor-mode
  "Minor mode to re-enable `C-x' prefix in run-command term buffers.

Since command being run is likely an ancillary process and not
the main focus of the user's work, rebind the `C-x' prefix back to
its usual location, so that occasional operations in the command buffer
don't require mentally switching to a different keybinding scheme."
  :keymap '(([?\C-x] . Control-X-prefix)))

(define-advice term-erase-in-display
    (:around
     (original-term-erase-in-display kind)
     run-command-runner-term-erase-advice)
  "Advice to force clearing scrollback.

Commands in watch mode often ask the terminal to erase from home
position (first row, first column) to end of display, leaving
scrollback untouched.  This makes it hard to scroll up and find
the beginning of last run's output.  Hence we force clearing
scrollback, so user only has to scroll to beginning of buffer to
find the beginning of last run's output."
  (if (and (boundp 'run-command--command-spec) (or (eq kind 2) (eq kind 3)))
      (run-command-runner-term--patched-term-erase-in-display)
    (funcall original-term-erase-in-display kind)))

(defun run-command-runner-term--patched-term-erase-in-display ()
  "A local version of `term-erase-in-display' that clears entire buffer."
  (let ((row (term-current-row))
        (col (term-horizontal-column))
        (start-region (point-min))
        (end-region (point-max)))
    (delete-region start-region end-region)
    (term-unwrap-line)
    (setq term-current-column nil)
    (setq term-current-row nil)
    (term-goto row col)))

;;;; Meta

(provide 'run-command-runner-term)

;;; run-command-runner-term.el ends here
