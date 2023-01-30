;;; run-command-runner-term.el --- term-mode runner -*- lexical-binding: t -*-

;; Copyright (C) 2020-2023 Massimiliano Mirra

;; Author: Massimiliano Mirra <hyperstruct@gmail.com>
;; URL: https://github.com/bard/emacs-run-command

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

;; Runner for `run-command' based on `term-mode'.

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
  "Command runner based on `term-mode'.

Executes COMMAND-LINE in buffer OUTPUT-BUFFER.  Name the process BUFFER-BASE-NAME."
  (with-current-buffer output-buffer
    (let ((term-set-terminal-size t))
      (term-mode)
      (term-exec
       (current-buffer)
       buffer-base-name
       shell-file-name
       nil
       (list "-c" command-line))
      (term-char-mode)
      (run-command-runner-term-minor-mode))))

(define-minor-mode run-command-runner-term-minor-mode
  "Minor mode to re-enable `C-x' prefix in `run-command' term buffers.

Since a command run via `run-command' is likely an ancillary
process and not the main focus of the user's work, rebind the
`C-x' prefix back to its usual location, so that occasional
operations in the command buffer don't force the user to mentally
switch to a different keybinding scheme."
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
  (if (and (boundp 'run-command--command-spec)
           (or (eq kind 2)
               (eq kind 3)
               (eq kind 0) ; used by `cargo watch'
               ))
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
