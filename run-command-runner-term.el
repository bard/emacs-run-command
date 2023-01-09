;;; run-command-runner-term.el --- term-mode runner -*- lexical-binding: t -*-

;; Copyright (C) 2020-2023 Massimiliano Mirra

;; Author: Massimiliano Mirra <hyperstruct@gmail.com>
;; URL: https://github.com/bard/emacs-run-command
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
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

;;; Code:

(require 'run-command-core)

(declare-function term-mode "ext:term")
(declare-function term-emulate-terminal "ext:term")
(defvar term-set-terminal-size)
(defvar term-raw-map)

(eval-when-compile (require 'cl-lib))

(defun run-command-runner-term (command-line buffer-base-name output-buffer)
  "Command execution backend for when run method is `term'.

Executes `COMMAND-LINE' in buffer `OUTPUT-BUFFER', naming it `BUFFER-BASE-NAME'."
  (with-current-buffer output-buffer
    (let ((term-set-terminal-size t))
      (run-command-runner-term--run-with-patched-emulator buffer-base-name command-line)
      ;; (setq term-scroll-show-maximum-output t) ; XXX experimental
      (compilation-minor-mode)
      (run-command-runner-term-minor-mode))))

(define-minor-mode run-command-runner-term-minor-mode
  "Minor mode to re-run `run-command' commands started in term buffers."
  :keymap '(("g" .  run-command-runner-term--recompile)))

(defun run-command-runner-term--recompile ()
  "Provide `recompile' in term buffers with command run via `run-command'."
  (interactive)
  (run-command--run run-command--command-spec))

(defun run-command-runner-term--run-with-patched-emulator (buffer-base-name command-line)
  "Run `COMMAND-LINE' in a term buffer, working around a bug in `term-mode'.

The bug causes spurious output when term in is line mode, and the
process clears the screen or part of it repeatedly (e.g. test
runners, installers).  Since it does not happen in char mode, we
trick the process filter into thinking it is in char mode.  See
https://debbugs.gnu.org/cgi/bugreport.cgi?bug=48716

`BUFFER-BASE-NAME' and `COMMAND-LINE' are passed to `make-term'."
  (let ((process-buffer (make-term buffer-base-name
                                   shell-file-name nil
                                   "-c" command-line)))
    (set-process-filter (get-buffer-process process-buffer)
                        (lambda (proc str)
                          (cl-letf (((symbol-function 'current-local-map)
                                     (lambda () term-raw-map)))
                            (term-emulate-terminal proc str))))
    process-buffer))

;;; Meta

(provide 'run-command-runner-term)

;;; run-command-runner-term.el ends here
