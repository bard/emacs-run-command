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
(require 'nadvice)

(declare-function term-mode "ext:term")
(declare-function term-emulate-terminal "ext:term")
(defvar term-set-terminal-size)
(defvar term-raw-map)

(eval-when-compile (require 'cl-lib))

(defun run-command-runner-term (command-line buffer-base-name output-buffer)
  "Command execution backend for when run method is `term'.

Executes `COMMAND-LINE' in buffer `OUTPUT-BUFFER', naming it `BUFFER-BASE-NAME'."
  (with-current-buffer output-buffer
    (run-command-runner-term--run-in-current-buffer command-line buffer-base-name)))

(defun run-command-runner-term/noninteractive (command-line buffer-base-name output-buffer)
  "Command execution backend for when run method is `term'.

Executes `COMMAND-LINE' in buffer `OUTPUT-BUFFER', naming it `BUFFER-BASE-NAME'."
  (with-current-buffer output-buffer
    (run-command-runner-term--run-in-current-buffer command-line buffer-base-name)
    ;; Work around term.el bug causing spurious output when term in
    ;; is line mode, and the process clears the screen or part of it
    ;; repeatedly (e.g. test runners, installers).  Since it does
    ;; not happen in char mode, we trick the process filter into
    ;; thinking it is in char mode.  See
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=48716
    (set-process-filter (get-buffer-process (current-buffer))
                        (lambda (proc str)
                          (cl-letf (((symbol-function 'current-local-map)
                                     (lambda () term-raw-map)))
                            (term-emulate-terminal proc str))))
    (run-command-runner-term-minor-mode)))

(defun run-command-runner-term--run-in-current-buffer (command-line buffer-base-name)
  (let ((term-set-terminal-size t)
        (original-control-x-key (where-is-internal 'Control-X-prefix nil t)))
    (term-mode)
    (term-exec (current-buffer) buffer-base-name
               shell-file-name nil (list "-c" command-line))

    ;; Since command being run is likely an ancillary process and not
    ;; the main focus of the user's work, rebind the C-x prefix again
    ;; so occasional operations in the command buffer can be done
    ;; without a mindset switch from Emacs flow to terminal flow.
    (let ((overriding-term-raw-map (make-sparse-keymap)))
      (set-keymap-parent overriding-term-raw-map term-raw-map)
      (define-key overriding-term-raw-map original-control-x-key 'Control-X-prefix)
      (let ((term-raw-map overriding-term-raw-map))
        (term-char-mode)))))

(define-minor-mode run-command-runner-term-minor-mode
  "Minor mode to re-run `run-command' commands started in term buffers."
  :keymap '(("g" .  run-command-runner-term--recompile)))

(define-advice term-erase-in-display (:around
                                      (original-term-erase-in-display kind)
                                      run-command-runner-term-erase-advice)
  "When running command asks for screen clear, force erasure of entire
buffer rather than from home position to bottom, so no output from
previous runs is left in scrollback."
  (if (and (boundp 'run-command--command-spec)
           (eq kind 2))
      (delete-region (point-min) (point-max))
    (funcall original-term-erase-in-display kind)))

(defun run-command-runner-term--recompile ()
  "Provide `recompile' in term buffers with command run via `run-command'."
  (interactive)
  (run-command--run run-command--command-spec))

;;; Meta

(provide 'run-command-runner-term)

;;; run-command-runner-term.el ends here
