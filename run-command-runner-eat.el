;;; run-command-runner-eat.el --- term-mode runner -*- lexical-binding: t -*-

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

;; Runner for `run-command' based on `eat-mode'.

;;; Code:

(require 'run-command-core)

(declare-function eat-mode "ext:eat")

(defun run-command-runner-eat (command-line buffer-base-name output-buffer)
  "Command runner based on `eat-mode'.

Executes COMMAND-LINE in buffer OUTPUT-BUFFER.  Name the process BUFFER-BASE-NAME."
  (require 'eat)
  (with-current-buffer output-buffer
    (let ((eat-semi-char-non-bound-keys '([C-next] [C-prior])))
      (eat-mode)
      (eat-exec
       output-buffer
       buffer-base-name
       shell-file-name
       nil
       (list "-c" command-line)))))

(define-advice eat--t-erase-in-disp
    (:around
     (original-eat--t-erase-in-disp n)
     run-command-runner-eat-erase-advice)
  "Advice to force clearing scrollback.

Commands in watch mode often ask the terminal to erase from home
position (first row, first column) to end of display, leaving
scrollback untouched.  This makes it hard to scroll up and find
the beginning of last run's output.  Hence we force clearing
scrollback, so user only has to scroll to beginning of buffer to
find the beginning of last run's output."
  (if (and (boundp 'run-command--command-spec) (eq n 2))
      (funcall original-eat--t-erase-in-disp 3)
    (funcall original-eat--t-erase-in-disp n)))

;;;; Meta

(provide 'run-command-runner-eat)

;;; run-command-runner-eat.el ends here
