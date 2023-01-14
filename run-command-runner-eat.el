;;; run-command-runner-eat.el --- term-mode runner -*- lexical-binding: t -*-

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

(declare-function eat-mode "ext:eat")

(defun run-command-runner-eat (command-line buffer-base-name output-buffer)
  "Runner that executes command in an `eat' buffer.

Executes COMMAND-LINE in buffer OUTPUT-BUFFER, naming it BUFFER-BASE-NAME."
  (require 'eat)
  (with-current-buffer output-buffer
    (eat-mode)
    (eat-exec output-buffer buffer-base-name
              "/usr/bin/env" nil (list "sh" "-c" command-line))))

(define-advice eat--t-erase-in-disp (:around
                                     (original-eat--t-erase-in-disp n)
                                     run-command-runner-eat-erase-advice)
  "When running command asks for screen clear, force erasure of entire
buffer rather than from home position to bottom, so no output from
previous runs is left in scrollback."
  (if (and (boundp 'run-command--command-spec)
           (eq n 2))
      (funcall original-eat--t-erase-in-disp 3)
    (funcall original-eat--t-erase-in-disp n)))

;;; Meta

(provide 'run-command-runner-eat)

;;; run-command-runner-eat.el ends here
