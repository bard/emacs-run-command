;;; run-command-runner-vterm.el --- term-mode runner -*- lexical-binding: t -*-

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

(declare-function vterm-mode "ext:vterm")
(defvar vterm-kill-buffer-on-exit)
(defvar vterm-shell)

(defun run-command-runner-vterm (command-line buffer-base-name output-buffer)
  "Command execution backend for `vterm' experiment.

Executes COMMAND-LINE in buffer OUTPUT-BUFFER, naming it BUFFER-BASE-NAME."
  (require 'vterm)
  (with-current-buffer output-buffer
    (let ((vterm-kill-buffer-on-exit nil)
          ;; XXX needs escaping or commands containing quotes will cause trouble
          (vterm-shell (format "%s -c '%s'" vterm-shell command-line)))
      (vterm-mode)
      ;; (use-local-map nil)
      ;; (vterm-copy-mode)
      )))

;;;; Meta

(provide 'run-command-runner-vterm)

;;; run-command-runner-vterm.el ends here
