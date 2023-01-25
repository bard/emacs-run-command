;;; run-command-runner-compile.el --- term-mode runner -*- lexical-binding: t -*-

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

(defun run-command-runner-compile (command-line buffer-base-name output-buffer)
  "Command execution backend for when run method is `compile'.

Executes COMMAND-LINE in buffer OUTPUT-BUFFER, naming it BUFFER-BASE-NAME."
  (with-current-buffer output-buffer
    (compilation-mode)
    (compilation-start command-line
                       nil
                       (lambda (_name-of-mode) (concat "*" buffer-base-name "*")))))

;;;; Meta

(provide 'run-command-runner-compile)

;;; run-command-runner-compile.el ends here
