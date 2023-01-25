;;; run-command-util.el --- run-command utils -*- lexical-binding: t -*-

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

(defun run-command-util-shorter-recipe-name-maybe (command-recipe)
  "Shorten `COMMAND-RECIPE' name when it begins with conventional prefix."
  (let ((recipe-name (symbol-name command-recipe)))
    (if (string-match "^run-command-recipe-\\(.+\\)$" recipe-name)
        (match-string 1 recipe-name)
      recipe-name)))

;;;; Meta

(provide 'run-command-util)

;;; run-command-util.el ends here
