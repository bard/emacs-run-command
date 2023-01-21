;;; run-command-selector-completing-read.el --- completing-read frontend to run-command -*- lexical-binding: t -*-

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

(require 'map)
(require 'run-command-core)

(defun run-command-selector-completing-read (command-recipes default-command-runner)
  "Complete command with `completing-read' and run it."
  (let* ((targets (run-command--completing-read-targets command-recipes))
         (choice (completing-read "Command: " targets)))
    (when choice
      (let ((command-spec (cdr (assoc choice targets))))
        (run-command-run command-spec)))))

(defun run-command--completing-read-targets (command-recipes)
  "Create completion-read targets from all recipes."
  (seq-mapcat (lambda (command-recipe)
                (let ((command-specs
                       (run-command-get-command-specs command-recipe))
                      (recipe-name
                       (run-command--shorter-recipe-name-maybe command-recipe)))
                  (seq-map (lambda (command-spec)
                             (cons (concat
                                    (propertize (concat recipe-name "/")
                                                'face 'shadow)
                                    (map-elt command-spec :display))
                                   command-spec))
                           command-specs)))
              command-recipes))

;;; Meta

(provide 'run-command-selector-completing-read)

;;; run-command-selector-completing-read.el ends here

