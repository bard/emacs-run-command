;;; run-command-selector-helm.el --- Helm frontend to run-command -*- lexical-binding: t -*-

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

(declare-function helm "ext:helm")
(declare-function helm-make-source "ext:helm")
(defvar helm-current-prefix-arg)

(defun run-command-selector-helm (command-recipes default-command-runner)
  "Complete command with helm and run it."
  (helm :buffer "*run-command*"
        :prompt "Command: "
        :sources (run-command--helm-sources command-recipes default-command-runner)))

(defun run-command--helm-sources (command-recipes default-command-runner)
  "Create Helm sources from `RECIPES'."
  (mapcar (lambda (command-recipe)
            (run-command--helm-source-from-recipe command-recipe default-command-runner))
          command-recipes))

(defun run-command--helm-source-from-recipe (command-recipe default-command-runner)
  "Create a Helm source from `COMMAND-RECIPE'."
  (require 'helm-adaptive)
  (let* ((command-specs (run-command--generate-command-specs command-recipe))
         (candidates (mapcar (lambda (command-spec)
                               (cons (plist-get command-spec :display)
                                     command-spec))
                             command-specs)))
    (helm-make-source (run-command--shorter-recipe-name-maybe command-recipe)
        'helm-source-sync
      :action (lambda (command-spec)
                (run-command--helm-action command-spec default-command-runner))
      :candidates candidates
      :filtered-candidate-transformer 'helm-adaptive-sort)))

(defun run-command--helm-action (command-spec default-command-runner)
  "Execute `COMMAND-SPEC' from Helm."
  (let* ((command-line (plist-get command-spec :command-line))
         (final-command-line (if helm-current-prefix-arg
                                 (read-string "> " (concat command-line " "))
                               command-line)))
    (run-command--run (plist-put command-spec
                                 :command-line
                                 final-command-line)
                      default-command-runner)))

(provide 'run-command-selector-helm)

;;; run-command-selector-helm.el ends here

