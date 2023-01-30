;;; run-command-selector-helm.el --- Helm frontend to run-command -*- lexical-binding: t -*-

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

;; Selector for `run-command' based on Helm.

;;; Code:

(require 'map)
(require 'seq)
(require 'run-command-core)
(require 'run-command-util)

(declare-function helm "ext:helm")
(declare-function helm-make-source "ext:helm")
(defvar helm-current-prefix-arg)

(defun run-command-selector-helm (command-recipes)
  "Select and run a command from COMMAND-RECIPES using Helm."
  (helm
   :buffer "*run-command*"
   :prompt "Command: "
   :sources (run-command--helm-sources command-recipes)))

(defun run-command--helm-sources (command-recipes)
  "Create Helm sources from COMMAND-RECIPES."
  (require 'helm-adaptive)
  (thread-last
   command-recipes
   (run-command-core-get-command-specs)
   (seq-group-by (lambda (spec) (map-elt spec :recipe)))
   (seq-map
    (lambda (recipe-specs-pair)
      (pcase-let ((`(,recipe . ,specs) recipe-specs-pair))
        (helm-make-source
         (run-command--shorter-recipe-name-maybe recipe)
         'helm-source-sync
         :action (lambda (spec) (run-command--helm-action spec))
         :candidates
         (seq-map
          (lambda (spec)
            (cons
             (map-elt spec :display) spec))
          specs)
         :filtered-candidate-transformer 'helm-adaptive-sort))))))

(defun run-command--helm-action (command-spec)
  "Execute COMMAND-SPEC from Helm."
  (let* ((command-line (map-elt command-spec :command-line))
         (final-command-line
          (if helm-current-prefix-arg
              (read-string "> " (concat command-line " "))
            command-line)))
    (map-put! command-spec :command-line final-command-line)
    (run-command-core-run command-spec)))

;;;; Meta

(provide 'run-command-selector-helm)

;;; run-command-selector-helm.el ends here
