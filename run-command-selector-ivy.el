;;; run-command-selector-ivy.el --- Ivy frontend to run-command -*- lexical-binding: t -*-

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

(require 'map)
(require 'seq)
(require 'run-command-core)
(require 'run-command-util)

(declare-function ivy-read "ext:ivy")
(defvar ivy-current-prefix-arg)

(defvar run-command--ivy-history nil
  "History for `run-command-selector-ivy'.")

(defun run-command-selector-ivy (command-recipes)
  "Select and run a command from `COMMAND-RECIPES' using Ivy."
  (unless (window-minibuffer-p)
    (ivy-read
     "Command: "
     (run-command--ivy-targets command-recipes)
     :caller 'run-command-selector-ivy
     :history 'run-command--ivy-history
     :action (lambda (command-spec) (run-command--ivy-action command-spec)))))

(defun run-command--ivy-targets (command-recipes)
  "Create Helm sources from `COMMAND-RECIPES'."
  (seq-map
   (lambda (command-spec)
     (cons
      (concat
       (propertize (concat
                    (run-command--shorter-recipe-name-maybe
                     (map-elt command-spec :recipe))
                    "/")
                   'face 'shadow)
       (map-elt command-spec :display))
      command-spec))
   (run-command-core-get-command-specs command-recipes)))

(defun run-command--ivy-action (selection)
  "Execute `SELECTION' from Ivy."
  (let* ((command-spec (cdr selection))
         (command-line (map-elt command-spec :command-line))
         (final-command-line
          (if ivy-current-prefix-arg
              (read-string "> " (concat command-line " "))
            command-line)))
    (map-put! command-spec :command-line final-command-line)
    (run-command-run command-spec)))

(defun run-command--ivy-edit-action (selection)
  "Edit `SELECTION' then execute from Ivy."
  (let ((ivy-current-prefix-arg t))
    (run-command--ivy-action selection)))

;;;; Meta

(provide 'run-command-selector-ivy)

;;; run-command-selector-ivy.el ends here
