;;; run-command.el --- Run an external command from a context-dependent list -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Massimiliano Mirra

;; Author: Massimiliano Mirra <hyperstruct@gmail.com>
;; URL: https://github.com/bard/emacs-run-command
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
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
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Pick and run a command from one or more user-configurable,
;; context-dependent lists of commands. Autocompletion via helm or
;; ivy, running via `compile'.

;;; Code:

(declare-function helm "ext:helm")
(declare-function helm-build-sync-source "ext:helm")
(declare-function ivy-read "ext:ivy")

;; Customization

(defgroup run-command nil
  "Run an external command from a context-dependent list."
  :group 'convenience)

(defcustom run-command-completion-method
  (if (fboundp 'helm) 'helm 'ivy)
  "Completion framework to use to select a command."
  :type '(choice (const :tag "Helm" helm)
                 (const :tag "Ivy" ivy)))

(defcustom run-command-config nil
  "List of functions that will produce runnable commands.

Each function will be called without arguments and is expected
to return a list-of-plists, where each plist represents a
runnable command and has the following format:

  :command-name

  :command-line

  :display

  :scope-name

  :working-dir
"
  :type '(repeat function)
  :group'run-command)

;; Entry point

(defun run-command ()
  "Pick a command from a context-dependent list, and run it.

The command is run with `compile'.

The command list is produced by the functions configured in
`run-command-config' (see that for the format expected from
said functions)."  
  (interactive)
  (pcase run-command-completion-method
    (helm
     (helm :buffer "*run-command*"
           :prompt "Command Name: "
           :sources (run-command--helm-sources)))
    (ivy (unless (window-minibuffer-p)
           (ivy-read "Command Name: "
                     (run-command--ivy-targets)
                     :action 'run-command--ivy-action)))))

;; Utilities

(defun run-command--compilation-buffer-name (command-name scope-name)
  (lambda (mode-name)
    (format "*%s[%s]*" command-name scope-name)))

;; Helm integration

(defun run-command--helm-sources ()
  (mapcar 'run-command--helm-source-from-config
          run-command-config))

(defun run-command--helm-source-from-config (command-list-generator)
  (let* ((command-specs (funcall command-list-generator))
         (candidates (mapcar (lambda (command-spec)
                               (cons (plist-get command-spec :display) command-spec))
                             command-specs)))
    (helm-build-sync-source (symbol-name command-list-generator)
      :action 'run-command--helm-action
      :candidates candidates
      :filtered-candidate-transformer '(helm-adaptive-sort))))

(defun run-command--helm-action (command-spec)
  (cl-destructuring-bind
      (&key command-name command-line scope-name working-dir &allow-other-keys)
      command-spec
    (let ((compilation-buffer-name-function
           (run-command--compilation-buffer-name command-name scope-name))
          (default-directory working-dir)
          (final-command-line (if helm-current-prefix-arg
                                  (read-string "> " (concat command-line " "))
                                command-line)))
      (compile final-command-line))))

;; Ivy integration

(defun run-command--ivy-targets ()
  (mapcan (lambda (command-list-generator)
            (let ((command-specs (funcall command-list-generator)))
              (mapcar (lambda (command-spec)
                        (cons (concat (propertize (concat (symbol-name command-list-generator)
                                                          "/")
                                                  'face
                                                  'shadow)
                                      (plist-get command-spec :display)) command-spec))
                      command-specs)))
          run-command-config))

(defun run-command--ivy-action (selection)
  (cl-destructuring-bind
      (&key command-name command-line scope-name working-dir &allow-other-keys)
      (cdr selection)
    (let ((compilation-buffer-name-function
           (run-command--compilation-buffer-name command-name scope-name))
          (default-directory working-dir))
      (compile command-line))))

(provide 'run-command)

;;; run-command.el ends here
