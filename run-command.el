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
;; Pick external commands from smart lists that are based on project type,
;; buffer mode, favorite scripts, or anything you fancy, and run them in
;; compilation mode, in as few keystrokes as possible, without memorizing
;; key bindings, autocompleting with Helm or Ivy.

;;; Code:

(declare-function helm "ext:helm")
(declare-function helm-build-sync-source "ext:helm")
(declare-function ivy-read "ext:ivy")

;; Customization

(defgroup run-command nil
  "Run an external command from a context-dependent list."
  :group 'convenience)

(defcustom run-command-completion-method
  (if (featurep 'ivy) 'ivy 'helm)
  "Completion framework to use to select a command."
  :type '(choice (const :tag "Helm" helm)
                 (const :tag "Ivy" ivy)))

(defcustom run-command-recipes nil
  "List of functions that will produce runnable commands.

Each function will be called without arguments and is expected
to return a list-of-plists, where each plist represents a
runnable command and has the following format:

  :command-name

    (string, required) A name for the command, used internally as well as (if
:display is not provided) shown to the user.

  :command-line

    (string, required) The command line that will be executed.  It will be
passed to `compile'.

  :display

    (string, optional) A descriptive name for the command that will be shown
in place of :command-name.

  :working-dir

    (string, optional) Directory path to run the command in.  If not given,
command will be run in `default-directory'."
  :type '(repeat function)
  :group'run-command)

;; Entry point

(defun run-command ()
  "Pick a command from a context-dependent list, and run it.

The command is run with `compile'.

The command list is produced by the functions configured in
`run-command-recipes' (see that for the format expected from
said functions)."
  (interactive)
  (pcase run-command-completion-method
    ('helm
     (helm :buffer "*run-command*"
           :prompt "Command Name: "
           :sources (run-command--helm-sources)))
    ('ivy (unless (window-minibuffer-p)
            (ivy-read "Command Name: "
                      (run-command--ivy-targets)
                      :action 'run-command--ivy-action)))))

;; Recipes

(defun run-command-recipe-package-json ()
  "Extract NPM scripts for `run-command' from the current project's 
package.json."
  (let ((project-dir (locate-dominating-file default-directory
                                             "package.json")))
    (when project-dir
      (with-temp-buffer
        (insert-file-contents (concat project-dir "package.json"))
        (let* ((package-json (json-parse-buffer))
               (project-name (gethash "name" package-json))
               (script-map (gethash "scripts" package-json))
               (scripts '())
               (runner (if (file-exists-p (concat project-dir "yarn.lock"))
                           "yarn"
                         "npm")))
          (maphash (lambda (key value)
                     (let ((command (concat runner " run " key)))
                       (push (list :command-name key
                                   :command-line command
                                   :display key
                                   :scope-name project-name
                                   :working-dir project-dir)
                             scripts)))
                   script-map)
          scripts)))))

;; Utilities

(defun run-command--compilation-buffer-name (command-name scope-name)
  (lambda ()
    (format "*%s[%s]*" command-name scope-name)))

(defun run-command--normalize-command-spec (command-spec)
  (when (not (plist-get command-spec :command-name))
    (error "[run-command] `:command-name' item missing from command spec"))
  (when (not (plist-get command-spec :command-line))
    (error "[run-command] `:command-line' item missing from command spec"))
  (append command-spec
          (when (not (plist-get command-spec :display))
            (list :display (plist-get command-spec :command-name)))
          (when (not (plist-get command-spec :working-dir))
            (list :working-dir default-directory))
          (when (not (plist-get command-spec :scope-name))
            (list :scope-name default-directory))))

;; Helm integration

(defun run-command--helm-sources ()
  (mapcar 'run-command--helm-source-from-config
          run-command-recipes))

(defun run-command--helm-source-from-config (command-list-generator)
  (let* ((command-specs (mapcar #'run-command--normalize-command-spec
                                (funcall command-list-generator)))
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
            (let ((command-specs (mapcar #'run-command--normalize-command-spec
                                         (funcall command-list-generator))))
              (mapcar (lambda (command-spec)
                        (cons (concat (propertize (concat (symbol-name command-list-generator)
                                                          "/")
                                                  'face
                                                  'shadow)
                                      (plist-get command-spec :display)) command-spec))
                      command-specs)))
          run-command-recipes))

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
