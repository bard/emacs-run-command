;;; run-command-core.el --- run-command core -*- lexical-binding: t -*-

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

;;; Commentary:
;;
;; Leave Emacs less.  Relocate those frequent shell commands to configurable,
;; dynamic, context-sensitive lists, and run them at a fraction of the
;; keystrokes with autocompletion.

;;; Code:

;; Customization

(defgroup run-command nil
  "Run an external command from a context-dependent list."
  :group 'convenience
  :prefix "run-command-"
  :link '(url-link "https://github.com/bard/emacs-run-command"))

;; XXX TODO replace with function
(defcustom run-command-completion-method
  'auto
  "Completion framework to use to select a command.

- `autodetect' (default): pick helm, ivy, or none, based on active
  global completion mode
- `helm': force use helm
- `ivy': force use ivy
- `completing-read': force use `completing-read'"
  :type '(choice (const :tag "autodetect" auto)
                 (const :tag "helm" helm)
                 (const :tag "ivy" ivy)
                 (const :tag "completing-read" completing-read)))

(defcustom run-command-recipes nil
  "List of functions that will produce command lists.

  Each function is called without arguments and must return a list of property
  lists, where each property list represents a command and has the following
  format:

  - `:command-name' (string, required): A name for the command, used
  to generate the output buffer name, as well as a fallback in case
  `:display' isn't provided
  - `:command-line' (string or function, required): The command
  line that will be executed.  Can be a function to e.g. further
  query the user, and should return a string.
  - `:display' (string, optional): A descriptive name that will
  be displayed to the user.
  - `:working-dir' (string, optional): Directory to run the command
  in.  If not given, defaults to `default-directory'."

  :type '(repeat function)
  :group 'run-command)

;; Utilities

(defvar-local run-command--command-spec nil
  "Holds command spec for command run via `run-command'.")

(defun run-command--generate-command-specs (command-recipe)
  "Execute `COMMAND-RECIPE' to generate command specs."
  (let ((command-specs
         (cond
          ((fboundp command-recipe)
           (funcall command-recipe))
          (t (error "[run-command] Invalid command recipe: %s" command-recipe)))))
    (mapcar #'run-command--normalize-command-spec
            (seq-filter (lambda (spec)
                          (and spec (map-elt spec :command-line)))
                        command-specs))))

(defun run-command--normalize-command-spec (command-spec)
  "Sanity-check and fill in defaults for user-provided `COMMAND-SPEC'."
  (unless (stringp (plist-get command-spec :command-name))
    (error "[run-command] Invalid `:command-name' in command spec: %S"
           command-spec))
  (unless (or (stringp (plist-get command-spec :command-line))
              (functionp (plist-get command-spec :command-line)))
    (error "[run-command] Invalid `:command-line' in command spec: %S"
           command-spec))
  (append command-spec
          (unless (plist-member command-spec :display)
            (list :display (plist-get command-spec :command-name)))
          (unless (plist-member command-spec :working-dir)
            (list :working-dir default-directory))
          (unless (plist-member command-spec :scope-name)
            (list :scope-name (abbreviate-file-name
                               (or (plist-get command-spec :working-dir)
                                   default-directory))))))

(defun run-command--shorter-recipe-name-maybe (command-recipe)
  "Shorten `COMMAND-RECIPE' name when it begins with conventional prefix."
  (let ((recipe-name (symbol-name command-recipe)))
    (if (string-match "^run-command-recipe-\\(.+\\)$" recipe-name)
        (match-string 1 recipe-name)
      recipe-name)))

(defun run-command--run (command-spec default-command-runner)
  "Run `COMMAND-SPEC'.  Back end for helm and ivy actions."
  (let* ((command-name (plist-get command-spec :command-name))
         (command-line (if (functionp (plist-get command-spec :command-line))
                           (funcall (plist-get command-spec :command-line))
                         (plist-get command-spec :command-line)))
         (command-runner (or (plist-get command-spec :runner)
                             default-command-runner))
         (scope-name (plist-get command-spec :scope-name))
         (working-directory (or (plist-get command-spec :working-dir)
                                default-directory))
         (buffer-base-name (format "%s[%s]" command-name scope-name)))
    (with-current-buffer (run-command--create-and-display-execution-buffer buffer-base-name)
      (let ((default-directory working-directory))
        (funcall command-runner command-line buffer-base-name (current-buffer))
        (setq-local run-command--command-spec command-spec)))))

(defun run-command--create-and-display-execution-buffer (buffer-base-name)
  (let ((buffer-name (concat "*" buffer-base-name "*"))
        (existing-buffer-window nil))
    (when (get-buffer buffer-name)
      (setq existing-buffer-window (get-buffer-window buffer-name))
      (let ((proc (get-buffer-process buffer-name)))
        (when (and proc
                   (yes-or-no-p "A process is running; kill it?"))
  (condition-case ()
      (progn
        (interrupt-process proc)
        (sit-for 1)
        (delete-process proc))
    (error nil))))
      (kill-buffer buffer-name))
    (with-current-buffer (get-buffer-create buffer-name)
      ;; Display buffer before enabling vterm mode, so that vterm can
      ;; read the column number accurately.
      (if existing-buffer-window
          (set-window-buffer existing-buffer-window (current-buffer))
        (display-buffer (current-buffer)))
      (current-buffer))))

;; Experiments

(defvar run-command-experiments nil)

(defvar run-command--deprecated-experiment-warning t)

(defun run-command--experiment--active-p (name)
  "Return t if experiment `NAME' is enabled, nil otherwise."
  (member name run-command-experiments))

(defun run-command--check-experiments ()
  "Sanity-check the configured experiments.

If experiment is active, do nothing.  If experiment is retired or unknown,
signal error.  If deprecated, print a warning and allow muting further warnings
for the rest of the session."
  (let ((experiments '((static-recipes . retired)
                       (vterm-run-method . retired)
                       (run-command-experiment-vterm-run-method . retired)
                       (run-command-experiment-function-command-lines . retired)
                       (run-command-experiment-lisp-commands . retired)
                       (example-retired .  retired)
                       (example-deprecated . deprecated))))
    (mapc (lambda (experiment-name)
            (let ((experiment (seq-find (lambda (e)
                                          (eq (car e) experiment-name))
                                        experiments)))
              (if experiment
                  (let ((name (car experiment))
                        (status (cdr experiment)))
                    (pcase status
                      ('retired
                       (error "[run-command] Experiment `%S' was \
retired, please remove from `run-command-experiments'" name))
                      ('deprecated
                       (when run-command--deprecated-experiment-warning
                         (setq run-command--deprecated-experiment-warning
                               (not (yes-or-no-p
                                     (format "Warning: run-command: experiment \
 `%S' is deprecated, please update your configuration.  Disable reminder for \
this session?" name))))))
                      ('active nil)))
                (error "[run-command] Experiment `%S' does not exist, \
please remove from `run-command-experiments'" experiment-name))))
          run-command-experiments)))

;;; Meta

(provide 'run-command-core)

;;; run-command-core.el ends here
