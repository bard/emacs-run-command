;;; run-command.el --- Run an external command from a list

;; Copyright (C) 2020-2021 Massimiliano Mirra

;; Author: Massimiliano Mirra <hyperstruct@gmail.com>
;; URL: https://github.com/bard/emacs-run-command
;; Version: 0.1.0
;; Keywords: compilation

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

;;; Code:


(declare-function helm "ext:helm")
(declare-function helm-build-sync-source "ext:helm")

(defgroup run-command nil "Run an external command from a context-dependent list.
:group 'convenience")

(defcustom run-command-completion-method 'helm
  "Completion framework to use to select a command."
  :type '(choice (const :tag "Helm"
                        helm)))

(defcustom run-command-config nil
  "List of functions that will produce runnable commands."
  :type '(repeat function):group'run-command)

(defun run-command ()
  (interactive)
  (pcase run-command-completion-method
    ('helm
     (helm :buffer "*run-command*"
           :prompt "Command name: "
           :sources (run-command--helm-sources)))))

(defun run-command--helm-sources ()
  (mapcar 'run-command--helm-source-from-config
          run-command-config))

(defun run-command--helm-source-from-config (config-name)
  (let* ((scripts (funcall config-name))
         (candidates (mapcar (lambda (script)
                               (cons (plist-get script :display) script))
                             scripts)))
    (helm-build-sync-source (symbol-name config-name)
      :action 'run-command--helm-action
      :candidates candidates
      :filtered-candidate-transformer '(helm-adaptive-sort))))

(defun run-command--helm-action (script)
  (let* ((command (plist-get script :command))
         (command-name (plist-get script :name))
         (scope-name (plist-get script :scope-name))
         (working-dir (plist-get script :working-dir))
         (compilation-buffer-name-function (lambda (name-of-mode)
                                             (concat "*" command-name "(" scope-name ")"
                                                     "*"))))
    (let ((default-directory working-dir))
      (compile (if helm-current-prefix-arg
                   (read-string "> "
                                (concat command " "))
                 command)))))
