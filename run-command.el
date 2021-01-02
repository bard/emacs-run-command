;;; run-command.el --- Run an external command from a list -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Massimiliano Mirra

;; Author: Massimiliano Mirra <hyperstruct@gmail.com>
;; URL: https://github.com/bard/emacs-run-command
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
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

(require 'run-command--helm)
(require 'run-command--ivy)

(defgroup run-command nil
  "Run an external command from a context-dependent list."
  :group 'convenience)

(defcustom run-command-completion-method 'helm
  "Completion framework to use to select a command."
  :type '(choice (const :tag "Helm"
                        helm)
                 (const :tag "Ivy"
                        ivy)))

(defcustom run-command-config nil
  "List of functions that will produce runnable commands."
  :type '(repeat function):group'run-command)

(defun run-command ()
  (interactive)
  (pcase run-command-completion-method
    ('helm
     (helm :buffer "*run-command*"
           :prompt "Command Name: "
           :sources (run-command--helm-sources)))
    (ivy (unless (window-minibuffer-p)
           (ivy-read "Command Name: "
                     (run-command--ivy-targets)
                     :action 'run-command--ivy-action)))))

(provide 'run-command)

;;; run-command.el ends here
