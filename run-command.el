
(require 'run-command-package-json)
(require 'run-command-make)
(require 'run-command-hugo)
(require 'run-command-global)

(declare-function helm "ext:helm")
(declare-function helm-build-sync-source "ext:helm")

(defvar run-command-config (list 'run-command-package-json 'run-command-hugo
                                 'run-command-makefile 'run-command-global))

(defun run-command ()
  (interactive)
  (helm :buffer "*helm scripts*"
        :prompt "Script name: "
        :sources (run-command--sources)))

(defun run-command--sources ()
  (mapcar 'run-command--source-from-config run-command-config))

(defun run-command--source-from-config (config-name)
  (let* ((scripts (funcall config-name))
         (candidates (mapcar (lambda (script)
                               (cons (plist-get script :display) script))
                             scripts)))
    (helm-build-sync-source (symbol-name config-name)
      :action 'run-command-util--action
      :candidates candidates)))

(defun run-command--action (script)
  (let* ((script-command (plist-get script :command))
         (script-name (plist-get script :name))
         (scope-name (plist-get script :scope-name))
         (working-dir (plist-get script :working-dir))
         (compilation-buffer-name-function (lambda (name-of-mode)
                                             (concat "*" script-name "(" scope-name ")"
                                                     "*"))))
    (let ((default-directory working-dir))
      (compile (if helm-current-prefix-arg
                   (read-string "> "
                                (concat script-command " "))
                 script-command)))))
