
(require 'helm-scripts-package-json)
(require 'helm-scripts-make)
(require 'helm-scripts-hugo)
(require 'helm-scripts-global)

(defvar helm-scripts-config (list 'helm-scripts-package-json 'helm-scripts-hugo
                                  'helm-scripts-makefile 'helm-scripts-global))

(defun helm-scripts ()
  (interactive)
  (helm :buffer "*helm scripts*"
        :prompt "Script name: "
        :sources (helm-scripts--sources)))

(defun helm-scripts--sources ()
  (mapcar 'helm-scripts--source-from-config
          helm-scripts-config))

(defun helm-scripts--source-from-config (config-name)
  (let* ((scripts (funcall config-name))
         (candidates (mapcar (lambda (script)
                               (cons (plist-get script :display) script))
                             scripts)))
    (helm-build-sync-source (symbol-name config-name)
      :action 'helm-scripts-util--action
      :candidates candidates)))

(defun helm-scripts--action (script)
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
