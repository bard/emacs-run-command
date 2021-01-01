(require 'run-command)

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
  (let* ((command (plist-get command-spec :command))
         (command-name (plist-get command-spec :name))
         (scope-name (plist-get command-spec :scope-name))
         (working-dir (plist-get command-spec :working-dir))
         (compilation-buffer-name-function (lambda (name-of-mode)
                                             (concat "*" command-name "(" scope-name ")"
                                                     "*"))))
    (let ((default-directory working-dir))
      (compile (if helm-current-prefix-arg
                   (read-string "> "
                                (concat command " "))
                 command)))))

(provide 'run-command--helm)
