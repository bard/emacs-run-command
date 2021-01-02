;;; -*- lexical-binding: t -*-

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

(provide 'run-command--helm)
