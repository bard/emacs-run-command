;;; -*- lexical-binding: t -*-

(require 'run-command)

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
  (let* ((command-spec (cdr selection))
         (command (plist-get command-spec :command))
         (command-name (plist-get command-spec :name))
         (scope-name (plist-get command-spec :scope-name))
         (working-dir (plist-get command-spec :working-dir))
         (compilation-buffer-name-function (lambda (name-of-mode)
                                             (concat "*" command-name "(" scope-name ")"
                                                     "*"))))
    (message "spec: %S" command)
    (let ((default-directory working-dir))
      (compile command))))

(provide 'run-command--ivy)
