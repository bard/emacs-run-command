;;; -*- lexical-binding: t -*-

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
      (&key command name scope-name working-dir &allow-other-keys)
      (cdr selection)
    (let ((compilation-buffer-name-function
           (run-command--compilation-buffer-name name scope-name))
          (default-directory working-dir))
      (compile command))))

(provide 'run-command--ivy)
