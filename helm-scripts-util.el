(provide 'helm-scripts-util)

(defun helm-scripts-util--get-project-dir ()
  (locate-dominating-file default-directory
                          (lambda (dir)
                            (file-exists-p (concat dir ".git")))))

(defun helm-scripts-util--action (script)
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
