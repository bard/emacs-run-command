(provide 'helm-scripts-util)

(defun helm-scripts-util--get-project-dir ()
  (locate-dominating-file default-directory
                          (lambda (dir)
                            (file-exists-p (concat dir ".git")))))

(defun helm-scripts-util--action (script)
  (let* ((script-command (plist-get script :command))
         (script-name (plist-get script :name))
         (project-name (plist-get script :project-name))
         (project-dir (plist-get script :project-dir))
         (compilation-buffer-name-function (lambda (name-of-mode)
                                             (concat "*" script-name "(" project-name ")"
                                                     "*"))))
    (let ((default-directory project-dir))
      (compile (if helm-current-prefix-arg
                   (read-string "> "
                                (concat script-command " "))
                 script-command)))))
