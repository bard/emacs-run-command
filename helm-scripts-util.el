(provide 'helm-scripts-util)

(defun helm-scripts-util--get-project-dir ()
  (locate-dominating-file default-directory
                          (lambda (dir)
                            (file-exists-p (concat dir ".git")))))
