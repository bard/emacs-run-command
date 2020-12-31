(defun helm-scripts-source-make ()
  (let ((project-dir (locate-dominating-file default-directory
                                             (lambda (dir)
                                               (file-exists-p (concat dir "Makefile"))))))
    (when project-dir
      (let* ((makefile (concat project-dir "Makefile"))
             (helm-make-command (helm--make-construct-command nil makefile)) ;; ignoring "arg"
             (targets (helm--make-cached-targets makefile))
             (default-directory (file-name-directory makefile))
             (candidates (helm-scripts--get-makefile-scripts project-dir)))
        (helm-build-sync-source "targets" :candidates candidates
                                :action 'helm-scripts-util--action)))))

(defun helm-scripts--get-makefile-scripts (project-dir)
  (mapcar (lambda (target)
            (cons target (list :name target
                               :working-dir project-dir
                               :scope-name project-dir
                               :command (concat "make " target))))
          (helm--make-cached-targets (concat project-dir "Makefile"))))

(provide 'helm-scripts-source-make)
