(require 'helm-scripts-util)

(defun helm-scripts-source-make ()
  (let ((scripts (helm-scripts--get-makefile-scripts)))
    (when scripts
      (let ((candidates (mapcar (lambda (script)
                                  (cons (plist-get script :display) script))
                                scripts)))
        (helm-build-sync-source "targets" :candidates candidates
                                :action 'helm-scripts-util--action)))))

(defun helm-scripts--get-makefile-scripts ()
  (let ((project-dir (locate-dominating-file default-directory
                                             (lambda (dir)
                                               (file-exists-p (concat dir "Makefile"))))))
    (when project-dir
      (let* ((makefile (concat project-dir "Makefile"))
             (targets (helm--make-cached-targets makefile)))
        (mapcar (lambda (target)
                  (list :name target
                        :display target
                        :working-dir project-dir
                        :scope-name project-dir
                        :command (concat "make " target)))
                targets)))))

(provide 'helm-scripts-source-make)
