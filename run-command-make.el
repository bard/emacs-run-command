(require 'helm-make)

(defun run-command-make ()
  (let ((project-dir (locate-dominating-file default-directory
                                             "Makefile")))
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

(provide 'run-command-make)
