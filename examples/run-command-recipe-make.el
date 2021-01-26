
;; Run a target from the project's Makefile. Requires
;; https://github.com/abo-abo/helm-make

(require 'helm-make)

(defun run-command-recipe-make ()
  (let ((project-dir (locate-dominating-file default-directory
                                             "Makefile")))
    (when project-dir
      (let* ((makefile (concat project-dir "Makefile"))
             (targets (helm--make-cached-targets makefile)))
        (mapcar (lambda (target)
                  (list :command-name target
                        :command-line (concat "make " target)
                        :display target
                        :working-dir project-dir
                        :scope-name project-dir))
                targets)))))


