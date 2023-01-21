
;; Run a target from the project's Makefile. 

(require 'helm-make)
(require 'seq)

(defun run-command-recipe-make ()
  "Provide commands to run Makefile targets.

Requires `helm-make' (https://github.com/abo-abo/helm-make) to be installed."
  (when-let* ((project-dir (locate-dominating-file default-directory "Makefile"))
              (makefile (concat project-dir "Makefile"))
              (targets (helm--make-cached-targets makefile)))
    (seq-map (lambda (target)
               (list :command-name target
                     :command-line (concat "make " target)
                     :display target
                     :working-dir project-dir))
             targets)))

