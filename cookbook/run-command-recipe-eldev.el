(require 'subr-x)

(defun run-command-recipe-eldev ()
  "Provide commands for an Emacs project managed with `eldev'."
  (when-let* ((project-dir
               (locate-dominating-file default-directory "Eldev")))
    (list (list
           :command-name "test:watch"
           :command-line "watchexec --ignore 'flycheck_*' eldev test"
           :display "test:watch"
           :working-dir project-dir)
          (list
           :command-name "lint"
           :command-line "eldev lint"
           :display "lint"
           :working-dir project-dir))))
