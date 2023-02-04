;;; run-command-recipe-make.el --- Recipe for Makefile targets -*- lexical-binding: t -*-

;;; Commentary:

;; Recipe for Makefile targets.

;;; Code:

(require 'seq)
(require 'subr-x)

(defun run-command-recipe-make ()
  "Provide commands to run Makefile targets.

Requires `helm-make' (https://github.com/abo-abo/helm-make) to
read Makefile targets, but does not require `helm' and can be
used with any of the selectors supported by `run-command'."

  (when (require 'helm-make nil t)
    (when-let* ((project-dir
                 (locate-dominating-file default-directory "Makefile"))
                (makefile (concat project-dir "Makefile"))
                (targets (helm--make-cached-targets makefile)))
      (seq-map
       (lambda (target)
         (list
          :command-name target
          :command-line (concat "make " target)
          :display target
          :working-dir project-dir))
       targets))))

(provide 'run-command-recipe-make)
;;; run-command-recipe-make.el ends here
