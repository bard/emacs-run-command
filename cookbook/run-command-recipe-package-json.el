;;; run-command-recipe-package-json.el --- Recipe for package.json scripts -*- lexical-binding: t -*-

;;; Commentary:

;; Recipe for package.json scripts.

;;; Code:

(require 'subr-x)
(require 'map)
(require 'seq)

(defun run-command-recipe-package-json ()
  "Provide commands to run script from `package.json'.

Automatically detects package manager based on lockfile: npm, yarn, and pnpm."
  (when-let* ((project-dir
               (locate-dominating-file default-directory "package.json"))
              (project-info
               (with-temp-buffer
                 (insert-file-contents
                  (concat project-dir "package.json"))
                 (json-parse-buffer)))
              (package-manager
               (cond
                ((file-exists-p
                  (concat project-dir "pnpm-lock.yaml"))
                 "pnpm")
                ((file-exists-p
                  (concat project-dir "yarn.lock"))
                 "yarn")
                (t
                 "npm")))
              (scripts (map-keys (map-elt project-info "scripts"))))
    (seq-map
     (lambda (script)
       (list
        :command-name script
        :command-line (concat package-manager " run " script)
        :display script
        :working-dir project-dir))
     scripts)))

(provide 'run-command-recipe-package-json)
;;; run-command-recipe-package-json.el ends here
