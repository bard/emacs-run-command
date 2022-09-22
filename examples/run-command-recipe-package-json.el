
;; Run a script from the project's package.json file. Supports both npm and yarn.

(defun run-command-recipe-package-json--get-scripts (package-json-file)
  "Extract NPM scripts from `package-json-file'."
  (with-temp-buffer
    (insert-file-contents package-json-file)
    (when-let ((script-hash (gethash "scripts" (json-parse-buffer))))
      (let (scripts '())
        (maphash (lambda (key _value) (push key scripts)) script-hash)
        scripts))))

(defun run-command-recipe-package-json ()
  (when-let* ((project-dir
               (locate-dominating-file default-directory "package.json"))
              (scripts
               (run-command-recipe-package-json--get-scripts (concat project-dir "package.json")))
              (script-runner
               (cond
                ((file-exists-p (concat project-dir "pnpm-lock.yaml")) "pnpm")
                ((file-exists-p (concat project-dir "yarn.lock")) "yarn")
                (t "npm"))))
    (mapcar (lambda (script)
              (list :command-name script
                    :command-line (concat script-runner " run " script)
                    :display script
                    :working-dir project-dir))
            scripts)))

