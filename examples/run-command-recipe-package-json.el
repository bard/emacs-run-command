
;; Run an NPM script from the project's package.json file.

(defun run-command-recipe-package-json ()
  "Extract NPM scripts for `run-command' from the current project's package.json."
  (let ((project-dir (locate-dominating-file default-directory
                                             "package.json")))
    (when project-dir
      (with-temp-buffer
        (insert-file-contents (concat project-dir "package.json"))
        (let* ((package-json (json-parse-buffer))
               (script-map (gethash "scripts" package-json))
               (scripts '())
               (runner (if (file-exists-p (concat project-dir "yarn.lock"))
                           "yarn"
                         "npm")))
          (maphash (lambda (key _value)
                     (let ((command (concat runner " run " key)))
                       (push (list :command-name key
                                   :command-line command
                                   :display key
                                   :scope-name (abbreviate-file-name project-dir)
                                   :working-dir project-dir)
                             scripts)))
                   script-map)
          scripts)))))

