
(defun run-command-package-json ()
  (let ((project-dir (locate-dominating-file default-directory
                                             "package.json")))
    (when project-dir
      (with-temp-buffer
        (insert-file-contents (concat project-dir "package.json"))
        (let* ((package-json (json-parse-buffer))
               (project-name (gethash "name" package-json))
               (script-map (gethash "scripts" package-json))
               (scripts '())
               (runner (if (file-exists-p (concat project-dir "yarn.lock"))
                           "yarn"
                         "npm")))
          (maphash (lambda (key value)
                     (let ((command (concat runner " run " key)))
                       (push (list :command command
                                   :name key
                                   :display key
                                   :scope-name project-name
                                   :working-dir project-dir)
                             scripts)))
                   script-map)
          (message "%S" scripts)
          scripts)))))

(provide 'run-command-package-json)
