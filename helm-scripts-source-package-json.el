(require 'helm-scripts-util)

(defun helm-scripts-source-package-json ()
  (let ((project-dir (helm-scripts-util--get-project-dir)))
    (when (and project-dir
               (file-exists-p (concat project-dir "package.json")))
      (let* ((scripts (helm-scripts--get-package-json-scripts project-dir)))
        (helm-build-sync-source "package.json"
          :candidates scripts
          :action 'helm-scripts-util--action
          :filtered-candidate-transformer '(helm-adaptive-sort))))))

(defun helm-scripts--get-package-json-scripts (project-dir)
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
                   (push (cons key (list :command command
                                         :name key
                                         :scope-name project-name
                                         :working-dir project-dir))
                         scripts)))
               script-map)
      scripts)))




(provide 'helm-scripts-source-package-json)
