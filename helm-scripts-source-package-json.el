(require 'helm-scripts-util)

(defun mmr/helm-package-json-scripts--action (script)
  (let* ((script-command (plist-get script :command))
         (script-name (plist-get script :name))
         (project-name (plist-get script :project-name))
         (project-dir (plist-get script :project-dir))
         (compilation-buffer-name-function (lambda (name-of-mode)
                                             (concat "*" script-name "(" project-name ")"
                                                     "*"))))
    (let ((default-directory project-dir))
      (compile (if helm-current-prefix-arg
                   (read-string "> "
                                (concat script-command " "))
                 script-command)))))

(defun helm-scripts-source-package-json ()
  (let ((project-dir (helm-scripts-util--get-project-dir)))
    (when (and project-dir
               (file-exists-p (concat project-dir "package.json")))
      (let* ((config (mmr/get-package-json-script-config project-dir))
             (scripts (plist-get config :scripts)))
        (helm-build-sync-source "package.json"
          :candidates scripts
          :action 'mmr/helm-package-json-scripts--action
          :filtered-candidate-transformer '(helm-adaptive-sort))))))

(defun mmr/get-package-json-script-config (project-dir)
  (with-temp-buffer
    (insert-file-contents (concat project-dir "package.json"))
    (let* ((package-json (json-parse-buffer))
           (project-name (gethash "name" package-json))
           (scripts (gethash "scripts" package-json))
           (script-names '())
           (runner (if (file-exists-p (concat project-dir "yarn.lock"))
                       "yarn"
                     "npm")))
      (maphash (lambda (key value)
                 (let ((command (concat runner " run " key)))
                   (push (cons key (list :command command
                                         :name key
                                         :project-name project-name
                                         :project-dir project-dir))
                         script-names)))
               scripts)
      (list :project-name project-name
            :scripts script-names))))




(provide 'helm-scripts-source-package-json)
