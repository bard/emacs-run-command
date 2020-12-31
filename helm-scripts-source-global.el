(require 'helm-scripts-util)

(defun helm-scripts-source-global ()
  (let ((scripts (helm-scripts--get-global-scripts)))
    (when scripts
      (let ((candidates (mapcar (lambda (script)
                                  (cons (plist-get script :display) script))
                                scripts)))
        (helm-build-sync-source "global scripts"
          :action 'helm-scripts-util--action
          :candidates candidates
          :filtered-candidate-transformer '(helm-adaptive-sort))))))

(defun helm-scripts--get-global-scripts ()
  (list (list :display "clone node starter"
              :name "clone-node-starter"
              :command "git clone ~/projects/starter-node-basic"
              :working-dir default-directory
              :scope-name default-directory)))

(provide 'helm-scripts-source-global)
