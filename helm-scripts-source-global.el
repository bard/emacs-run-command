(defun helm-scripts-source-global ()
  (helm-build-sync-source "global scripts"
    :action 'helm-scripts-util--action
    :candidates (helm-scripts--get-global-scripts default-directory)))

(defun helm-scripts--get-global-scripts (working-dir)
  (mapcar (lambda (script)
            (cons (plist-get script :display) (append script
                                                      (list :working-dir working-dir
                                                            :scope-name working-dir))))
          '((:display "clone node starter" :name "clone-node-starter"
                      :command "git clone ~/projects/starter-node-basic"))))

(provide 'helm-scripts-source-global)
