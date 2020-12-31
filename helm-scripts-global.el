
(defun helm-scripts-global ()
  (list (list :display "clone node starter"
              :name "clone-node-starter"
              :command "git clone ~/projects/starter-node-basic"
              :working-dir default-directory
              :scope-name default-directory)))

(provide 'helm-scripts-global)
