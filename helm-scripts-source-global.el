(defvar mmr/helm-scripts-global '(("clone node starter" . mmr/clone-node-starter)))

(defun mmr/clone-node-starter ()
  (let ((name (read-string "name: ")))
    (shell-command (format "git clone ~/projects/starter-node-basic %s"
                           name))))

(defun helm-scripts-source-global (project-dir)
  (helm-build-sync-source "global scripts"
    :candidates mmr/helm-scripts-global
    :action (lambda (fn)
              (funcall fn))))

(provide 'helm-scripts-source-global)
