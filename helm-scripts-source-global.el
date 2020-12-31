(defvar helm-scripts-global '(("clone node starter" . mmr/clone-node-starter)))

(defun mmr/clone-node-starter ()
  (let ((name (read-string "name: ")))
    (shell-command (format "git clone ~/projects/starter-node-basic %s"
                           name))))

(defun helm-scripts-source-global ()
  (helm-build-sync-source "global scripts"
    :candidates helm-scripts-global
    :action (lambda (fn)
              (funcall fn))))

(provide 'helm-scripts-source-global)
