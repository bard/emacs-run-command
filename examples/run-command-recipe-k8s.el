(defun run-command-recipe-k8s ()
  (let ((file-name (buffer-file-name)))
    (when (and file-name
               (equal (file-name-extension file-name) "yaml")
               (save-excursion
                 (goto-char (point-min))
                 (looking-at "\\(---\n\\)?apiVersion:")))
      (list
       (list :command-name "kubectl-apply"
             :command-line (format "kubectl apply -f %s" file-name)
             :display (format "Apply `%s' to current k8s context"
                              (file-name-nondirectory file-name)))))))
