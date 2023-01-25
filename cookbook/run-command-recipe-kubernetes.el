;;; run-command-recipe-kubernetes.el --- Recipe for Kubernetes commands -*- lexical-binding: t -*-

;;; Commentary:

;; Recipe for Kubernetes commands.

;;; Code:

(defun run-command-recipe-kubernetes ()
  "Provide commands to work on Kubernetes manifests."
  (when-let ((file-name (buffer-file-name)))
    (when (and file-name
               (equal (file-name-extension file-name) "yaml")
               (save-excursion
                 (goto-char (point-min))
                 (looking-at "\\(---\n\\)?apiVersion:")))
      (list
       (list
        :command-name "kubectl:apply"
        :command-line (format "kubectl apply -f %s" file-name)
        :display
        (format "Apply `%s' to current k8s context"
                (file-name-nondirectory file-name)))))))

(provide 'run-command-recipe-kubernetes)
;;; run-command-recipe-kubernetes.el ends here
