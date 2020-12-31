(defun helm-scripts-source-make (project-dir)
  (when project-dir
    (let ((makefile-dir (locate-dominating-file default-directory
                                                (lambda (dir)
                                                  (file-exists-p (concat dir "Makefile"))))))
      (when makefile-dir
        (let* ((makefile (concat makefile-dir "Makefile"))
               (helm-make-command (helm--make-construct-command nil makefile)) ;; ignoring "arg"
               (targets (helm--make-cached-targets makefile))
               (default-directory (file-name-directory makefile)))
          (delete-dups helm-make-target-history)
          (helm-build-sync-source "targets"
                                        ;            :header-name (lambda (name) (format "%s (%s):" name makefile))
            :candidates targets
            :fuzzy-match helm-make-fuzzy-matching
            :action 'helm--make-action))))))

(provide 'helm-scripts-source-make)
