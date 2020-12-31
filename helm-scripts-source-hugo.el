(require 'helm-scripts-util)

(defun helm-scripts-source-hugo ()
  (let ((project-dir (helm-scripts-util--get-project-dir)))
    (when (and project-dir
               (file-exists-p (concat project-dir "archetypes")))
      (let ((project-name (file-name-nondirectory (directory-file-name project-dir))))
        (helm-build-sync-source "Hugo"
          :action 'mmr/helm-package-json-scripts--action
          :candidates (list (cons "start local server (drafts+fastRender)" (list :name "start:server"
                                                                                 :command "hugo server --disableFastRender -D --navigateToChanged"
                                                                                 :project-name project-name
                                                                                 :project-dir project-dir))
                            (cons "start local server" (list :name "start:server"
                                                             :command "hugo server"
                                                             :project-name project-name
                                                             :project-dir project-dir))
                            (cons "build" (list :name "build"
                                                :command "hugo"
                                                :project-name project-name
                                                :project-dir project-dir))
                            (cons "validate" (list :name "validate"
                                                   :command "for i in `fd .html$ public/`; do echo $i; tidy -errors -q $i ; done"
                                                   :project-name project-name
                                                   :project-dir project-dir))
                            (cons "update modules" (list :name "update:modules"
                                                         :command "unset HUGO_MODULE_REPLACEMENTS; hugo mod get -u"
                                                         :project-name project-name
                                                         :project-dir project-dir))
                            (cons "new post" (list :name "post:new"
                                                   :command "hugo new posts/unnamed.md"
                                                   :project-name project-name
                                                   :project-dir project-dir))))))))

(provide 'helm-scripts-source-hugo)
