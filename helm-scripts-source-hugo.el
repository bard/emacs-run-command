(require 'helm-scripts-util)

(defun helm-scripts-source-hugo ()
  (let ((scripts (helm-scripts--get-hugo-scripts)))
    (when scripts
      (let ((candidates (mapcar (lambda (script)
                                  (cons (plist-get script :display) script))
                                scripts)))
        (helm-build-sync-source "Hugo"
          :action 'helm-scripts-util--action
          :candidates candidates
          :filtered-candidate-transformer '(helm-adaptive-sort))))))

(defun helm-scripts--get-hugo-scripts ()
  (let* ((project-dir (locate-dominating-file default-directory
                                              "archetypes")))
    (when project-dir
      (let ((project-name (file-name-nondirectory (directory-file-name project-dir))))
        (list (list :display "start local server (drafts+fastRender)"
                    :name "start:server"
                    :command "hugo server --disableFastRender -D --navigateToChanged"
                    :scope-name project-name
                    :working-dir project-dir)
              (list :display "start local server"
                    :name "start:server"
                    :command "hugo server"
                    :scope-name project-name
                    :working-dir project-dir)
              (list :display "build"
                    :name "build"
                    :command "hugo"
                    :scope-name project-name
                    :working-dir project-dir)
              (list :display "validate"
                    :name "validate"
                    :command "for i in `fd .html$ public/`; do echo $i; tidy -errors -q $i ; done"
                    :scope-name project-name
                    :working-dir project-dir)
              (list :display "update modules"
                    :name "update:modules"
                    :command "unset HUGO_MODULE_REPLACEMENTS; hugo mod get -u"
                    :scope-name project-name
                    :working-dir project-dir)
              (list :display "new post"
                    :name "post:new"
                    :command "hugo new posts/unnamed.md"
                    :scope-name project-name
                    :working-dir project-dir))))))

(provide 'helm-scripts-source-hugo)
