;;; -*- lexical-binding: t -*-

(defun run-command-hugo ()
  (let* ((project-dir (locate-dominating-file default-directory
                                              "archetypes")))
    (when project-dir
      (let ((project-name (file-name-nondirectory (directory-file-name project-dir))))
        (list (list :command-name "start:server"
                    :command-line "hugo server --disableFastRender -D --navigateToChanged"
                    :display "start local server (drafts+fastRender)"
                    :scope-name project-name
                    :working-dir project-dir)
              (list :command-name "start:server"
                    :command-line "hugo server"
                    :display "start local server"
                    :scope-name project-name
                    :working-dir project-dir)
              (list :command-name "build"
                    :command-line "hugo"
                    :display "build"
                    :scope-name project-name
                    :working-dir project-dir)
              (list :command-name "validate"
                    :command-line "for i in `fd .html$ public/`; do echo $i; tidy -errors -q $i ; done"
                    :display "validate"
                    :scope-name project-name
                    :working-dir project-dir)
              (list :command-name "update:modules"
                    :command-line "unset HUGO_MODULE_REPLACEMENTS; hugo mod get -u"
                    :display "update modules"
                    :scope-name project-name
                    :working-dir project-dir)
              (list :command-name "post:new"
                    :command-line "hugo new posts/unnamed.md"
                    :display "new post"
                    :scope-name project-name
                    :working-dir project-dir))))))

(provide 'run-command-hugo)
