
(defun run-command-recipe-hugo ()
  (let* ((project-dir (locate-dominating-file default-directory
                                              "archetypes")))
    (when project-dir
      (let ((project-name (file-name-nondirectory (directory-file-name project-dir))))
        (list
         (list :command-name "start:server"
               :command-line "hugo server"
               :display "Start local preview server"
               :working-dir project-dir)
         (list :command-name "start:server"
               :command-line "hugo server --disableFastRender -D --navigateToChanged"
               :display "Start local preview server (drafts+fastRender)"
               :working-dir project-dir)
         (list :command-name "build"
               :command-line "hugo"
               :display "Build"
               :working-dir project-dir)
         (list :command-name "post:new"
               :command-line "hugo new posts/unnamed.md"
               :display "New post"
               :working-dir project-dir))))))


