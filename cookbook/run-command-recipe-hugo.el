(defun run-command-recipe-hugo ()
  "Provide commands to work with a Hugo web site.

Honor `<projectroot>/.env' if present.
"
  (let* ((dotenv-snippet (concat "set -a; test -f .env && source .env; set +a;"))
         (project-dir (locate-dominating-file default-directory
                                              "archetypes")))
    (when project-dir
      (let ((project-name (file-name-nondirectory
                           (directory-file-name project-dir))))
        (list
         (list :command-name "hugo:server:with-drafts"
               :command-line (concat
                              dotenv-snippet
                              "hugo server --disableFastRender -D --navigateToChanged")
               :display "Start local preview server (drafts+fastRender)"
               :working-dir project-dir)
         (list :command-name "hugo:server"
               :command-line (concat dotenv-snippet "hugo server")
               :display "Start local preview server"
               :working-dir project-dir)
         (list :command-name "build"
               :command-line (concat dotenv-snippet "hugo")
               :display "Build"
               :working-dir project-dir)
         (list :command-name "post:new"
               :command-line (concat dotenv-snippet "hugo new posts/unnamed.md")
               :display "New post"
               :working-dir project-dir))))))
