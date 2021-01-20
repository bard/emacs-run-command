
(defun run-command-recipe-hugo ()
  (let* ((dotenv-snippet (concat "set -a; test -f .env && source .env; set +a;"))
         (project-dir (locate-dominating-file default-directory
                                              "archetypes")))
    (when project-dir
      (let ((project-name (file-name-nondirectory
                           (directory-file-name project-dir))))
        (list
         ;; Next two commands have the same `:command-name' and thus are mutually
         ;; exclusive — running one will ask to stop the other first.
         (list :command-name "hugo-server"
               ;; Read from `.env' if available (e.g. for HUGO_MODULE_REPLACEMENTS)
               :command-line (concat dotenv-snippet "hugo server")
               :display "Start local preview server"
               :working-dir project-dir)         
         (list :command-name "hugo-server"
               :command-line (concat
                              dotenv-snippet
                              "hugo server --disableFastRender -D --navigateToChanged")
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
