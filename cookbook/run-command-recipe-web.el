(defun run-command-recipe-web ()
  (list
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000 --bind localhost"
         :display "Serve current dir via HTTP on port 8000")
   (list :command-name "serve-http-dir-watch"
         ;; requires python-livereload
         :command-line "livereload --host 127.0.0.1 --port 8000"
         :display "Serve current dir via HTTP on port 8000 (reload on save)")))
