;; Generic hot reload. Pick a command and it will be re-run whenever the current
;; file is saved. Requires entr(1)

(defun run-command-recipe-hot-reload ()
  (let ((buffer-file (buffer-file-name)))
    (when buffer-file
      (list
       (list :display (format "Run arbitrary command whenever `%s' is saved"
                              (file-name-nondirectory buffer-file))
             :command-name "run-command-hot-reload"
             :command-line (lambda ()
                             (format "echo %s | entr -r -n -c -s %s"
                                     (buffer-file-name)
                                     (shell-quote-argument
                                      (read-string "Enter command: ")))))))))
