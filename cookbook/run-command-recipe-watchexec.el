(defun run-command-recipe-watchexec ()
  "Ask for a command and run it whenever the current file is saved.

Requires `watchexec' (https://watchexec.github.io/) to be installed."
  (when-let ((buffer-file (buffer-file-name)))
    (list
     (list :display (format "Run arbitrary command whenever `%s' is saved"
                            (file-name-nondirectory buffer-file))
           :command-name "watchexec"
           :command-line (lambda ()
                           (let ((command-to-run (shell-quote-argument
                                                  (read-string "Enter command: "))))
                             (format "watchexec --clear --watch '%s' `%s'"
                                     buffer-file command-to-run)))))))
