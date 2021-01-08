
(defun run-command-recipe-executables ()
  (let ((file (buffer-file-name)))
    (when (and file (file-executable-p file))
      (list
       (list
        :command-name "run-buffer-file"
        :command-line file
        :display "Run file associated to buffer")
       (list
        :command-name "run-buffer-file-watch"
        ;; `entr' (http://entrproject.org/) is available via APT, pacman, brew
        :command-line (format "echo %s | entr -c /_" file)
        :display "Run file associated to buffer (re-run on each save)")))))
