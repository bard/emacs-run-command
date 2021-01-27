
;; Run the current buffer's file, if executable. Optionally, re-run it on every
;; save (requires `entr' from http://entrproject.org/, available via apt,
;; pacman, brew).

(defun run-command-recipe-executables ()
  (let* ((buffer-file (buffer-file-name))
         (executable-p (and buffer-file (file-executable-p buffer-file))))
    (list
     (when executable-p
       (list
        :command-name "run-buffer-file"
        :command-line buffer-file
        :display "Run this buffer's file"))
     (when executable-p
       (list
        :command-name "run-buffer-file-watch"
        :command-line (format "echo %s | entr -c /_" buffer-file)
        :display "Run this buffer's file (re-run on each save)")))))


