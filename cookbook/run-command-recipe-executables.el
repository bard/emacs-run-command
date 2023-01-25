;;; run-command-recipe-executables.el --- Recipe for commands related to working on scripts -*- lexical-binding: t -*-

;;; Commentary:

;; Recipe for commands related to working on scripts.

;;; Code:

(defun run-command-recipe-executables ()
  "Provide commands relevant to executable scripts.

Requires `watchexec' (https://watchexec.github.io/) to be installed."
  (let* ((buffer-file (buffer-file-name))
         (executable-p (and buffer-file (file-executable-p buffer-file))))
    (list
     (when executable-p
       (list
        :command-name "run-buffer-file"
        :command-line buffer-file
        :display "Run this buffer's file once"))
     (when executable-p
       (list
        :command-name "run-buffer-file-watch"
        :command-line
        (format "watchexec --clear --watch %s %s"
                buffer-file
                buffer-file)
        :display "Run this buffer's file and re-run on each save")))))

(provide 'run-command-recipe-executables)
;;; run-command-recipe-executables.el ends here
