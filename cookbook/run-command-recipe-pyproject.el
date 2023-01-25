;;; run-command-recipe-pyproject.el --- Recipe for Poetry pyproject.toml scripts -*- lexical-binding: t -*-

;;; Commentary:

;; Recipe for Poetry pyproject.toml scripts.

;;; Code:

(require 'subr-x)
(require 'seq)

(defun run-command-recipe-pyproject ()
  "Provide commands to run tasks from a poetry `pyproject.toml' file.

See https://pypi.org/project/taskipy/."
  (when-let* ((project-dir
               (locate-dominating-file default-directory "pyproject.toml"))
              (scripts
               (run-command-recipe-pyproject--get-scripts
                (concat project-dir "pyproject.toml")))
              (script-runner "poetry"))
    (seq-map
     (lambda (script)
       (list
        :command-name script
        :command-line (concat script-runner " run task " script)
        :display script
        :working-dir project-dir))
     scripts)))

(defun run-command-recipe-pyproject--get-scripts (pyproject-file)
  "Get names of scripts defined in `PYPROJECT-FILE'."
  (with-temp-buffer
    (insert-file-contents pyproject-file)
    (when (re-search-forward "^\\[tool\\.taskipy\\.tasks\\]$" nil t)
      (let ((scripts '())
            (block-end
             (save-excursion
               (or (and (re-search-forward "^\\[.*\\]$" nil t) (point))
                   (point-max)))))
        (while (re-search-forward "^\\([a-zA-Z_-]+\\)\s*=\s*\\(.+\\)$"
                                  block-end
                                  t)
          (push (match-string-no-properties 1) scripts))
        scripts))))

(provide 'run-command-recipe-pyproject)
;;; run-command-recipe-pyproject.el ends here
