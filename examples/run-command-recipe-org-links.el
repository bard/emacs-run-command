;; Open all URL's under an org heading selected by the user:

(require 'cl-lib)

(defvar run-command-recipe-org-links-buf nil
  "Used to pass the test buffer to the `run-command' 's lisp command, which is
  a lambda and thus oblivious to its lexical? environment.")

(defun run-command-recipe-org-links-buf-get-or-create ()
  (prog1 (if (buffer-live-p run-command-recipe-org-links-buf)
             run-command-recipe-org-links-buf
           (setq run-command-recipe-org-links-buf
                 (get-buffer-create " *run-command-org-links*")))
    (with-current-buffer run-command-recipe-org-links-buf
      (delete-region (point-min) (point-max))
      (org-mode)
      (insert "* home repository

[[https://github.com/bard/emacs-run-command/][emacs-run-command.git]] [[https://github.com/bard/emacs-run-command/][emacs-run-command.git]]

* issue tracker

[[https://github.com/bard/emacs-run-command/issues/]]")))
  )

(defun run-command-recipe-org-links--heading-urls-alist ()
  "List the headings in the test buffer."
  ;; create an org buffer with urls:
  (with-current-buffer (run-command-recipe-org-links-buf-get-or-create)
    (goto-char (point-min))
    (cl-loop
     with heading = nil
     with body = nil
     with tangled-urls = nil
     while (re-search-forward "^[*]+ +" nil t)
     do (setq
         heading (buffer-substring-no-properties (point) (line-end-position))
         body (string-trim (buffer-substring-no-properties (line-end-position)
                                                           (or (outline-next-heading)
                                                               (point-max))))
         tangled-urls (split-string body "\\]\\]\\|\\[\\[" t "[[:space:]\n\t]+"))
     collect (cons heading
                   (cl-loop for tangled-url in tangled-urls
                            collect (car (split-string tangled-url "\\]\\[")))))))

(defun run-command-recipe-org-links ()
  "Make commands to open some links in an org buffer."
  (let* ((heading-url-alist ))
    (cl-loop for (heading . urls) in (run-command-recipe-org-links--heading-urls-alist)
             collect (eval `(list :command-name (format "open %s" ,heading)
                                  :command-line (lambda ()
                                                  (mapcar 'browse-url ',urls)
                                                  (kill-buffer (run-command-recipe-org-links-buf-get-or-create))))))))
