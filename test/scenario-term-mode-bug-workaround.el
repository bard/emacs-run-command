(load-file "setup.el")

(defun run-command-recipe-example ()
  '(( :command-name "run"
      :command-line "python scenario-term-mode-bug-workaround.py")))

(director-run
 :version 1
 :before-start
 (lambda ()
   (require 'run-command)
   (require 'term)
   (setq run-command-run-method 'term)
   (add-to-list 'run-command-recipes 'run-command-recipe-example))
 :steps
 '((:call run-command)
   (:type "example/run")
   (:type [return])
   (:wait 1)
   (:assert (let ((command-output-buffer (window-buffer (nth 1 (window-list)))))
              (with-current-buffer command-output-buffer
                (goto-char (point-min))
                (equal "[----------------------------------------]"
                       (buffer-substring-no-properties (point-min)
                                                       (point-at-eol)))))))
 :log-target '(file . "director.log")
 :delay-between-steps 0.01
 :on-error (lambda () (kill-emacs 1))
 :on-failure (lambda () (kill-emacs 1))
 :after-end (lambda () (kill-emacs)))


