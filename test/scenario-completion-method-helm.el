(load-file "setup.el")

(director-run
 :version 1
 :before-start
 (lambda ()
   (require 'helm)
   (require 'helm-config)
   (require 'run-command)
   (helm-mode 1)
   (add-to-list 'run-command-recipes 'run-command-recipe-example))
 :steps
 '((:call run-command)
   (:wait 0.25) ;; apparently, helm opens window asynchronously
   (:assert (length= (window-list) 3))
   (:type "say")
   (:assert (let ((window-2-buffer (window-buffer (nth 2 (window-list)))))
              (with-current-buffer window-2-buffer
                (equal
                 (buffer-substring-no-properties (point-min) (point-max))
                 "example\nSay hello\n"))))
   (:type [return])
   (:assert (let ((window-1-buffer (window-buffer (nth 1 (window-list)))))
              (with-current-buffer window-1-buffer
                (and (string-match "^say-hello" (buffer-name))
                     (re-search-forward "Hello, world!" nil t))))))
 :log-target '(file . "director.log")
 :delay-between-steps 0.01
 :on-error (lambda () (kill-emacs 1))
 :on-failure (lambda () (kill-emacs 1))
 :after-end (lambda () (kill-emacs)))
