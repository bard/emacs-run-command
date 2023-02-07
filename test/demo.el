;; This demo is scripted using director.el: https://github.com/bard/emacs-director
;;
;; To record it:
;;
;;   asciinema rec --overwrite -c 'emacs -Q -nw -l director-bootstrap.el -l demo.el' demo.asciicast
;;   agg --font-size 24 demo.asciicast ../docs/public/demo2.gif

(director-bootstrap
 :user-dir "/tmp/run-command-demo.userdir"
 :packages '(helm ivy async rust-mode)
 :load-path
 '("~/projects/emacs-run-command"
   "~/projects/emacs-director" ;; won't be needed after director is published on Melpa
   ))

(director-run
 :version 1
 :before-start
 (lambda ()
   ;; load required features
   (require 'run-command)
   (require 'helm)
   (require 'term)

   ;; remove distractions
   (setq initial-scratch-message "")
   (menu-bar-mode 'toggle)
   (setq helm-display-header-line nil)
   (set-face-attribute 'helm-source-header nil
                       :background "transparent"
                       :foreground "dim gray"
                       :weight 'normal)
   (set-face-attribute 'helm-match nil
                       :foreground "brightred "
                       :weight 'bold
                       :underline t)

   ;; create a fresh space for each demo run
   (setq demo-directory (make-temp-file "run-command-demo-" t))

   ;; prepare the environment
   (helm-mode 1)

   ;; configure package to be demo'd
   (setq run-command-recipes '(run-command-recipe-example))

   ;; prepare the buffer where most typing will happen
   (emacs-lisp-mode)
   (eldoc-mode -1))

 :delay-between-steps 0.1

 ;; give useful feedback during development
 :on-error (lambda (err) (message "Error while executing director script: %S" err))

 :after-end (lambda () (kill-emacs))

 :typing-style 'human

 :steps
 '( ;; don't rush
   (:wait 1)

   ;; define recipe for creating project
   (:eval (switch-to-buffer "*scratch*"))
   (:type "(defun run-command-recipe-example ()\r")
   (:wait 0.5)
   (:type "(list ")
   (:type "'( :command-name \"Create Rust project\"\r")
   (:wait 0.5)
   (:type ":command-line (lambda () (format \"cargo new %s\" (read-string \"Name: \"))))\r")
   (:type "))")
   (:eval (eval-buffer))

   ;; execute first recipe
   (:wait 1)
   (:eval (dired demo-directory))
   (:wait 1)
   (:type "\M-x")
   (:type "run-command")
   (:wait 0.5)
   (:type [return])
   (:wait 1.5)
   (:type "cre")
   (:wait 1)
   (:type [return])
   (:type "helloworld")
   (:wait 1.5)
   (:type [return])
   (:wait 2)
   (:eval (delete-other-windows))
   (:type "g")
   (:wait 2)

   ;; define second recipe
   (:eval (progn
           (switch-to-buffer "*scratch*")
           (goto-char (point-min))
           (re-search-forward "(list ")
           (forward-sexp)))
   (:wait 2)
   (:type [return])
   (:type "'( :command-name \"Run Rust project in watch mode\"\r")
   (:type ":command-line \"cargo watch --clear -x run\")")
   (:eval (eval-buffer))
   (:wait 2)

   ;; execute second recipe
   (:eval (dired demo-directory))
   (:wait 1)
   (:type [return])
   (:wait 1)
   (:type [down])
   (:wait 0.2)
   (:type [down])
   (:wait 0.2)
   (:type [down])
   (:wait 1)
   (:type [return])
   (:wait 1)
   (:type [return])
   (:wait 1)
   (:type "\M-x")
   (:type "run-command")
   (:wait 0.5)
   (:type [return])
   (:wait 1)
   (:type "run")
   (:wait 1)
   (:type [return])
   (:wait 2)

   ;; modify file
   (:eval (re-search-forward "world" nil t))
   (:wait 2)
   (:call backward-kill-word)
   (:wait 1)
   (:type "Emacs")
   (:wait 2)
   (:call save-buffer)
   (:wait 4)))
