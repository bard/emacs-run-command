;; Bootstrap:

(director-bootstrap
 :user-dir "/tmp/emacs-run-command-tests"
 :packages '(helm ivy async)
 :load-path '("~/projects/emacs-run-command"
              "~/projects/emacs-director" ;; won't be needed after director is published on Melpa
              ))

;; Fixtures:

(defun run-command-recipe-example ()
  '(( :command-name "react-app"
      :command-line "yarn create react-app --template typescript react-app"
      :display "Create React app")
    ( :command-name "emacs-package"
      :command-line "git clone https://github.com/bard/emacs-package-boilerplate"
      :display "Create Emacs package")
    ( :command-name "node-project"
      :command-line "git clone https://github.com/bard/starter-node-basic starter-node-basic && yarn --cwd $_ install"
      :display "Create NodeJS project")
    ( :command-name "serve-http-dir"
      :command-line "python3 -m http.server 8000"
      :display "Serve current directory over HTTP")
    ( :command-name "say-hello"
      :command-line "echo Hello, world!"
      :display "Say hello")))
