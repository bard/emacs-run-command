(defvar run-command-recipe-dir-locals-fn nil)

(defun run-command-recipe-dir-locals ()
  "Provide dir-local commands, when the dir-local recipe is defined.

This is a lightweight solution for per-project commands for when you
don't want to bring in an external tool like `make' or `npm'.

To define a dir-local recipe, create a `.dir-locals.el' file and add
something like the following (note the leading periods):

  ((nil . ((run-command-recipe-dir-locals-fn
            . (lambda ()
                (list
                 (list :command-name \"say-hello\"
                       :command-line \"echo Hello, world!\")
                 (list :command-name \"deploy\"
                       :command-line \"scripts/deploy.sh\"
                       :working-dir (locate-dominating-file
                                     default-directory
                                     \".git\"))))))))

More about per-directory local variables:
https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html"
  (when run-command-recipe-dir-locals-fn
    (funcall run-command-recipe-dir-locals-fn)))
