
;; This example describes per-directory commands. Useful for simple
;; cases where you don't want to bring in a build tool or task runner
;; such as Make or NPM.

;; 1. In your init file, define a variable that will hold
;; per-directory commands, and a recipe function that will return
;; those commands when available:

(defvar run-command-recipe-dir-locals-command-specs '())

(defun run-command-recipe-dir-locals ()
  run-command-recipe-dir-locals-command-specs)

;; 2. Customize `run-command-recipes' to include
;; `run-command-recipe-dir-locals'.

;; 3. In the directory where you want some commands activated, create
;; a `.dir-locals.el' file and add definitions such as (note the
;; leading periods):

((nil
  . ((run-command-recipe-dir-locals-command-specs
      . (( :command-name "say-hello"
           :command-line "echo hello, world!"))))))

;; This is currently limited to static recipes, i.e. recipes that
;; don't need to toggle or modify commands dynamically. Commands will
;; run in the directory of the buffer from which they are called. More
;; sophisticated behavior (such as substituing variable or selecting
;; the working directory) can be attained by pre-processing commands
;; inside the `run-command-recipe-dir-locals' function.  More about
;; per-directory local variables:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html

