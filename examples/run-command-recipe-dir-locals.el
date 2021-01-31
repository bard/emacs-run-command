;; EXPERIMENTAL. This example describes how to have per-directory
;; commands. Useful for simple cases where you don't want to bring in
;; a build tool or task runner such as Make or NPM.
;;
;; 1. Enable the static recipe experiment:

(run-command--enable-experiments '(static-recipes))

;; 2. Define an empty static recipe (it will be filled in later):

(defvar run-command-recipe-dir-locals nil)

;; 3. Customize `run-command-recipes' so that it includes
;; `run-command-recipe-dir-locals'.
;;
;; 3. Create a `.dir-locals.el' file in the directory where you want
;; some commands activated, containing the following:

((nil
  . ((run-command-recipe-dir-locals
      . (( :command-name "say-hello"
           :command-line "echo hello, world!"))))))

;; (Note the leading periods.)
;;
;; This is limited to static recipes, i.e. those recipes that don't
;; need to switch or modify commands dynamically.
;;
;; Read more about per-directory local variables:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html

