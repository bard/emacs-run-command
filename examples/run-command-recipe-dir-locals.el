
;; This example describes how to have per-directory commands. Useful
;; for simple cases where you don't want to bring in a build tool or
;; task runner such as Make or NPM.

;; 1. Add this to your init file:

(defvar run-command-recipe-dir-locals nil)

;; 2. Customize `run-command-recipes' so that it includes
;; `run-command-recipe-dir-locals':

;; 3. Cretae a `.dir-locals.el' file in the directory where you want some
;; commands activated, with this content;

((nil
  . ((run-command-recipe-dir-locals
      . (( :command-name "say-hello"
           :command-line "echo hello, world!"))))))

;; (Note the leading periods.)

;; This is currently limited to static recipes, i.e. recipes that
;; don't need on Lisp to switch on/off or modify command lines
;; depending on context.
;;
;; Read more about per-directory local variables:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html

