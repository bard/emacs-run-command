Select external commands from smart lists that are based on project type, buffer mode, favorite scripts, or anything you fancy, and run them in compilation mode, with as few keystrokes as possible and without memorizing key bindings.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Table of Contents**

- [Demo](#demo)
- [Installing](#installing)
- [Configuring](#configuring)
- [Invoking](#invoking)
- [Adding commands](#adding-commands)
  - [Readable name](#readable-name)
  - [Context-aware lists](#context-aware-lists)

<!-- markdown-toc end -->

## Demo

Using `run-command` to create a project from a boilerplate, start a watcher that re-run the main source whenever a file changes, and start the test runner.

![Demo](./demo.gif)

## Installing

To be submitted to MELPA. For now, clone repository and add to load path, e.g.:

```emacs-lisp
(use-package run-command
  :load-path "~/projects/emacs-run-command")
```

## Configuring

Customize `run-command-recipes` and add command list generators. Some generators are provided with this package:

- `run-command-package-json` for npm projects
- `run-command-hugo` for Hugo sites
- `run-command-make` for Makefiles (requires [helm-make](https://github.com/abo-abo/helm-make) to be installed)

Command list generators are functions that return a list of commands in a simple format, see [Adding commands](#adding-commands) below to add your own.

`run-command` supports [Helm](https://github.com/emacs-helm/helm/) and [ivy](https://github.com/abo-abo/swiper) for completion. It tries to autodetect which one to use; if it gets it wrong, customize `run-config-completion-method`.

## Invoking

Run `M-x run-command` or bind it to a key:

```emacs-lisp
(global-set-key (kbd "C-c c") 'run-command)

;; or:

(use-package run-command
  :bind ("C-c c" . run-command)
```

When using Helm, you can edit a command before running by selecting it with `C-u RET` instead of `RET`. (See #1 if you can help bring that to Ivy.)

## Adding commands

Say you want to serve the current buffer's directory via http. Add this to Emacs's init file:

```emacs-lisp
(defun run-command-local () ;; any name will do
  (list
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000")))
```

And customize `run-command-recipes` to include `run-command-local`.

### Readable name

Use `:display` to provide a more descriptive command name. For example:

```emacs-lisp
(defun run-command-local ()
  (list
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000"
         :display "Serve directory via HTTP")))
```

### Context-aware lists

Use `:working-dir` to change the working directory for the command.

For example, if you want to serve `public_html` when the file you're visiting is somewhere in a `public_html` directory tree, and the current directory in all other cases:

```emacs-lisp
(defun run-command-local ()
  (list
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000"
         :working-dir
         (let ((project-dir (locate-dominating-file default-directory "public")))
           (if project-dir (concat project-dir "public") default-directory)))))
```
