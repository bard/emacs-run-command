Pick external commands from smart lists that are based on project type, buffer mode, favorite scripts, or anything you fancy, and run them in compilation mode, in as few keystrokes as possible, without memorizing key bindings, autocompleting with
Helm or Ivy.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Table of Contents**

- [Demo](#demo)
- [Installing](#installing)
- [Configuring](#configuring)
- [Invoking](#invoking)
- [Adding commands](#adding-commands)
  - [Readable command names](#readable-command-names)
  - [Context-aware lists, and specifying a working directory](#context-aware-lists-and-specifying-a-working-directory)

<!-- markdown-toc end -->

## Demo

Below, using `run-command` to 1) create a project from a boilerplate, 2) start a file watcher runs the main file upon file changes, and 3) start the test runner.

![Demo](./demo.gif)

## Installing

To be submitted to MELPA soon. For now, clone repository, and add to load path, e.g.:

```emacs-lisp
(use-package run-command
  :load-path "~/projects/emacs-run-command")
```

## Configuring

Customize `run-command-recipes`. A recipe for JavaScript/npm projects in included (`run-command-package-json`) and two more (`run-command-make` and `run-command-hugo`) can be found in the [examples](./examples).

The recipe format is intentionally simple, it's meant to let you add commands easily, rather than pull opaque collections from a repository. See [Add commands](#add-commands) below for a guided example.

`run-command` supports [Helm](https://github.com/emacs-helm/helm/) and [Ivy](https://github.com/abo-abo/swiper) for completion. It tries to autodetect which one to use; if it gets it wrong, customize `run-config-completion-method`.

## Invoking

Run `M-x run-command` or bind it to a key:

```emacs-lisp
(global-set-key (kbd "C-c c") 'run-command)

;; or:

(use-package run-command
  :bind ("C-c c" . run-command)
```

When using Helm, you can edit a command before running by selecting it with `C-u RET` instead of `RET`. (See [#1](https://github.com/bard/emacs-run-command/issues) if you can help bring that to Ivy.)

## Adding commands

Example: you want to serve the current directory via HTTP. Add this to Emacs's init file:

```emacs-lisp
(defun run-command-local () ;; any name will do
  (list
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000")))
```

And customize `run-command-recipes` to include `run-command-local`.

### Readable command names

Use `:display` to provide a more descriptive name for a command:

```emacs-lisp
(defun run-command-local ()
  (list
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000"
         :display "Serve directory via HTTP")))
```

### Context-aware lists, and specifying a working directory

The recipe runs in the context of the current buffer, and can inquire about its context in whichever way makes sense (including different ways for different commands).

Example: you want to serve the current directory, unless the file you're visiting is somewhere under a `public_html` directory, in which case you want to serve that directory instead. You would use `locate-dominating-file` to look for a `public_html` ancestor, and assign it to `:working-dir` if found:

```emacs-lisp
(defun run-command-local ()
  (list
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000"
         :working-dir
         (let ((project-dir (locate-dominating-file default-directory "public_html")))
           (if project-dir (concat project-dir "public_html") default-directory)))))
```
