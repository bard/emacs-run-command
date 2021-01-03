Pick external commands from smart lists that are based on project type, buffer mode, favorite scripts, ... anything you want, and run them in compilation mode, with as few keystrokes as possible and without memorizing new bindings. Add commands through functions that return a list of simple command specs. Autocomplete with Helm or Ivy.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Table of Contents**

- [Installing](#installing)
- [Configuring](#configuring)
- [Invoking](#invoking)
- [Adding commands](#adding-commands)
  - [Readable name](#readable-name)
  - [Context-aware lists](#context-aware-lists)

<!-- markdown-toc end -->

## Installing

To be submitted to MELPA. For now, clone repository and add to load path, e.g. with:

```emacs-lisp
(use-package run-command
  :load-path "~/projects/emacs-run-command")
```

## Configuring

Customize `run-command-config` and add command list generators. Some generators are provided with this package:

- `run-command-package-json` for npm projects
- `run-command-hugo` for Hugo sites
- `run-command-make` for Makefiles (requires [helm-make](https://github.com/abo-abo/helm-make) to be installed)

Command list generators are functions that return a list of commands in a simple format, see [Adding commands](#adding-commands) below to add your own.

`run-command` supports [Helm](https://github.com/emacs-helm/helm/) and [ivy](https://github.com/abo-abo/swiper) for completion. It tries to autodetect which one to use; if it gets it wrong, customize `run-config-completion-method`.

## Invoking

Run `M-x run-command` or bind it to a key:

```emacs-lisp
(global-set-key (kbd "C-c c") 'run-command)
```

When using Helm, selecting with `C-u RET` instead of `RET` allows editing the command before it's run. (See #1 if you can help bring that to Ivy.)

## Adding commands

Say you want to serve the current buffer's directory via http. Add this to Emacs's init file:

```emacs-lisp
(defun run-command-local () ;; any name will do
  (list
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000")))
```

And customize `run-command-config` to include `run-command-local`.

Run `M-x run-command` and the command list will include `serve-http-dir`.

### Readable name

To display a more descriptive command name, use `:display`:

```emacs-lisp
(defun run-command-local ()
  (list
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000"
         :display "Serve directory via HTTP")))
```

### Context-aware lists

Say that, when you run the command while on a file somewhere in a `public_html` directory tree, you want to serve the `public_html` directory, not the current directory:

```emacs-lisp
(defun run-command-local ()
  (list
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000"
         :working-dir
         (let ((container (locate-dominating-file default-directory "public")))
           (if container (concat container "public") default-directory)))))
```
