Pick external commands from smart lists that are based on project type, buffer mode, favorite scripts, or anything you fancy, and run them in compilation mode, in as few keystrokes as possible, without memorizing key bindings, autocompleting with
Helm or Ivy.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Table of Contents**

- [Demo](#demo)
- [Installing](#installing)
- [Configuring](#configuring)
- [Invoking](#invoking)
- [Tutorial: adding commands](#tutorial-adding-commands)
  - [Readable command names](#readable-command-names)
  - [Context-aware recipes directory](#context-aware-recipes)
  - [Specifying the working directory](#specifying-the-working-directory)
- [Tuning compilation mode](#tuning-compilation-mode)

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

Customize `run-command-recipes`. A recipe for JavaScript/npm projects is included (`run-command-recipe-package-json`) and two more (`run-command-recipe-make` and `run-command-recipe-hugo`) can be found in the [examples](./examples).

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

## Tutorial: adding commands

Example: you want to serve the current directory over HTTP. Add this to Emacs's init file:

```emacs-lisp
(defun run-command-recipe-local ()
  (list
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000")))
```

And customize `run-command-recipes` to include `run-command-recipe-local`.

### Readable command names

To provide a more user-friendly name for a command, use `:display`:

```emacs-lisp
(defun run-command-recipe-local ()
  (list
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000"
         :display "Serve directory over HTTP port 8000")))
```

### Context-aware recipes

A recipe runs in the context of the current buffer, and can look at the context to decide to what a command should be, or whether it should be available at all.

Example: if a buffer is associated to an executable file, you want to run it:

```emacs-lisp
(defun run-command-recipe-local ()
  (list
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000"
         :display "Serve directory over HTTP port 8000")
   (let ((file (buffer-file-name)))
     (when (and file
                (file-executable-p file))
       (list
        :command-name "run-buffer-file"
        :command-line file
        :display "Run file associated to current buffer")))))
```

When no file is associated to the buffer (e.g. a `*Help*` or dired buffer), or when the file is not executable, the code above simply returns `nil`.

### Specifying the working directory

Example: you want to serve the current directory over HTTP, unless the file you're visiting is somewhere under a `public_html` directory, in which case you want to serve that directory instead.

You would use `locate-dominating-file` to look for a `public_html` ancestor and, if found, make that the `:working-dir`:

```emacs-lisp
(defun run-command-recipe-local ()
  (list
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000"
         :display "Serve directory over HTTP port 8000"
         :working-dir
         (let ((project-dir (locate-dominating-file default-directory "public_html")))
           (if project-dir (concat project-dir "public_html") default-directory)))))
   (let ((file (buffer-file-name)))
     (when (and file
                (file-executable-p file))
       (list
        :command-name "run-buffer-file"
        :command-line file
        :display "Run file associated to current buffer")))))
```

## Tuning compilation mode

Since `run-command` uses compilation mode so heavily, it pays to tune that so as to provide functionality that commands expect, such as color output and between-runs screen clearing. See [Lightweight Emacs integration with external commands via compilation mode](https://massimilianomirra.com/notes/lightweight-emacs-integration-with-external-commands-via-compilation-mode/) for a guide.
