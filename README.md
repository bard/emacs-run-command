Run external commands from dynamic, configurable lists based on project type, buffer mode, favorite scripts, or anything you want. Autocompletion via Helm or Ivy.

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

Screencast below shows using `run-command` to 1) create a project from a boilerplate, 2) execute the main source on every save, and 3) start the test runner.

![Demo](./demo.gif)

## Installing

Submitted to MELPA ([subscribe to the PR](https://github.com/melpa/melpa/pull/7344) to be notified).

Meanwhile, clone the repository, and add to load path, e.g. with:

```emacs-lisp
(use-package run-command
  :load-path "~/path/to/emacs-run-command")
```

## Configuring

Customize `run-command-recipes`. A recipe for JavaScript/npm projects is included (`run-command-recipe-package-json`) and two more (`run-command-recipe-make` and `run-command-recipe-hugo`) can be found in the [examples](./examples).

The recipe format is simple, and meant to let you quickly throw together lists of useful commands. See [Add commands](#add-commands) below for a guided example.

`run-command` supports [Helm](https://github.com/emacs-helm/helm/) and [Ivy](https://github.com/abo-abo/swiper) for completion. It tries to autodetect which one to use; if it gets it wrong, customize `run-config-completion-method`.

By default, commands are run in `compilation-mode`. Alternatively, you can run them in a `term-mode` buffer with `compilation-minor-mode`; this is especially useful for commands with rich output such as colors, progress bars, screen refreshes, and so on. To enable it, customize `run-command-run-method` setting it to `term`, and please comment on [issue #2](https://github.com/bard/emacs-run-command/issues/2) if you find differences with the `compile` method.

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

(See [examples/run-command-recipe-executables.el](examples/run-command-recipe-executables.el) for a variant that will start the command once, and re-run it every time it's modified.)

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
