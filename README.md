[![MELPA](https://melpa.org/packages/run-command-badge.svg)](https://melpa.org/#/run-command)

# run-command

Emacs, the text editor where you can read mail and play Tetris, is often cast in opposition to the Unix philosophy of "do one thing well" and "combine programs". It's a false dichotomy. Emacs can do a lot _and_ can be combined usefully with other programs.

`run-command` makes the combination convenient through a simple configuration format and an interaction flow that stays out of your way. Where you'd usually reach for a shell or look for a specialized major mode, with `run-command` you can write a short recipe and get a command that is easy to bring up, invoke, and keep track of, without leaving Emacs.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

Table of Contents:

- [Demo](#demo)
- [Features](#features)
- [Installing](#installing)
- [Quickstart](#quickstart)
- [Invoking](#invoking)
- [Optional configuration](#optional-configuration)
- [Tutorial: adding commands](#tutorial-adding-commands)
  - [Readable command names](#readable-command-names)
  - [Specifying the working directory](#specifying-the-working-directory)
  - [Enabling and disabling depending on context](#enabling-and-disabling-depending-on-context)
  - [Generating commands on the fly](#generating-commands-on-the-fly)

<!-- markdown-toc end -->

## Demo

The screencast below shows using `run-command` to 1) clone a project from a boilerplate, 2) execute a file on every save, and 3) start the test runner.

<p align="center"><img alt="demo" src="./demo.gif"></p>

## Features

- **Unopinionated**: run a command against a word, a file, a project, a service, anything.
- **Minimal cognitive tax**: one key binding, one configuration variable, two sensible defaults.
- **Flexible configuration**: hardcode commands, generate them dynamically based on context, or anything in between.

## Installing

[Available from MELPA](https://melpa.org/#/run-command).

## Quickstart

1. Add a "command recipe" to your init file, for example:

```emacs-lisp
(defun run-command-recipe-local ()
  (list
   (list :command-name "say-hello"
         :command-line "echo Hello, World!")

   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000")

   (when (equal (buffer-name) "README.md")
     (list :command-name "preview-github-readme"
           ;; uses https://github.com/joeyespo/grip
           :command-line "grip --browser --norefresh"))

   (when-let ((word (thing-at-point 'word t)))
     (list :command-name "wordnet-synonyms"
           ;; uses https://wordnet.princeton.edu/documentation/wn1wn
           :command-line (format "wn '%s' -synsn -synsv -synsa -synsr" word)
           :display (format "Look up '%s' synonyms in wordnet" word)))))
```

2. Customize `run-command-recipes` and add `run-command-recipe-local` to the list.

3. Type `M-x run-command RET`.

Read more about [invocation](#invoking), [configuration](#optional-configuration), and [how to add commands](#tutorial-adding-commands), or check out some [recipe examples](./examples).

## Invoking

Type `M-x run-command` or bind `run-command` to a key:

```emacs-lisp
(global-set-key (kbd "C-c c") 'run-command)
```

Or:

```emacs-lisp
(use-package run-command
  :bind ("C-c c" . run-command)
```

When using Helm, you can edit a command before running it by typing `C-u RET` instead of `RET`. (See [issue #1](https://github.com/bard/emacs-run-command/issues) if you can help bring that to Ivy.)

## Optional configuration

By default, commands are run in `compilation-mode`. See [Lightweight external command integration in Emacs via compilation mode](https://massimilianomirra.com/notes/lightweight-external-command-integration-in-emacs-via-compilation-mode/) for some notes on how to make the most of `compilation-mode`. Alternatively (and experimentally), commands can be run in `term-mode` plus `compilation-minor-mode`, especially useful for commands with rich output such as colors, progress bars, and screen refreshes, while preserving `compilation-mode` functionality. Set `run-command-run-method` to `term` and please comment on [issue #2](https://github.com/bard/emacs-run-command/issues/2) if you find issues.

The auto-completion framework is automatically detected. It can be set manually by customizing `run-command-completion-method`.

## Tutorial: adding commands

### Readable command names

To provide a more user-friendly name for a command, use the `:display` property:

```emacs-lisp
(defun run-command-recipe-local ()
  (list
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000"
         :display "Serve directory over HTTP port 8000")))
```

### Specifying the working directory

A command runs by default in the current buffer's directory. You can make it run in a different directory by setting `:working-dir`.

For example, you want to serve the current directory via HTTP, unless you're visiting a file that is somewhere below a `public_html` directory, in which case you want to serve `public_html` instead:

```emacs-lisp
(defun run-command-recipe-local ()
  (list
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000"
         :display "Serve directory over HTTP port 8000"
         :working-dir (let ((project-dir
                             (locate-dominating-file default-directory "public_html")))
                        (if project-dir
                            (concat project-dir "public_html")
                          default-directory)))))
```

See the [Hugo project recipe](examples/run-command-recipe-hugo.el) for a recipe that uses the project's directory for all commands.

### Enabling and disabling depending on context

To disable a command in certain circumstances, return `nil` in its place.

For example, you want to enable a command only when the current buffer is visiting an executable file:

```emacs-lisp
(defun run-command-recipe-local ()
  (let ((buffer-file (buffer-file-name)))
    (list
     (when (and buffer-file (file-executable-p buffer-file))
       (list
        :command-name "run-buffer-file"
        :command-line buffer-file
        :display "Run this buffer's file")))))
```

See the [executable file recipe](examples/run-command-recipe-executables.el) for a variant that also re-runs the file on each save.

See the [Hugo project recipe](examples/run-command-recipe-hugo.el) for a recipe that switches off entirely when you're not in a Hugo project.

### Generating commands on the fly

Recipes are plain old Lisp functions, so they generate commands based on e.g. project setup.

See the [NPM project recipe](examples/run-command-recipe-package-json.el), which uses a JavaScript's project `package.json` file to generate commands, and the [Make project recipe](examples/run-command-recipe-make.el), which does the same for `Makefile` projects.
