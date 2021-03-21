[![MELPA](https://melpa.org/packages/run-command-badge.svg)](https://melpa.org/#/run-command)

# run-command

Emacs, the text editor where you can read mail and play Tetris, is often cast in opposition to the Unix philosophy of "do one thing well" and "combine programs". It's a false dichotomy. Emacs can do a lot on its own _and_ be combined usefully with other programs.

`run-command` makes the combination convenient through a simple configuration format and an interaction flow that stays out of your way. Where you'd usually reach for a shell or look for a specialized major mode, with `run-command` you write a short recipe and obtain a command that is easy to bring up, invoke, and keep track of, without leaving Emacs.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Table of Contents**

- [Demo](#demo)
- [Features](#features)
- [Installation](#installation)
- [Quickstart](#quickstart)
- [Invocation](#invocation)
- [Configuration](#configuration)
- [Examples](#examples)
- [Cookbook](#cookbook)
  - [Choose a recipe name](#choose-a-recipe-name)
  - [Display readable command names](#display-readable-command-names)
  - [Run a command that is independent of the current context](#run-a-command-that-is-independent-of-the-current-context)
  - [Run a command that uses data from the current context](#run-a-command-that-uses-data-from-the-current-context)
  - [Specify the working directory](#specify-the-working-directory)
  - [Toggle a command depending on context](#toggle-a-command-depending-on-context)
  - [Launch long-lived commands that re-run on file changes](#launch-long-lived-commands-that-re-run-on-file-changes)
  - [Generate commands from the context](#generate-commands-from-the-context)
- [API Reference](#api-reference)
- [Experimental features](#experimental-features)

<!-- markdown-toc end -->

## Demo

The screencast below shows using `run-command` to 1) clone a project from a boilerplate, 2) execute a file on every save, and 3) start the test runner.

<p align="center"><img alt="demo" src="./demo.gif"></p>

## Features

- **Unopinionated**: run a command against a word, a file, a project, a service, anything.
- **Minimal cognitive tax**: one key binding, one configuration variable.
- **Flexible configuration**: hardcode commands, generate them dynamically based on context, or anything in between.

## Installation

[Available from MELPA](https://melpa.org/#/run-command).

## Quickstart

1. Add a "command recipe" to your init file, for example:

```emacs-lisp
(defun run-command-recipe-example ()
  (list
   ;; Arbitrary command
   (list :command-name "say-hello"
         :command-line "echo Hello, World!")

   ;; Do something with current directory
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000"
         :display "Serve current directory over HTTP")

   ;; Do something with current file if it's a README
   ;; (uses https://github.com/joeyespo/grip)
   (when (equal (buffer-name) "README.md")
     (list :command-name "preview-github-readme"
           :command-line "grip --browser --norefresh"
           :display "Preview GitHub README"))

   ;; Do something with current file if it's executable
   (let ((buffer-file (buffer-file-name)))
     (when (and buffer-file
                (file-executable-p buffer-file))
       (list :command-name "run-buffer-file"
             :command-line buffer-file
             :display "Run this buffer's file")))

   ;; Do something with current word
   ;; (uses https://wordnet.princeton.edu/documentation/wn1wn)
   (when-let ((word (thing-at-point 'word t)))
     (list :command-name "wordnet-synonyms"
           :command-line (format "wn '%s' -synsn -synsv -synsa -synsr" word)
           :display (format "Look up '%s' synonyms in wordnet" word)))))
```

2. Type `M-x customize RET run-command-recipes RET` and add `run-command-recipe-example` to the list.

3. Type `M-x run-command RET`.

Read more about [invocation](#invocation), [configuration](#configuration), look at some advanced [examples](#examples), or start writing your own commands by following the [cookbook](#cookbook).

## Invocation

Type `M-x run-command` or bind `run-command` to a key:

```emacs-lisp
(global-set-key (kbd "C-c c") 'run-command)
```

Or:

```emacs-lisp
(use-package run-command
  :bind ("C-c c" . run-command))
```

When completing via Helm or Ivy, you can edit a command before running it by typing `C-u RET` instead of `RET`.

## Configuration

Write recipe functions into your init file as described in the [quickstart](#quickstart) and the [cookbook](#cookbook), then add them via `M-x customize` to the `run-command-recipes` variable. This is the only required configuration.

By default, commands run in `compilation-mode`. See [Lightweight external command integration in Emacs via compilation mode](https://massimilianomirra.com/notes/lightweight-external-command-integration-in-emacs-via-compilation-mode/) for some notes on how to make the most of it. An alternative method (and probably future default) is `term-mode` plus `compilation-minor-mode`, especially useful for commands with rich output such as colors, progress bars, and screen refreshes, while preserving `compilation-mode` functionality. Set `run-command-run-method` to `term` and please comment on [issue #2](https://github.com/bard/emacs-run-command/issues/2) if you find issues.

The auto-completion framework is automatically detected. It can be set manually by customizing `run-command-completion-method`.

## Examples

- [Define and run per-directory commands](examples/run-command-recipe-dir-locals.el)
- [Run current buffer's file](examples/run-command-recipe-executables.el), optionally re-running on save
- [Run package.json scripts](examples/run-command-recipe-package-json.el)
- [Run Makefile targets](examples/run-command-recipe-make.el)
- [Run commands to manage a Hugo blog](examples/run-command-recipe-hugo.el) (also shows how to support `.env` files)

## Cookbook

### Choose a recipe name

A recipe function can have any name. If the name begins with the `run-command-recipe-` prefix, the prefix is removed when displaying commands:

```emacs-lisp
(defun run-command-recipe-boilerplates ()) ;; shows as "boilerplates"
(defun my-command-recipe ())               ;; shows as "my-command-recipe"
```

### Display readable command names

Provide a user-friendly name via the `:display` property:

```emacs-lisp
(defun run-command-recipe-example ()
  (list
   (list :display "Serve current directory over HTTP port 8000"
         :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000")))
```

### Run a command that is independent of the current context

The simplest command is one that hardcodes everything, without inspecting the context:

```emacs-lisp
(defun run-command-recipe-example ()
  (list
   (list :command-name "create-webdev-project"
         :command-line "git clone https://github.com/h5bp/html5-boilerplate webdev-project"
         :display "Create a web project from HTML5 boilerplate")))
```

### Run a command that uses data from the current context

Run a command that uses the buffer's file:

```emacs-lisp
(defun run-command-recipe-example ()
  (list
   (when-let ((buffer-file (buffer-file-name)))
     (list :display "Count lines of code"
           :command-name "count-lines-of-code"
           :command-line (format "sloccount '%s'" buffer-file)))))
```

Run a command that uses the word at point (requires [wordnet](https://wordnet.princeton.edu/documentation/wn1wn)):

```emacs-lisp
(defun run-command-recipe-example ()
  (list
   (when-let ((word (thing-at-point 'word t)))
     (list :command-name "wordnet-synonyms"
           :command-line (format "wn '%s' -synsn -synsv -synsa -synsr" word)
           :display (format "Look for '%s' synonyms in wordnet" word)))))
```

### Specify the working directory

Commands run by default in the current buffer's directory. Specify a different directory via `:working-dir`:

```emacs-lisp
(defun run-command-recipe-example ()
  (list
   (when-let ((project-dir (locate-dominating-file default-directory "Makefile")))
     (list :display "Run make with default target"
           :command-name "make-default"
           :command-line "make"
           :working-dir project-dir))))
```

The [Hugo example](examples/run-command-recipe-hugo.el) shovws how to use a project's directory for all commands. commands.

### Toggle a command depending on context

Disable a command in certain circumstances by returning `nil` in its place (as in most of these examples):

```emacs-lisp
(defun run-command-recipe-example ()
  (list
   (let ((buffer-file (buffer-file-name)))
     (when (and buffer-file (file-executable-p buffer-file))
       (list
        :command-name "run-buffer-file"
        :command-line buffer-file
        :display "Run this buffer's file")))))
```

The [executable file example](examples/run-command-recipe-executables.el) also re-runs the file on each save, and the [Hugo example](examples/run-command-recipe-hugo.el) switches off entirely when you're not in a Hugo project.

### Launch long-lived commands that re-run on file changes

Utilities such as [entr](http://entrproject.org/), [watchman-make](https://manpages.debian.org/testing/python3-pywatchman/watchman-make.1.en.html), [nodemon](https://github.com/remy/nodemon) and so on make it easy to convert a one-off command into an watch-style utility:

```emacs-lisp
(defun run-command-recipe-example ()
  (list
   (when-let ((buffer-file (buffer-file-name)))
     (list :display "Live PDF preview"
           :command-name "live-pdf-preview"
           :command-line
           (format "echo %s | entr -s 'pandoc %s -t ms -o /tmp/preview.pdf && pkill -HUP mupdf || mupdf /tmp/preview.pdf &'"
                   buffer-file buffer-file)))))
```

### Generate commands from the context

Read project settings (such as Makefiles) or directory contents (such as executables within a `scripts` directory) to generate commands specific to the project:

```emacs-lisp
(defun run-command-recipe-example ()
  (when-let ((file-accessible-directory-p "scripts"))
    (mapcar (lambda (script-name)
              (let ((file (expand-file-name script-name "scripts")))
                (when (and (file-regular-p file)
                           (file-executable-p file))
                  (list :command-line file
                        :command-name script-name))))
            (directory-files "scripts"))))
```

The [NPM example](examples/run-command-recipe-package-json.el) generates commands from a project's `package.json` file, and the [Make example](examples/run-command-recipe-make.el) generates commands from a project's `Makefile` (courtesy of [helm-make](https://github.com/abo-abo/helm-make)).

## API reference

<!-- autodoc-api-start - Don't edit. Run M-x autodoc-refresh-markdown
-->

### Command: `run-command`

Pick a command from a context-dependent list, and run it.

The command list is generated by running the functions configured in
`run-command-recipes`.

### Variable: `run-command-recipes`

List of functions that will produce command lists.

Each function is called without arguments and must return a list of property
lists, where each property list represents a command and has the following
format:

- `:command-name` (string, required): A name for the command, used
  to generate the output buffer name, as well as a fallback in case
  `:display` isn't provided
- `:command-line` (string or function, required): The command
  line that will be executed. Can be a function to e.g. further
  query the user, and should return a string.
- `:display` (string, optional): A descriptive name that will
  be displayed to the user.
- `:working-dir` (string, optional): Directory to run the command
  in. If not given, defaults to `default-directory`.

### Variable: `run-command-run-method`

Run strategy.

- `compile` (default): display command output in a `compilation-mode` buffer
- `term`: display command output in a `term-mode` buffer

### Variable: `run-command-completion-method`

Completion framework to use to select a command.

- `autodetect` (default): pick helm, ivy, or none, based on active
  global completion mode
- `helm`: force use helm
- `ivy`: force use ivy
- `completing-read`: force use `completing-read`

<!-- autodoc-api-end -->

## Experimental features

Some features are work-in-progress or it's undecided whether they should form part of the official release, so they are switched off by default. If you'd like to have a peek and help testing them, you can enable them individually with:

```emacs-lisp
(setq run-command-experiments '(experiment-1 experiment-2))
```

| name                                            | status  | description                                                                                                                                                      |
| ----------------------------------------------- | ------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `run-command-experiment-function-command-lines` | retired | ~Allow specifying command line via function~ Integrated.                                                                                                         |
| `run-command-experiment-vterm-run-method`       | active  | Run commands in a [vterm](https://github.com/akermu/emacs-libvterm) buffer                                                                                       |
| `static-recipes`                                | retired | ~Allow recipes to be defined by variables in addition to functions~. See this [example](examples/run-command-recipe-dir-locals.el) for equivalent functionality. |
| `run-command-experiment-lisp-commands`          | active  | Run inline Lisp functions in addition to external commands                                                                                                       |

<!-- Local Variables: -->
<!-- autodoc-markdown-headline-level: 3 -->
<!-- autodoc-filter: (run-command run-command-recipes run-command-run-method run-command-completion-method) -->
<!-- End: -->
