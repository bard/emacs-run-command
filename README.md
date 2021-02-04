[![MELPA](https://melpa.org/packages/run-command-badge.svg)](https://melpa.org/#/run-command)

# run-command

Emacs, the text editor where you can read mail and play Tetris, is often cast in opposition to the Unix philosophy of "do one thing well" and "combine programs". It's a false dichotomy. Emacs can do a lot on its own _and_ be combined usefully with other programs.

`run-command` makes the combination convenient through a simple configuration format and an interaction flow that stays out of your way. Where you'd usually reach for a shell or look for a specialized major mode, with `run-command` you can write a short recipe and get a command that is easy to bring up, invoke, and keep track of, without leaving Emacs.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Table of Contents**

- [Demo](#demo)
- [Features](#features)
- [Installation](#installation)
- [Quickstart](#quickstart)
- [Invocation](#invocation)
- [Configuration](#configuration)
- [Examples](#examples)
  - [Count lines of code in the current buffer](#count-lines-of-code-in-the-current-buffer)
  - [Look up synonyms for the word at point](#look-up-synonyms-for-the-word-at-point)
  - [Launch make](#launch-make)
  - [Live-preview PDF from markdown](#live-preview-pdf-from-markdown)
  - [Make all executables from a directory available as commands](#make-all-executables-from-a-directory-available-as-commands)
  - [Advanced examples](#advanced-examples)
- [Tutorial: Adding commands](#tutorial-adding-commands)
  - [Display readable command names](#display-readable-command-names)
  - [Specify the working directory](#specify-the-working-directory)
  - [Toggle depending on context](#toggle-depending-on-context)
  - [Choose a recipe name](#choose-a-recipe-name)
  - [Generate commands on the fly](#generate-commands-on-the-fly)
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

Read more about [invocation](#invocation), [configuration](#configuration), check out some [examples](#examples), or dive into [adding commands](#tutorial-adding-commands).

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

Write recipe functions into your init file as described in the [quickstart](#quickstart) and the [tutorial](#tutorial-adding-commands), then add them via `M-x customize` to the `run-command-recipes` variable. This is the only required configuration.

By default, commands run in `compilation-mode`. See [Lightweight external command integration in Emacs via compilation mode](https://massimilianomirra.com/notes/lightweight-external-command-integration-in-emacs-via-compilation-mode/) for some notes on how to make the most of it. An alternative method (and probably future default) is `term-mode`plus`compilation-minor-mode`, especially useful for commands with rich output such as colors, progress bars, and screen refreshes, while preserving `compilation-mode`functionality. Set`run-command-run-method`to`term` and please comment on [issue #2](https://github.com/bard/emacs-run-command/issues/2) if you find issues.

The auto-completion framework is automatically detected. It can be set manually by customizing `run-command-completion-method`.

## Examples

### Create a project from a boilerplate

Demonstrates: running commands that are independent of context other than the current directory. Requires: git.

```emacs-lisp
(defun run-command-recipe-example ()
  (list :command-name "create-webdev-project"
        :command-line "git clone https://github.com/h5bp/html5-boilerplate webdev-project"
        :display "Create a web project from HTML5 boilerplate"))
```

### Count lines of code in the current buffer

Demonstrates: running a command on the current buffer's file.

```emacs-lisp
(defun run-command-recipe-example ()
  (list
   (when-let ((buffer-file (buffer-file-name)))
     (list :command-name "count-lines-of-code"
           :command-line (format "sloccount '%s'" buffer-file)
           :display "Count lines of code"))))
```

### Look up synonyms for the word at point

Demonstrates: running command with a text object from the current buffer. Requires: [wordnet](https://wordnet.princeton.edu/documentation/wn1wn).

```emacs-lisp
(defun run-command-recipe-example ()
  (list
   (when-let ((word (thing-at-point 'word t)))
     (list :command-name "wordnet-synonyms"
           :command-line (format "wn '%s' -synsn -synsv -synsa -synsr" word)
           :display (format "Look for '%s' synonyms in wordnet" word)))))
```

### Launch make

Demonstrates: running command in arbitrary working directory. Requires: make.

```emacs-lisp
(defun run-command-recipe-example ()
  (list
   (when-let ((project-dir (locate-dominating-file default-directory "Makefile")))
     (list :command-name "make-default"
           :command-line "make"
           :display "Run make with default target"
           :working-dir project-dir))))
```

### Live-preview PDF from markdown

Demonstrates: running command on every save. Requires: [entr](http://entrproject.org/), pandoc, mupdf.

```emacs-lisp
(defun run-command-recipe-example ()
  (list
   (when-let ((buffer-file (buffer-file-name)))
     (list :command-name "live-pdf-preview"
           :command-line
           (format "echo %s | entr -s 'pandoc %s -t ms -o /tmp/preview.pdf && pkill -HUP mupdf || mupdf /tmp/preview.pdf &'"
                   buffer-file buffer-file)
           :display "Live PDF preview"))))
```

### Make all executables from a directory available as commands

Demonstrates: generating command specs dynamically. Requires: a `scripts` directory with some executables in it.

```emacs-lisp
(defun run-command-recipe-example ()
  (when-let ((file-accessible-directory-p "scripts"))
    (mapcar (lambda (script-name)
              (let ((file (expand-file-name script-name "scripts")))
                (when (and (file-regular-p file) (file-executable-p file))
                  (list :command-line file
                        :command-name script-name))))
            (directory-files "scripts"))))
```

### Advanced examples

See the [examples](examples) directory.

## Tutorial: Adding commands

### Display readable command names

To provide a more user-friendly name for a command, use the `:display` property:

```emacs-lisp
(defun run-command-recipe-example ()
  (list
   (list :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000"
         :display "Serve directory over HTTP port 8000")))
```

### Specify the working directory

A command runs by default in the current buffer's directory. You can make it run in a different directory by setting `:working-dir`.

For example, you want to serve the current directory via HTTP, unless you're visiting a file that is somewhere below a `public_html` directory, in which case you want to serve `public_html` instead:

```emacs-lisp
(defun run-command-recipe-example ()
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

See also:

- the [Hugo project recipe](examples/run-command-recipe-hugo.el) for a recipe that uses the project's directory for all commands.

### Toggle depending on context

To disable a command in certain circumstances, return `nil` in its place.

For example, you want to enable a command only when the current buffer is visiting an executable file:

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

See also:

- the [executable file recipe](examples/run-command-recipe-executables.el) for a variant that also re-runs the file on each save.
- the [Hugo project recipe](examples/run-command-recipe-hugo.el) for a recipe that switches off entirely when you're not in a Hugo project.

### Choose a recipe name

You can name a recipe function anything. If the name begins with `run-command-recipe-`, that will be removed when displaying commands.

For example:

```emacs-lisp
(defun run-command-recipe-example ()) ;; displays as "example"
(defun my-command-recipe ())          ;; displays as "my-command-recipe"
```

### Generate commands on the fly

Recipes are plain old Lisp functions, so they can generate commands based on e.g. project setup.

See also:

- the [NPM project recipe](examples/run-command-recipe-package-json.el) generates commands from a project's `package.json` file
- the [Make project recipe](examples/run-command-recipe-make.el) generates commands from a project's `Makefile` (courtesy of [helm-make](https://github.com/abo-abo/helm-make))

## Experimental features

Some features are work-in-progress or it's undecided whether they will make it to the official release. If you'd like to have a peek and help testing them, you can enable them individually.

To enable or disable them, customize `run-command-experiments`.

| name               | description                                                                                                                                                      | status  |
| ------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------- |
| `vterm-run-method` | Run commands in a [vterm](https://github.com/akermu/emacs-libvterm) buffer                                                                                       | active  |
| `static-recipes`   | ~Allow recipes to be defined by variables in addition to functions~. See this [example](examples/run-command-recipe-dir-locals.el) for equivalent functionality. | retired |
