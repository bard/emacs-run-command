# Writing Recipes

## Choosing the name for a recipe

If the name begins with `run-command-recipe-`, the prefix is removed when displaying commands, otherwise the entire function name is used:

```lisp
(defun run-command-recipe-boilerplates ()) ;; shows up as "boilerplates"
(defun my-command-recipe ())               ;; shows up as "my-command-recipe"
```

## Readable command names

You can provide a user-friendly name via the `:display` property:

```lisp /:display/
(defun run-command-recipe-example ()
  (list
   (list :display "Serve current directory over HTTP port 8000"
         :command-name "serve-http-dir"
         :command-line "python3 -m http.server 8000")))
```

## Context-independent commands

The simplest command hardcodes everything:

```lisp
(defun run-command-recipe-example ()
  (list
   (list :command-name "create-webdev-project"
         :command-line "git clone https://github.com/h5bp/html5-boilerplate webdev-project"
         :display "Create a web project from HTML5 boilerplate")))
```

## Context-sensitive commands

A recipe is a plain function, so you can evaluate Lisp code to query the context. For example, to find out the current buffer's file name and pass it to the command:

```lisp /(buffer-file (buffer-file-name))/ / buffer-file)/
(defun run-command-recipe-example ()
  (list
   (when-let ((buffer-file (buffer-file-name)))
     (list :display "Count lines of code"
           :command-name "count-lines-of-code"
           :command-line (format "sloccount '%s'" buffer-file)))))
```

(Note that we use `when-let` to guard against buffers that aren't associated with a file, such as `*scratch*`.)

To find out the word at point and pass it to the command:

```lisp /(word (thing-at-point 'word t)/ / word)/
(defun run-command-recipe-example ()
  (list
   (when-let ((word (thing-at-point 'word t)))
     (list :command-name "wordnet-synonyms"
           :command-line (format "wn '%s' -synsn -synsv -synsa -synsr" word)
           :display (format "Look for '%s' synonyms in wordnet" word)))))
```

## Specifying a different working directory

By default, commands run in the same directory as the buffer from where `run-command` is launched. You can specify a different one via `:working-dir`.

For example, to run a command in the base directory of a project managed with `Makefile`:

```lisp /(project-dir (locate-dominating-file default-directory "Makefile")/ / project-dir)/
(defun run-command-recipe-example ()
  (list
   (when-let ((project-dir (locate-dominating-file default-directory "Makefile")))
     (list :display "Run make with default target"
           :command-name "make-default"
           :command-line "make"
           :working-dir project-dir))))
```

## Toggling individual commands on or off depending on context

To disable a command, return `nil` in its place. `when` and `when-let` are convenient ways of doing so.

For example, to only enable a command when the buffer's file is executable:

```lisp {4}
(defun run-command-recipe-example ()
  (list
   (let ((buffer-file (buffer-file-name)))
     (when (and buffer-file (file-executable-p buffer-file))
       (list
        :command-name "run-buffer-file"
        :command-line buffer-file
        :display "Run this buffer's file")))))
```

## Toggling entire recipes on or off depending on context

To disable an entire list, return `nil` in its place. As for individual commands, `when` and `when-let` are convenient ways of doing so.

For example, to disable the entire recipe when we're not in a JavaScript project:

```lisp {2}
  (defun run-command-recipe-eldev ()
    (when-let ((project-dir (locate-dominating-file default-directory "package.json")))
      (list
       (list :command-name "test:watch"
             :command-line "npm run test:watch"
             :display "test:watch"
             :working-dir project-dir))))
```

(Contrast the position of `when-let` with the previous examples.)

## Launching long-lived commands that re-run on file changes

Utilities such as [watchexec](https://watchexec.github.io/) make it easy to convert a one-off command into a watch-style utility.

For example, to regenerate a PDF whenever you save a file in Markdown or another pandoc-supported format:

```lisp /watchexec/
(defun run-command-recipe-example ()
  (list
   (when-let ((buffer-file (buffer-file-name)))
     (list :display "Generate PDF"
           :command-name "generate-pdf"
           :command-line (format "watchexec --clear --watch '%s' 'pandoc --standalone -t html5 -o /tmp/preview.pdf \'%s\''"
                          buffer-file
                          buffer-file)))))
```

## Generating a recipe from project contents

Entire recipes can be generated dynamically based on a project's contents.

For examples of generation from project management file, see the [package.json recipe](https://github.com/bard/emacs-run-command/tree/master/cookbook/run-command-recipe-package-json.el) or the [Makefile example](https://github.com/bard/emacs-run-command/tree/master/cookbook/run-command-recipe-make.el).

Likewise one may use the contents of a directory:

```lisp
(defun run-command-recipe-project-scripts ()
  (when-let ((file-accessible-directory-p "scripts"))
    (mapcar (lambda (script-name)
              (let ((file (expand-file-name script-name "scripts")))
                (when (and (file-regular-p file)
                           (file-executable-p file))
                  (list :command-line file
                        :command-name script-name))))
            (directory-files "scripts"))))
```

## Overriding the default runner

Normally, the variable `run-command-default-runner` determines how commands are launched (e.g. whether in `term-mode`, `compilation-mode`, etc).

The default runner can be overridden on a per-command basis using the `:runner` property.

You might want to use this if a particular runner glitches on a command's output.

For example:

```lisp {5}
(defun run-command-recipe-sysadmin ()
  (list
   (list :command-name "htop"
         :command-line "htop"
         :runner 'run-command-runner-vterm)))
```

## Hooks

To execute Lisp code just after the command has been launched, assign a function to the `:hook` property, either in the form of a symbol naming a function, or as a lambda.

For example, to enable `compilation-minor-mode`:

```lisp {9}
  (defun run-command-recipe-example ()
    (when-let* ((project-dir
                 (locate-dominating-file default-directory "Eldev")))
      (list (list
             :command-name "lint"
             :command-line "eldev lint"
             :display "lint"
             :runner 'run-command-runner-term
             :hook 'compilation-minor-mode
             :working-dir project-dir))))
```
