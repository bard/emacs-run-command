# Quickstart

## Installation

Install from [MELPA](https://melpa.org/#/run-command) with:

```
M-x package-install RET run-command RET
```

Or with `use-package`:

```lisp
(use-package run-command
  :ensure t)
```

## Keybinding

Add a key binding:

```lisp
(global-set-key (kbd "C-c c") 'run-command)

;; Or:

(use-package run-command
  :ensure t
  :bind ("C-c c" . run-command))
```

## Your first recipe

Add a command recipe to your init file or copy one from the [cookbook](./cookbook):

For example:

```lisp
(defun run-command-recipe-example ()
  (list
   ;; Run a simple command
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

   ;; Do something with the word under point
   (when-let ((word (thing-at-point 'word t)))
     (list :command-name "wordnet-synonyms"
           :command-line (format "wn '%s' -synsn -synsv -synsa -synsr" word)
           :display (format "Look up '%s' synonyms in wordnet" word)))))
```

To activate the recipe, execute `M-x customize RET run-command-recipes RET` and add `run-command-recipe-example` to the list.

To run a command from the recipe, press `C-c c` (or whichever key you associated to it) and select the command.

## What next?

- Read more about [using](./usage) `run-command`
- Read more about [configuring](./configuration) it
- Browse the [included recipes](./cookbook)
- [Write your own recipes](./writing-recipes)
