# Configuration

## Recipes

1. Write recipe functions into your init file as described in the [quickstart](./quickstart); or load or copy them from the [cookbook](./cookbook).
2. Execute `M-x customize-variable RET run-command-recipes` to activate them.

This is the only required configuration.

## Runner

By default, commands run in `term-mode`. You can modify this by executing `M-x customize-variable RET run-command-default-runner`. Available runners are currently:

- `run-command-runner-compile`: runs command in a `compilation-mode` buffer
- `run-command-runner-term`: runs command in a `term-mode` buffer (default)
- `run-command-runner-eat`: runs command in a `eat-mode` buffer (requires [eat.el](https://codeberg.org/akib/emacs-eat) to be installed)
- `run-command-runner-vterm`: runs command in a `Vterm-mode` buffer (requires [vterm](https://github.com/akermu/emacs-libvterm) to be installed)

A runner can also be specified per-command when writing a recipe. See [Writing Recipes](./writing-recipes#specifying-the-runner-per-command).

New runners can be added easily. See [Extending](./extending).

## Selector

By default, the selector is automatically detected. You can modify this by executing `M-x customize-variable RET run-command-selector`. Available selectors are currently:

- `run-command-selector-completing-read`: uses the built-in Emacs `completing-read`
- `run-command-selector-helm`: uses [Helm](https://emacs-helm.github.io/helm/)
- `run-command-selector-ivy`: uses [Ivy](https://oremacs.com/swiper/)

When completing via Helm or Ivy, you can edit a command before running it by typing `C-u RET` instead of `RET`.

New selectors can be added easily. See [Extending](./extending).
