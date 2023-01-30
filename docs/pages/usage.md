# Usage

If you're launching compilers, servers, linters, etc you'll probably want to scroll the command's buffer to read through the output. Since commands run in term buffers, which "swallow" usual Emacs keybindings, switching to the buffer and pressing the `C-v`, `M-v`, `PageUp`, and `PageDown` won't work.

Luckily, there's an even better way.

You can navigate the command buffer without even leaving the buffer where you're doing your main work by using the following keybindings:

- `M-<home>` (aka `Alt+Home`) to scroll the other window to the top

- `M-<next>` or `C-M-v` (aka `Alt+PageDown` and `Ctrl+Alt+v`) to scroll the other window down

- `M-<prior>` or `C-M-S-v` (aka `Alt+PageUp` and `Ctrl+Alt+Shift+v`) to scroll the other window up

- `M-<end>` (aka `Alt+End`) to scroll the other window to the bottom.
