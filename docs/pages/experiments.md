# Experiments

Some features are work-in-progress or it's undecided whether they should form part of the official release, so they are switched off by default.

If you'd like to have a peek and help testing them, you can enable them individually with:

```lisp
(setq run-command-experiments '(experiment-1 experiment-2))
```

## Experimental feature status

| name                                            | status  | description                                                                                                                                                                                                            |
| ----------------------------------------------- | ------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `run-command-experiment-function-command-lines` | retired | ~Allow specifying command line via function~ Integrated.                                                                                                                                                               |
| `run-command-experiment-vterm-run-method`       | retired | ~Run commands in a [vterm](https://github.com/akermu/emacs-libvterm) buffer~ Integrated.                                                                                                                               |
| `static-recipes`                                | retired | ~Allow recipes to be defined by variables in addition to functions~. See this [example](https://github.com/bard/emacs-run-command/blob/master/examples/run-command-recipe-dir-locals.el) for equivalent functionality. |
| `run-command-experiment-lisp-commands`          | retired | ~Run inline Lisp functions in addition to external commands.~                                                                                                                                                          |
