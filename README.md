# flycheck-vale

This package provides [flycheck](http://www.flycheck.org/) integration
for [vale](https://github.com/ValeLint/vale). Flycheck is an Emacs system for
on-the-fly syntax checking. Vale is a natural language linter. So with
`flycheck-vale` you get on-the-fly natural language linting.

Right now `flycheck-vale` is very new and unpolished. Ideas, PRs, etc. are welcome!

## Quickstart

To use `flycheck-vale` just `require` it and run `flycheck-vale-setup`:

```emacs-lisp
(require 'flycheck-vale)
(flycheck-vale-setup)
```
