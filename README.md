# flycheck-vale

[![MELPA](http://melpa.org/packages/flycheck-vale.svg)](http://melpa.org/#/flycheck-vale)

This package provides [flycheck](http://www.flycheck.org/) integration
for [vale](https://github.com/ValeLint/vale). Flycheck is an Emacs system for
on-the-fly syntax checking. Vale is a natural language linter. So with
`flycheck-vale` you get on-the-fly natural language linting.

Right now `flycheck-vale` is very new and unpolished. Ideas, PRs, etc. are welcome!

## Quickstart

Install `flycheck-vale` from MELPA using `package-install` or something equivalent.

To use `flycheck-vale` just `require` it and run `flycheck-vale-setup`:

```emacs-lisp
(require 'flycheck-vale)
(flycheck-vale-setup)
```

## Dis/enabling flycheck vale for specific buffers

The buffer-local variable `flycheck-vale-enabled` allows you to enabled or
disable vale linting for specific buffers. If this variable is `t` then vale
linting will be performed (assuming you've got `flycheck-mode` enabled, etc.)
Likewise, if it is `nil` then vale linting will never be performed.

You can use `flycheck-vale-toggle-enabled` to toggle this variable between `t`
and `nil`. (And of course you can set it other ways if you want.) By default the
variable is `t`.
