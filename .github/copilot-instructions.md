# Copilot Instructions

Common Lisp library (ASDF system `generic-arithmetic`) that re-implements the `cl` numeric functions as CLOS generic functions so users can extend the numeric tower with custom types.

## Build & test

No build script; use ASDF/Quicklisp (repo expected under `~/quicklisp/local-projects/`). Dependencies: `fiveam`, `fold`, `function`.

```lisp
(ql:quickload :generic-arithmetic)                                   ; loads library AND tests.lisp
(fiveam:run! 'generic-arithmetic-tests::generic-arithmetic-suite)    ; full suite
(fiveam:run! 'generic-arithmetic-tests::addition)                    ; single test (use the test name)
```

There is no linter. `tests.lisp` is a component of the main system, so a test compile error breaks `quickload`.

## Architecture

- `package.lisp` — defines `GENERIC-ARITHMETIC`, which **shadows** the `cl` numeric symbols (`+ - * / < = abs sin ...`) and exports them plus the generic hooks (`ADD2`, `SUBTRACT2`, `MULTIPLY2`, `DIVIDE2`, `<2`, `=2`, `NEGATE`, `RECIPROCAL`, `SQUARE`, `CUBE`, ...). Uses the `FOLD` and `FUNCTION` packages.
- `generic-arithmetic.lisp` — each operation is a pair: a binary/unary `defgeneric` (the extension point, e.g. `add2`, `<2`) with a default method on `number` that calls the `cl:` function, plus an n-ary `defun` wrapper (e.g. `+`, `<`) that folds over the generic with `fold-left`. Users support new types by defining methods on the generics, not the wrappers.
- `tests.lisp` — FiveAM suite in its own package `GENERIC-ARITHMETIC-TESTS` (uses `shadowing-import-from` for every exported symbol).

## Conventions

- **Adding/exporting a symbol requires updating three places**: `:shadow` (if it shadows a `cl` symbol) and `:export` in `package.lisp`, and the `:shadowing-import-from` list in `tests.lisp`.
- In `generic-arithmetic.lisp`, call the real CL function with an explicit `cl:` prefix (e.g. `cl:+`); unprefixed names refer to the generic versions.
- Docstrings are copied from CL at compile time: `#.(documentation 'cl:+ 'cl:function)`.
- Binary generics use a `2` suffix (`add2`, `<2`); the n-ary wrapper keeps the plain CL name.
- Add a FiveAM `(test ...)` in `tests.lisp` for each new operation.
