[![CircleCI](https://circleci.com/gh/benedekfazekas/trin/tree/master.svg?style=svg)](https://circleci.com/gh/benedekfazekas/trin/tree/master)

# trin

- [`trin`](trin/) a static analyzer for Clojure(Script)
- [PoC](find-used-locals-trin-poc/) to explore reimplementing `refactor-nrepl.find.find-locals` using `trin`

## Status

`trin` is **alpha**.

Currently `trin` only focuses on analysing locals to achieve a PoC for `find-locals`.

That said this should unlock the following features (based on [refactor-nrepl](https://github.com/clojure-emacs/clj-refactor.el)):

- promote fn
- extract function
- inline local symbol
- find local usages/rename local symbol
- create-fn from example

without the need of a REPL.

## License

Copyright © 2019 Benedek Fazekas
