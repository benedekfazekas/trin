[![codecov](https://codecov.io/gh/benedekfazekas/trin/branch/master/graph/badge.svg)](https://codecov.io/gh/benedekfazekas/trin)

# trin

A tiny analyzer, maybe.

`trin` is a static, non-evaling [rewrite-clj](https://github.com/xsc/rewrite-clj) based analyzer. It decorates [rewrite-clj](https://github.com/xsc/rewrite-clj) nodes with the information gathered in the analysis turning them into AST nodes. The AST information added complies with [tools.analyzer(.jvm) AST](http://clojure.github.io/tools.analyzer.jvm/spec/quickref.html) where it makes sense, altough the above spec is extended at places. The usual [rewrite-clj](https://github.com/xsc/rewrite-clj) tools can be used to traverse the tree after analysis.

Macros are not expanded as of now. `trin` very likely will focus on analysing the non macroexpanded source in the future too. However, it may analyse macroexpanded source in the future **additionally** and/or add an extension point to handle custom macros.

Compared to other [rewrite-clj](https://github.com/xsc/rewrite-clj) based analysis tools like [clj-kondo](https://github.com/borkdude/clj-kondo) and [clojure-lsp](https://github.com/snoe/clojure-lsp) `trin` aims to create a generic AST for other tools to work with.

## Usage

```clojure
(require '[rewrite-clj.zip :as zip])
(require '[thomasa.trin :as trin])

(def source-as-string "(defn foo [f] f)")

(trin/analyze-loc {} (zip/of-string source-as-string)
```

Find the AST information in the returned `rewrite-clj` nodes under `[0 :ast-info]`. See a small utility function to print AST info for nodes in the [user namespace](dev/user.clj).

### Running the tests

```
clojure -A:test -m kaocha.runner
```

## Credits

Thanks for all authors/maintainers/contributors of projects mentioned above.
