# find-used-locals `trin` PoC

PoC to explore reimplementing https://github.com/clojure-emacs/refactor-nrepl/blob/bc22338e9d92c33147a138fa8f83fb22d26d51f0/src/refactor_nrepl/find/find_locals.clj with `trin`.

To achieve this `find-locals` was reimplemented in [`thomasa.find-used-locals-trin-poc/find-used-locals`](src/thomasa/find_used_locals_trin_poc.clj) using `trin` to retrieve an AST for the selected SEXPR. The machinery to find the selected SEXPR was also brought over from `refactor-nrepl` and adapted to work with `trin` AST.

The tests were also brought over from `refactor-nrepl` to test this particular function to validate the PoC. `trin` does a bit more than necessary to make these tests pass, those extra features are unit tested in the `trin` project itself.

Find locals was picked because while discussing our problems with `refactor-nrepl` (the lack of cljs/cljc support) due to the analyzer we use we figured that this feature was a good candidate to experiment with new analyzers.

See [refactor-nrepl#195](https://github.com/clojure-emacs/refactor-nrepl/issues/195) for more context. And [this commit](https://github.com/clojure-emacs/refactor-nrepl/commit/8b651a0e23b62a390f343f891e8ef6bb6e8cd32f) for a PoC using `cljs.analyzer` for cljs.

## Running the tests

```
clojure -A:test -m kaocha.runner
```
