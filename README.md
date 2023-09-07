# jqjq

jq implementation of [jq](https://github.com/stedolan/jq)

> **Warning** this project is mostly for learning, experimenting and fun.

Why? It started when I was researching how to write decoders directly in jq for [fq](https://github.com/wader/fq) which ended up involving some syntax tree rewriting and walking and then it grew from there.

But it's also a great way to promote and show that jq is a very expressive, capable and nice language! :)

You can try and play around with it at [jqplay.org](https://jqplay.org/s/nQQg2jV7vH5).

### Use via `jqjq` wrapper

```sh
$ ./jqjq -n 'def f: 1,8; [f,f] | map(.+105) | implode'
"jqjq"

$ ./jqjq '.+. | map(.+105) | implode' <<< '[1,8]'
"jqjq"

# jqjq using jqjq to run above example
# eval concatenation of jqjq.jq as a string and example
$ ./jqjq "eval($(jq -Rs . jqjq.jq)+.)" <<< '"eval(\"def f: 1,8; [f,f] | map(.+105) | implode\")"'
"jqjq"

$ ./jqjq --repl
> 1,2,3 | .*2
2
4
6
> "jqjq" | explode | map(.-32) | implode
"JQJQ"
> "jqjq" | [eval("explode[] | .-32")] | implode
"JQJQ"
> ^D

# 01mf02 adaptation of itchyny's bf.jq running fib.bf
$ ./jqjq -n "\"$(cat fib.bf)\" | $(cat bf.jq)"
"1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233"

$ ./jqjq -h
jqjq - jq implementation of jq
Usage: jqjq [OPTIONS] [--] [EXPR]
  --jq PATH        jq implementation to run with
  --lex            Lex EXPR
  --no-builtins    No builtins
  --null-input,-n  Null input
  --parse          Lex and parse EXPR
  --repl           REPL
  --run-tests      Run jq tests from stdin
  --slurp,-s       Slurp inputs into an array
```

### Use with `jq`

```sh
$ jq -n -L . 'include "jqjq"; eval("def f: 1,8; [f,f] | map(.+105) | implode")'
"jqjq"

$ jq -L . 'include "jqjq"; eval("(.+.) | map(.+105) | implode")' <<< '[1,8]'
"jqjq"
```

### Run tests

`make test`

Note that the tests are meant to be used with jq 1.7.

## Progress

- [x] `123, .123, 1.23, 1.23e2, 1.23e+2, "abc", true, false, null` Scalar literals
  - [x] Unicode codepoint escape `"\ud83d\ude03"`
  - [x] Handle surrogate pairs `\ud800`-`\udfff`, should translate to codepoint.
  - [x] Control code and quote escape `"\"\n\r\t\f\b\\\/"`
- [x] `{key: "value"}` Object literal
  - [x] `{key}`
  - [x] `{"key"}`
  - [x] `{$key}`
  - [x] `{(f): f}`
  - [x] `{("a","b"): (1,2), c: 2}` Multiple key/value outputs
  - [ ] `{"\(f)"}` String interpolation
  - [x] `{key: 1 | .}` Multi value queries
- [x] `[1,2,3]` Array literal, collect
- [x] `1, 2` Comma operator
- [x] `1 | 2` Pipe operator
- [x] `+`, `-`, `*`, `/`, `%` Arithmetic operators
- [x] `+123`, `-1` Unary operators
- [x] `==`, `!=`, `<`, `<=`, `>`, `>=` Comparison operators
- [x] `123 as $a | ...` Binding
  - [x] `(1,2,3) as $a | ...` Binding per output
  - [x] `{a: [123]} as {a: [$v]}` Destructuring binding
- [x] `.` Identity
- [x] `.a`, `."a"`, `.[1]`, `.[f]` Index
- [x] `.key[123]."key"[f]` Suffix expressions
  - [x] `.a.b` Multi index
  - [x] `.a.b?` Optional index
  - [x] `.a[]` Iterate index
  - [x] `.[]?` Try iterate
- [x] `.[]` Iterate
- [x] `.[start:stop]`, `.[:stop]`, `.[start:]` Array slicing
  - [ ] `.[{start: 123, stop: 123}]` Slice using object
  - [ ] Slice and path tracking `path(.[1:2]) -> [{"start":1,"end":2}]`
- [x] `try f`, Shorthand for `try f catch empty`
- [ ] `f?` Shorthand for `try f catch empty`
- [x] `and`, `or` operators
- [x] `not` operator
- [x] `if f then 2 else 3 end` Conditional
  - [x] `if f then 2 end` Optional else
  - [x] `if f then 2 elif f then 3 end` Else if clauses
  - [x] `if true,false then "a" else "b" end` Multiple condition outputs
- [x] `reduce f as $a (init; update)` Reduce outputs from `f` into one output
- [x] `foreach f as $a (init; update; extract)` Foreach outputs of `f` update state and output extracted value
  - [x] Optional extract
- [x] `f = v` Assignment
- [x] `f |= v`, `f +=` Update assignment
- [x] `+=`, `-=`, `*=`, `/=`, `%=` Arithmetic update assignment
- [x] `eval($expr)` (jqjq specific)
- [x] `path(f)` Output paths for `f`
- [x] `input`, `inputs`
- [ ] Builtins / standard library
  - [x] `add`
  - [x] `all`, `all(cond)`, `all(gen; cond)`
  - [x] `any`, `any(cond)`, `any(gen; cond)`
  - [x] `capture($val)`, `capture(re; mods)`
  - [x] `debug` (passthrough)
  - [x] `del(f)`
  - [x] `delpaths($paths)` (passthrough)
  - [x] `empty` (passthrough)
  - [x] `endswith($s)`
  - [x] `error($v)` (passthrough)
  - [x] `error` (passthrough)
  - [x] `explode` (passthrough)
  - [x] `first(f)`
  - [x] `first`
  - [x] `flatten`, `flatten($depth)`
  - [x] `from_entries`
  - [x] `fromjson`
  - [x] `getpath(path)` (passthrough)
  - [x] `group`, `group_by(f)`
  - [x] `gsub($regex; f)` (passthrough)
  - [ ] `gsub($regex; f; $flags)`
  - [x] `has($key)` (passthrough)
  - [x] `implode` (passthrough)
  - [x] `index($i)`
  - [x] `indices($i)`
  - [x] `isempty`
  - [x] `join($s)`
  - [x] `last(f)`
  - [x] `last`
  - [x] `length` (passthrough)
  - [x] `limit($n; f)`
  - [x] `map(f)`
  - [x] `match($val)`
  - [x] `match($regex; $flags)` (passthrough)
  - [x] `max`, `max_by(f)`
  - [x] `min`, `min_by(f)`
  - [x] `nth($n; f); nth($n)`
  - [x] `range($to)`, `range($from; $to)`, `range($from; $to; $by)`
  - [x] `recurse`, `recurse(f)`
  - [x] `repeat`
  - [x] `reverse`
  - [x] `rindex($i)`
  - [x] `scalars`
  - [x] `select(f)`
  - [x] `setpath` (passthrough)
  - [x] `sort`, `sort_by(f)`
  - [x] `startswith($s)`
  - [x] `test($val)`
  - [x] `test($regex; $flags)` (passthrough)
  - [x] `to_entries`
  - [x] `tojson`
  - [x] `tonumber` (passthrough)
  - [x] `tostring` (passthrough)
  - [x] `transpose`
  - [x] `type` (passthrough)
  - [x] `unique`, `unique_by(f)`
  - [x] `until(cond; next)`
  - [x] `while(cond; update)`
  - [x] `with_entries`
  - [x] Math functions, `sin/0`, ... `atan/2`, ...
  - [ ] More...
- [x] `def f: .` Function declaration
  - [x] `def f(lambda): lambda` Lambda argument
  - [x] `(def f: 123; f) | .` Closure function
  - [x] `def f: def _f: 123; _f; f` Local function
  - [x] `def f($binding): $binding` Binding arguments
  - [x] `def f: f;` Recursion
- [x] `..` Recurse input, same as `recurse`
- [x] `//` Alternative operator
- [ ] `?//` Alternative destructuring operator
- [ ] `$ENV`
- [ ] `@format "string"` Format string
- [ ] `label $out | break $out` Break out
- [ ] `include "f"`, `import "f"` Include
- [x] Run jqjq with jqjq
- [x] Bugs

### jq's test suite

```
$ ./jqjq --run-tests < ../jq/tests/jq.test | grep passed
245 of 362 tests passed
```

Note that expected test values are based on stedolan's jq. If you run with a different jq implementation like gojq some tests might fail because of different error messages, support for arbitrary precision integers etc.

### Design overview

jqjq has the common lex, parse, eval design.

#### Lex

Lexer gets a string and chews off parts from left to right producing an array of tokens `[{<name>: ...}, ...]`. Each chew is done by testing regex:s in a priority order to make sure to match longer prefixes first, ex: `+=` is matched before `+`. For a match a lambda is evaluated, usually just `.` (identity), but in some cases like for quoted strings it is a bit more complicated.

You can use `./jqjq --lex '...'` to lex and see the tokens.

#### Parse

Parser takes an array of tokens and uses a left-to-right (LR) parser with backtracking in combination with precedence climbing for infix operators to not end up in an infinite loop (ex parser rule `E -> E + E`). Backtracking is done by outputting empty for non-match and `//` to try the next rule, ex: `a // b // ... // error` where `a` and `b` are functions that try to match a rule. When a rule has matched it returns an array with the pair `[<tokens left>, <ast>]`. `<ast>` uses the same AST design as gojq.

You can use `./jqjq --parse '...'` to lex and parse and see the AST tree.

#### Eval

Eval is done by traversing the AST tree and evaluates each AST node and also keeps track of the current path and environment.

Path is used in jq to keep track of current path to where you are in the input, this only works for simple indexing (ex: `path(.a[1]), .b` outputs `["a",1]` and `["b"]`). This is also used to implement assignment and some other operators.

Environment is an object with current functions and bindings. Functions have the key name `<name>/<arity>` and the value is a function AST. Bindings use the key name `$<name>/0` and the value is `{value: <value>}` where value is normal jq value.

When evaluating the AST eval function get the current AST node, path and environment and will output zero, one or more arrays with the pair `[<path>, <value>]`. Path can be `[null]` if the evaluation produced a "new" value etc so that path tracking is not possible.

### Problems, issues and unknowns

- Better error messages.
- The "environment" pass around is not very efficient and also it makes support recursion a bit awkward (called function is injected in the env at call time).
- "," operator in jq (and gojq) is left associate but for the way jqjq parses it creates the correct parse tree when it's right associate. Don't know why.
- Suffix with multiple `[]` outputs values in wrong order.
- Non-associate operators like `==` should fail, ex: `1 == 2 == 3`.
- Object are parsed differently compared to gojq. gojq has a list of pipe queries, jqjq will only have one that might be pipe op.
- Less "passthrough" piggyback on jq features:
  - `reduce/foreach` via recursive function? similar to `if` or `{}`-literal?
  - `try/catch` via some backtrack return value? change `[path, value]` to include an error somehow?
- How to support `label/break`?
- How to support `delpaths` (usd by `del` etc). Have to keep paths the same while deleting a group of paths? use sentinel value? work with paths instead?
- Rewrite AST before eval, currently `if` and some others do rewrite (optional parts etc) while evaluating.
- Rethink invalid path handling, current `[null]` is used as sentinel value.
- `{a:123} | .a |= empty` should remove the key.

### Useful references

- [jq - Command-line JSON processor](https://github.com/stedolan/jq)
- [jq's builtin.jq](https://github.com/stedolan/jq/blob/master/src/builtin.jq)
- [jq Language Description](https://github.com/stedolan/jq/wiki/jq-Language-Description)
- [jq simplified grammar](https://github.com/fadado/JBOL/blob/master/doc/JQ-language-grammar.md)
- [gojq - Pure Go implementation of jq](https://github.com/itchyny/gojq)
- [jaq - A jq clone focused on correctness, speed, and simplicity](https://github.com/01mf02/jaq)
- [xq - Pure rust implementation of jq](https://github.com/MiSawa/xq)
- [jq wiki: jq as a PEG engine by pkoppstein](https://github.com/stedolan/jq/wiki/Parsing-Expression-Grammars)
- [Precedence climbing](https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#climbing)

### Tools and tricks

- `jq -n --debug-dump-disasm '...'` show jq byte code
- `jq -n --debug-trace=all '...'` show jq byte code run trace
- `jq -n '{a: "hello"} | debug' 2> >(jq -R 'gsub("\u001b\\[.*?m";"") | fromjson' >&2)` pretty print debug messages
- `GOJQ_DEBUG=1 go run -tags gojq_debug cmd/gojq/main.go -n '...'` run gojq in debug mode
- `fq -n '".a.b" | _query_fromstring'` gojq parse tree for string
- `fq -n '{...} | _query_tostring'` jq expression string for gojq parse tree
- For a convenient jq development experience:
  - [jq-dash-docset](https://github.com/wader/jq-dash-docset)
  - [vscode-jq](https://github.com/wader/vscode-jq)
  - [jq-lsp](https://github.com/wader/jq-lsp)

## Thanks to

- [stedolan](https://github.com/stedolan) for jq and got me interested in generator/backtracking based languages.
- [pkoppstein](https://github.com/pkoppstein) for writing about [jq and PEG parsing](https://github.com/stedolan/jq/wiki/Parsing-Expression-Grammars).
- [itchyny](https://github.com/itchyny) for jqjq fixes and [gojq](https://github.com/itchyny/gojq) from which is learned a lot and is also from where most of jqjq's AST design comes from. Sharing AST design made it easier to compare parser output (ex via [fq's `_query_fromstring`](https://github.com/wader/fq)). gojq also fixes some confusing jq bugs and has better error messages which saves a lot of time.
- Michael Färber [@01mf02](https://github.com/01m) for [jaq](https://github.com/01mf02/jaq) and where I also learned about precedence climbing.

## License

Copyright (c) 2022 Mattias Wadman

jqjq is distributed under the terms of the MIT License.

See the [LICENSE](LICENSE) file for license details.
