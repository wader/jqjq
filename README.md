# jqjq

jq implementation of [jq](https://github.com/stedolan/jq)

> **Warning** this project is mostly for learning, experimenting and fun.

Why? It started when I was researching how to write decoders directly in jq for [fq](https://github.com/wader/fq) which ended up involving some syntax tree rewriting and walking and then it grow from there.

But it's also a great way to promote and show that jq is a very expressive, capable and nice language! :)

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

## Progress

- [x] `123, .123, 1.23, 1.23e2, 1.23e+2, "abc", true, false, null` Scalar literals
- [x] `{key: "value"}` Object literal
  - [x] `{key}`
  - [x] `{"key"}`
  - [x] `{$key}`
  - [x] `{(...): ...}`
  - [x] `{("a","b"): (1,2), c: 2}` Multiple key/value outputs
  - [ ] `{"\()"}`
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
- [ ] `.key[123]."key"[f]` Index
  - [x] `.a`, `.["a"]` Simple index
  - [x] `."key"`
  - [x] `.a.b` Multi index
  - [x] `.a?` Optional index
  - [x] `.a[]` Iterate index
- [x] `.[]` Iterate
- [ ] `.[]?` Try iterate
- [x] `.[start:stop]`, `.[:stop]`, `.[start:]` Array slicing
  - [ ] `.[{start: 123, stop: 123}]` Slice using objec
  - [ ] Slice and path tracking `path(.[1:2]) -> [{"start":1,"end":2}]`
- [x] `and`, `or` operators
- [x]  `not` operator
- [x] `if f then 2 else 3 end` Conditional
  - [x] `if f then 2 end` Optional else
  - [x] `if f then 2 elif f then 3 end` Else if clauses
  - [x] `if true,false then "a" else "b" end` Multiple condition outputs
- [x] `reduce f as $a (init; update)` Reduce output
- [x] `foreach f as $a (init; update; extract)` Foreach output, update state and output extracted value
  - [x] Optional extract
- [x] `f = v` Assignment
- [x] `f |= v`, `f +=` Update assignment
- [x] `+=`, `-=`, `*=`, `/=`, `%=` Arithmetic update assignment
- [x] `eval($expr)`
- [x] `input`, `inputs`
- [ ] Builtins / standard library
  - [ ] `del(f)`
  - [x] `add`
  - [x] `all`, `all(cond)`, `all(gen; cond)`
  - [x] `any`, `any(cond)`, `any(gen; cond)`
  - [x] `debug` (passthrough)
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
  - [x] `fromjson` (passthrough)
  - [x] `getpath(path)` (passthrough)
  - [x] `group`, `group_by(f)`
  - [x] `has($key)` (passthrough)
  - [x] `implode` (passthrough)
  - [x] `isempty`
  - [x] `join($s)`
  - [x] `last(f)`
  - [x] `last`
  - [x] `length` (passthrough)
  - [x] `limit($n; f)`
  - [x] `map(f)`
  - [x] `max`, `max_by(f)`
  - [x] `min`, `min_by(f)`
  - [x] `nth($n; f); nth($n)`
  - [x] `range($to)`, `range($from; $to)`, `range($from; $to; $by)`
  - [x] `recurse`, `recurse(f)`
  - [x] `repeat`
  - [x] `reverse`
  - [x] `scalars`
  - [x] `select(f)`
  - [x] `setpath` (passthrough)
  - [x] `sort`, `sort_by(f)`
  - [x] `startswith($s)`
  - [x] `to_entries`
  - [x] `tojson` (passthrough)
  - [x] `tonumber` (passthrough)
  - [x] `tostring` (passthrough)
  - [x] `match($regex; $flags)` (passthrough)
  - [ ] `match($val)`
  - [x] `gsub($regex; f)` (passthrough)
  - [ ] `gsub($regex; f; $flags)`
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
- [x] `path(f)` Output paths for `f` for input
- [x] `try f`, `try f catch .` Catch error
- [ ] `f?` Empty shorthand catch
- [x] `..` Recurse input
- [x] `//` Alternative operator
- [ ] `?//` Alternative destructuring operator
- [ ] `$ENV`
- [ ] `"\(f)"` String interpolation
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

### Design problems, issues and unknowns

- Better parser errors.
- The "environment" pass around is not very efficient and also it make support recursion a bit awkward (called function is injected in the env at call time).
- "," operator in jq (and gojq) is left associate but for the way jqjq parses it creates the correct parse tree when it's right associate. Don't know why.
- Suffix with multiple `[]` outputs values in wrong order.
- Non-associate operators like `==` should fail, ex: `1 == 2 == 3`.
- Object are parsed differently compared to gojq. gojq has a list of pipe queries, jqjq will only have one that might be pipe op.
- Less "passthrough" piggyback on jq features:
  - `reduce/foreach` via recursive function? similar to `if` or `{}`-literal?
  - `try/catch` via some backtrack return value? change `[path, value]` to include an error somehow?
- How to support `label/break`?
- How to support `delpaths` (usd by `del` etc). Have to keep paths same while deleting a group of paths? use sentinel value? work with paths instead?
- Rewrite AST before eval, currently `if` and some other do rewrite (optional parts etc) while evaluating.
- Rethink invalid path handling, current `[null]` is used as sentinel value.
- `{a:123} | .a |= empty` should remove key.

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

- [stedolan](https://github.com/stedolan) for jq and got me interesting in generator/backtracking based languages.
- [pkoppstein](https://github.com/pkoppstein) for writing about [jq and PEG parsing](https://github.com/stedolan/jq/wiki/Parsing-Expression-Grammars).
- [itchyny](https://github.com/itchyny) for jqjq fixes and [gojq](https://github.com/itchyny/gojq) from which is learned a lot and is also from where most of jqjq's AST design comes from. Sharing AST design made it easier to compare parser output (ex via [fq's `_query_fromstring`](https://github.com/wader/fq)). gojq also fixes some confusing jq bugs and has better error messages which saves a lot of time.
- Michael Färber [@01mf02](https://github.com/01m) for [jaq](https://github.com/01mf02/jaq) and where I also learned about precedence climbing.

## License

Copyright (c) 2022 Mattias Wadman

jqjq is distributed under the terms of the MIT License.

See the [LICENSE](LICENSE) file for license details.
