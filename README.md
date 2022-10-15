# jqjq

jq implementation of [jq](https://github.com/stedolan/jq)

> **Warning** this project is mostly for learning, experimenting and fun.

### Use via `jqjq` wrapper

```
$ ./jqjq -n 'def f: 1,8; [f,f] | map(.+105) | implode'
"jqjq"

$ ./jqjq '.+. | map(.+105) | implode' <<< '[1,8]'
"jqjq"

$ ./jqjq --repl
> 1,2,3 | .*2
2
4
6
> "jqjq" | explode | map(.-32) | implode
"JQJQ"
> ^D

$ ./jqjq -h
jqjq - jq implementation of jq
Usage: jqjq [OPTIONS] [--] [EXPR]
  --jq PATH        jq implementation to run with
  --lex            Lex EXPR
  --no-builtins    Don't include builtins
  --null-input,-n  Null input
  --parse          Lex then parse EXPR
  --repl           REPL
  --run-tests      Run jq tests from stdin
```

### Use with `jq`

```
$ jq -n -L . 'include "jqjq"; eval("def f: 1,8; [f,f] | map(.+105) | implode")'
"jqjq"

$ jq -L . 'include "jqjq"; eval("(.+.) | map(.+105) | implode")' <<< '[1,8]'
"jqjq"
```

### Run tests

`make test`

## Progress

- [x] `123, .123, 1.23, 1.23e2, "abc", true, false, null` Scalar literals
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
- [x] `1 | .` Pipe operator
- [x] `+`, `-`, `*`, `/`, `%` Arithmetic operators
- [x] `+123`, `-1` Unary operators
- [x] `==`, `!=`, `<`, `<=`, `>`, `>=` Comparison operators
- [x] `123 as $a | ...` Binding
  - [x] `(1,2,3) as $a | ...` Binding per output
  - [ ] `{a: [123]} as {a: [$v]}` Pattern binding
- [x] `.` Identity
- [ ] `.key[123]."key"[f]` Index
  - [x] `.a`, `.["a"]` Simple index
  - [x] `."key"`
  - [x] `.a.b` Multi index
  - [ ] `.a?` Optional index
  - [x] `.a[]` Iterate index
- [x] `.[]` Iterate
- [ ] `.[]?` Try iterate
- [x] `.[start:stop]`, `.[:stop]`, `.[start:]` Array slicing
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
- [ ] Builtins / standard library
  - [x] `add`
  - [x] `debug` (passthrough)
  - [x] `empty` (passthrough)
  - [x] `error($v)` (passthrough)
  - [x] `explode` (passthrough)
  - [x] `fromjson` (passthrough)
  - [x] `getpath(path)` (passthrough)
  - [x] `implode` (passthrough)
  - [x] `join($s)`
  - [x] `length` (passthrough)
  - [x] `map(f)`
  - [x] `max`, `max_by(f)`
  - [x] `min`, `min_by(f)`
  - [x] `range($to)`, `range($from; $to)`, `range($from; $to; $by)`
  - [x] `recurse`, `recurse(f)`
  - [x] `repeat`
  - [x] `reverse`
  - [x] `scalars`
  - [x] `select(f)`
  - [x] `setpath` (passthrough)
  - [x] `sort`, `sort_by(f)`
  - [x] `tojson` (passthrough)
  - [x] `tostring` (passthrough)
  - [x] `type` (passthrough)
  - [x] `until(cond; next)`
  - [x] `while(cond; update)`
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
- [ ] `//`, `?//` Alternative operator
- [ ] `$ENV`
- [ ] `"\(f)"` String interpolation
- [ ] `@format "string"` Format string
- [ ] `label $out | break $out` Break out
- [ ] `include "f"`, `import "f"` Include
- [ ] Run jqjq with jqjq
- [x] Bugs

### jq's jq.test test suite

```
$ ./jqjq --run-tests < ../jq/tests/jq.test | grep passed
152 of 348 tests passed
```

Not that expected test values are based on stedolan jq. If they are run with a different jq implementation like gojq some might fail because of different error messages, support for arbitrary precision integers etc.

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
- [jaq - A jq clone focussed on correctness, speed, and simplicity](https://github.com/01mf02/jaq)
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

## Thanks to

- [stedolan](https://github.com/stedolan) for jq and got me interesting in generator/backtracking based languages.
- [pkoppstein](https://github.com/pkoppstein) for writing about [jq and PEG parsing](https://github.com/stedolan/jq/wiki/Parsing-Expression-Grammars).
- [itchyny](https://github.com/itchyny) for [gojq](https://github.com/itchyny/gojq) where most of jqjq's AST design comes from which made it easier to compare parser output (ex via [fq's `_query_from
- string`](https://github.com/wader/fq)). It also fixes some confusing jq bugs and has better error messages which saves a lot of time.
- Michael Färber [@01mf02](https://github.com/01m) for [jaq](https://github.com/01mf02/jaq) and where I also learned about precedence climbing.

## License

Copyright (c) 2022 Mattias Wadman

jqjq is distributed under the terms of the MIT License.

See the [LICENSE](LICENSE) file for license details.
