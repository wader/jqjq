# jqjq

jq implementation of [jq](https://github.com/stedolan/jq)

Why? It started when researching how to write decoders in jq for [fq](https://github.com/wader/fq) which ended up involving some AST rewriting and walking and then it escalated a bit.

It's also a great way to show that jq is a very expressive, capable and neat language!

There is a [jqplay snippet with jqjq](https://jqplay.org/s/HwbMS8mHAEwElhI) if you want to play around.


### Use via `jqjq` wrapper

```sh
$ ./jqjq -n 'def f: 1,8; [f,f] | map(.+105) | implode'
"jqjq"

$ ./jqjq '.+. | map(.+105) | implode' <<< '[1,8]'
"jqjq"

# eval example above using jqjq in jqjq. will take some time.
# eval the concatenation of jqjq.jq as a string and the example
$ ./jqjq "eval($(jq -Rs . jqjq.jq)+.)" <<< '"eval(\"def f: 1,8; [f,f] | map(.+105) | implode\")"'
"jqjq"

# jqjq has a REPL
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

# run 01mf02's adaptation of itchyny's bf.jq running fib.bf
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

# can be used as path expression (only gojq for now because of jq issue)
$ gojq -cn -L . 'include "jqjq"; {a:0, b:1} | eval(".a, .b") += 1'
{"a":1,"b":2}
```

### Run tests

`make test`

Note that the tests are meant to be used with jq 1.7.1.

## Progress

- [x] `123, .123, 1.23, 1.23e2, 1.23e+2, "abc", true, false, null` Scalar literals
  - [x] Unicode codepoint escape `"\ud83d\ude03"`
  - [x] Handle surrogate pairs `\ud800`-`\udfff`, should translate to codepoint.
  - [x] Control code and quote escape `"\"\n\r\t\f\b\\\/"`
- [x] `"abc \(123)"` String interpolation
- [x] `{key: "value"}` Object literal
  - [x] `{key}`
  - [x] `{"key"}`
  - [x] `{$key}`
  - [x] `{(f): f}`
  - [x] `{("a","b"): (1,2), c: 2}` Multiple key/value outputs
  - [ ] `{"\("abc")": 123}` Key string interpolation
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
- [x] `f?` Shorthand for `try f catch empty`
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
  - [x] `bsearch($target)`
  - [x] `capture($val)`, `capture(re; mods)`
  - [x] `debug` (passthrough)
  - [x] `debug(msgs)`
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
  - [x] `gsub($regex; f; $flags)` (passthrough)
  - [x] `halt_error`, `halt_error($exit_code)`
  - [x] `has($key)` (passthrough)
  - [x] `implode` (passthrough)
  - [x] `in(xs)`
  - [x] `index($i)`
  - [x] `indices($i)`
  - [x] `isempty`
  - [x] `join($s)`
  - [x] `last(f)`
  - [x] `last`
  - [x] `length` (passthrough)
  - [x] `limit($n; f)`
  - [x] `map(f)`
  - [x] `match($regex; $flags)` (passthrough)
  - [x] `match($val)`
  - [x] `max`, `max_by(f)`
  - [x] `min`, `min_by(f)`
  - [x] `nth($n; f); nth($n)`
  - [x] `paths`
  - [x] `range($to)`, `range($from; $to)`, `range($from; $to; $by)`
  - [x] `recurse`, `recurse(f)`
  - [x] `repeat`
  - [x] `reverse`
  - [x] `rindex($i)`
  - [x] `scalars`
  - [x] `select(f)`
  - [x] `setpath` (passthrough)
  - [x] `sort`, `sort_by(f)`
  - [x] `split($s)`
  - [x] `split($re; flags)`
  - [x] `splits($re)`, `splits($re; flags)`
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
- [x] `$ENV`
- [x] `@format "abc \(.)"` Format string
  - [ ] `@base32`
  - [ ] `@base64`
  - [ ] `@csv`
  - [ ] `@html`
  - [ ] `@sh`
  - [ ] `@tsv`
  - [x] `@uri`
  - [x] `@json`
  - [x] `@text`
- [ ] `label $out | break $out` Break out
- [ ] `include "f"`, `import "f"` Include
- [ ] CLI options
  - [x] `--help` / `-h`
  - [x] `--null-input` / `-n`
  - [ ] `--raw-input` / `-R`
  - [x] `--slurp` / `-s`
  - [x] `--compact-output` / `-c`
  - [x] `--raw-output` / `-r`
  - [x] `--raw-output0`
  - [x] `--join-output` / `-j`
  - [x] `--color-output` / `-C`
  - [x] `--monochrome-output` / `-M`
  - [ ] `-L directory`
  - [ ] `--arg name value`
  - [ ] `--rawfile name filename`
  - [x] `--run-tests`
  - [ ] `--run-tests [filename]`
  - [x] `--`
  - [ ] Combined short options
  - [ ] More...
  - Non-standard CLI options
    - [x] `--jq`
    - [x] `--lex`
    - [x] `--no-builtins`
    - [x] `--parse`
    - [x] `--repl`
- Host jq
  - [x] jq
  - [x] gojq
  - [x] jqjq
    - Used to work but runs out of memory on my laptop
  - [ ] jaq
    - Fails on missing destructing support
  - [ ] xq
    - Fails on missing `debug`, maths, jqjq `lex` returns empty (regexp issues?)
- [x] Bugs

### jq's test suite

```sh
$ ./jqjq --run-tests < ../jq/tests/jq.test | grep passed
311 of 462 tests passed
```

Note that expected test values are based on stedolan's jq. If you run with a different jq implementation like gojq some tests might fail because of different error messages, support for arbitrary precision integers etc.

### Design overview

jqjq has the common lex, parse, eval design.

#### Lex

Lexer gets a string and chews off parts from left to right producing an array of tokens `[{<name>: ...}, ...]`. Each chew is done by testing regex:s in a priority order to make sure to match longer prefixes first, ex: `+=` is matched before `+`. For a match a lambda is evaluated, usually `{<token-name>: .}`, but in some cases like for quoted strings it is a bit more complicated.

The lexer also has a stack to keep track of balance of seen `(`, `)` and `\(` to properly know how to chop of a string with interpolation into tokens. e.g. is `)` a right parenthesis or continuation of a string as in `"abc \(123) def"`?

You can use `./jqjq --lex '...'` to lex and see the tokens.

#### Parse

Parser takes an array of tokens and uses a left-to-right (LR) parser with backtracking in combination with precedence climbing for infix operators to not end up in an infinite loop (ex parser rule `E -> E + E`). Backtracking is done by outputting empty for non-match and `//` to try the next rule, ex: `a // b // ... // error` where `a` and `b` are functions that try to match a rule. When a rule has matched it returns an array with the pair `[<tokens left>, <ast>]`. `<ast>` uses the same AST design as gojq.

You can use `./jqjq --parse '...'` to lex and parse and see the AST tree.

#### Eval

Eval is done by traversing the AST tree and evaluates each AST node and also keeps track of the current path and environment.

Path is used in jq to keep track of current path to where you are in the input. The tracking can be done as long as you only operate on the input and don't create and output new values. I.e `path(.a[1]), .b` outputs `["a",1]` and `["b"]`. This is also used to implement assignment and some other operators.

Environment is an object with current functions and bindings. Functions have the key name `<name>/<arity>` and the value is a function AST. Bindings use the key name `$<name>/0` and the value is `{value: <value>}` where value is normal jq value.

When evaluating the AST eval function get the current AST node, path and environment and will output zero, one or more arrays with the pair `[<path>, <value>]`. Path can be `[null]` if the evaluation produced a "new" value etc so that path tracking is not possible.

### Problems, issues and unknowns

- Better error messages.
- The "environment" pass around is not very efficient and also it makes support recursion a bit awkward (called function is injected in the env at call time).
- "," operator in jq (and gojq) is left associate but for the way jqjq parses it creates the correct parse tree when it's right associate. Don't know why.
- Suffix with multiple `[]` outputs values in wrong order.
- String literal using interpolation that has more than one generator outputs in wrong order. Ex: `"\(1,2) \(3,4)"`.
- Non-associative operators like `==` should fail, ex: `1 == 2 == 3`.
- Objects are parsed differently compared to gojq. gojq has a list of pipe queries, jqjq will only have one that might be pipe op.
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
- [wsjq - Whitespace interpreter in jq, which can run with jqjq](https://github.com/thaliaarchi/wsjq/tree/jqjq-compat)
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
- [itchyny](https://github.com/itchyny) for jqjq fixes and [gojq](https://github.com/itchyny/gojq) from which i learned a lot and is also from where most of jqjq's AST design comes from. Sharing AST design made it easier to compare parser output (ex via [fq's `_query_fromstring`](https://github.com/wader/fq)). gojq also fixes some confusing jq bugs and has better error messages which saves a lot of time.
- Michael Färber [@01mf02](https://github.com/01m) for [jaq](https://github.com/01mf02/jaq) and where I also learned about precedence climbing.
- Thalia Archibald [@thaliaarchi](https://github.com/thaliaarchi) for correctness fixes, builtins, more CLI arguments, fancy and colorful output and more.

## License

Copyright (c) 2022 Mattias Wadman

jqjq is distributed under the terms of the MIT License.

See the [LICENSE](LICENSE) file for license details.
