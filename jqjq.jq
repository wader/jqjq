#!/usr/bin/env -S bash -euo pipefail
# jqjq - jq implementation of jq
# Copyright (c) 2022 Mattias Wadman
# MIT License
# \
eval "$( \
  "${JQ:=jq}" \
    -nr \
    -L "$(dirname "$(realpath "${BASH_SOURCE[0]}")")" \
    'include "jqjq"; $ARGS.positional | construct_jqjq_command' \
    --args -- "$@" \
)"; exit

# TODO:
# ".end" lex, require whitespace/end around ident?
# how test associativity 1|2|3?
# add some term builder helper, _term("TermTypeArray"; {query: ...}) etc?
# "a |" parses as "a | .", should be error, make empty eval special case?
#
# Notes:
# - AST is more or less identical to the one used by gojq to make it easier to test parser
# - jq bindings $<name>_ is used if <name> is a keyword as jq (not gojq) does not allow it
# - "string_middle" token is used to distingush between a string that could be an index and
#   a string that is parts of string interpolation.
#

def _internal_error($v): {_internal_error: $v} | error;
def _is_internal_error: type == "object" and has("_internal_error");
def _unwrap_internal_error: ._internal_error;

# reimplementation of getpath/1 for jaq
def _getpath($path):
  if . == null or ($path | length) == 0 then .
  else .[$path[0]] | _getpath($path[1:])
  end;

# reimplementation of delpaths/1 for jaq
def _delpaths($paths):
  def _delpath($p):
    if $p == [] then empty
    elif has($p[0]) then .[$p[0]] |= _delpath($p[1:])
    else .
    end;
  reduce ($paths | unique | reverse)[] as $p
    (.; _delpath($p));

# TODO: keep track of position?
# TODO: error on unbalanced string stack?
# string_stack is used to keep track of matching ( ) and \( <-> )" or ) (
def lex:
  def _unescape:
    def _fromhex:
      def _fromradix($base; tonum):
        reduce explode[] as $c (
          0;
          . * $base + ($c | tonum)
        );
      _fromradix(
        16;
        if . >= 48 and . <= 57 then .-48 # 0-9
        elif . >= 97 and . <= 102 then .-97+10 # a-f
        else .-65+10 # A-F
        end
      );

    gsub(
      ( "(?<surrogate>(\\\\u[dD][89a-fA-F][0-9a-fA-F]{2}){2})|"
      + "(?<codepoint>\\\\u[0-9a-fA-F]{4})|"
      + "(?<escape>\\\\.)"
      );
      if .surrogate then
        # surrogate pair \uD83D\uDCA9 -> 💩
        ( .surrogate
        | ([.[2:6], .[8:] | _fromhex]) as [$hi,$lo]
        # translate surrogate hi/lo pair values into codepoint
        # (hi-0xd800<<10) + (lo-0xdc00) + 0x10000
        | [($hi-55296)*1024 + ($lo-56320) + 65536]
        | implode
        )
      elif .codepoint then
        # codepoint \u006a -> j
        ( .codepoint[2:]
        | [_fromhex]
        | implode
        )
      elif .escape then
        # escape \n -> \n
        ( .escape[1:] as $escape
        | { "n": "\n"
          , "r": "\r"
          , "t": "\t"
          , "f": "\f"
          , "b": "\b"
          , "\"": "\""
          , "/": "/"
          , "\\": "\\"
          }[$escape]
        | if not then error("unknown escape: \\\($escape)") else . end
        )
      else error("unreachable")
      end
    );

  def _token:
    def _re($re; f):
      ( . as {$remain, $string_stack}
      | $remain
      | match($re; "").string
      | f as $token
      | { result: ($token | del(.string_stack))
        , remain: $remain[length:]
        , string_stack:
            ( if $token.string_stack == null then $string_stack
              else $token.string_stack
              end
            )
        }
      );
    if .remain == "" then empty
    else
      ( . as {$string_stack}
      | _re("^\\s+"; {whitespace: .})
      // _re("^#[^\n]*"; {comment: .})
      // _re("^\\.[_a-zA-Z][_a-zA-Z0-9]*"; {index: .[1:]})
      // _re("^@[_a-zA-Z][_a-zA-Z0-9]*"; {at_ident: .})
      // _re("^[_a-zA-Z][_a-zA-Z0-9]*"; {ident: .})
      // _re("^\\$[_a-zA-Z][_a-zA-Z0-9]*"; {binding: .})
      # 1.23, .123, 123e2, 1.23e2, 123E2, 1.23e+2, 1.23E-2 or 123
      // _re("^(?:[0-9]+\\.[0-9]+|[0-9]+)(?:[eE][-\\+]?[0-9]+)?"; {number: .})
      // _re("^\"(?:[^\"\\\\]|\\\\.)*?\\\\\\(";
          ( .[1:-2]
          | _unescape
          | {string_start: ., string_stack: ($string_stack+["\\("])}
          )
        )
      //
        # conditionally look for end or middle of interpolated string
        ( select($string_stack[-1] == "\\(")
        | _re("^\\)(?:[^\"\\\\]|\\\\.)*?\\\\\\(";
            ( .[1:-2]
            | _unescape
            | {string_middle: .}
            )
          )
        // _re("^\\)(?:[^\"\\\\]|\\\\.)*?\"";
            ( .[1:-1]
            | _unescape
            | {string_end: .
            , string_stack: ($string_stack[0:-1])})
          )
        )
      # match " <any non-"-or-\> or <\ + any> "
      // _re("^\"(?:[^\"\\\\]|\\\\.)*?\""; .[1:-1] | _unescape | {string: .})
      // _re("^==";     {equal_equal: .})
      // _re("^\\|=";   {pipe_equal: .})
      // _re("^=";      {equal: .})
      // _re("^!=";     {not_equal: .})
      // _re("^<=";     {less_equal: .})
      // _re("^>=";     {greater_equal: .})
      // _re("^\\+=";   {equal_plus: .})
      // _re("^-=";     {equal_dash: .})
      // _re("^\\*=";   {equal_star: .})
      // _re("^/=";     {equal_slash: .})
      // _re("^%=";     {equal_percent: .})
      // _re("^<";      {less: .})
      // _re("^>";      {greater: .})
      // _re("^:";      {colon: .})
      // _re("^;";      {semicolon: .})
      // _re("^\\|";    {pipe: .})
      // _re("^,";      {comma: .})
      // _re("^\\+";    {plus: .})
      // _re("^-";      {dash: .})
      // _re("^\\*";    {star: .})
      // _re("^//";     {slash_slash: .})
      // _re("^/";      {slash: .})
      // _re("^%";      {percent: .})
      // _re("^\\(";    {lparen: ., string_stack: ($string_stack + ["("])})
      // _re("^\\)";    {rparen: ., string_stack: ($string_stack[0:-1])})
      // _re("^\\[";    {lsquare: .})
      // _re("^\\]";    {rsquare: .})
      // _re("^\\{";    {lcurly: .})
      // _re("^\\}";    {rcurly: .})
      // _re("^\\.\\."; {dotdot: .})
      // _re("^\\.";    {dot: .})
      // _re("^\\?";    {qmark: .})
      // error("unknown token: '\(.remain[0:100])'")
      )
    end;
  def _lex:
    ( { remain: .
      , result: {whitespace: ""}
      , string_stack: []
      }
    | recurse(_token)
    | .result
    | select((.whitespace // .comment) | not)
    );
  [_lex];

def parse:
  def _consume(f):
    ( select(length > 0 and (.[0] | f))
    | [.[1:], .[0]]
    );
  def _optional(f):
    ( f
    // [., null]
    );
  def _repeat(f):
    def _f:
      ( f as [$rest, $v]
      | [$rest, $v]
      , ( $rest
        | _f
        )
      );
    ( . as $c
    | [_f]
    | if length > 0 then [.[-1][0], map(.[1])]
      else [$c, []]
      end
    );
  def _keyword($name): _consume(.ident == $name)[0];

  def _p($type):
    # based on:
    # https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#climbing
    # filter is used to disable operators, ex in keyval query
    def _op_prec_climb($p; filter):
      def _ops:
        if filter then            null
        elif .pipe then           {prec: 0, name: "|",   assoc: "right"}
        # TODO: understand why jq has left associativity for "," but right seems to give correct parse tree
        elif .comma then          {prec: 1, name: ",",   assoc: "right"}
        elif .slash_slash then    {prec: 2, name: "//",  assoc: "right"}
        elif .equal then          {prec: 3, name: "=",   assoc: "none"}
        elif .pipe_equal then     {prec: 3, name: "|=",  assoc: "none"}
        elif .equal_plus then     {prec: 3, name: "+=",  assoc: "none"}
        elif .equal_dash then     {prec: 3, name: "-=",  assoc: "none"}
        elif .equal_star then     {prec: 3, name: "*=",  assoc: "none"}
        elif .equal_slash then    {prec: 3, name: "/=",  assoc: "none"}
        elif .equal_percent then  {prec: 3, name: "%=",  assoc: "none"}
        elif .ident == "or" then  {prec: 4, name: "or",  assoc: "left"}
        elif .ident == "and" then {prec: 5, name: "and", assoc: "left"}
        elif .equal_equal then    {prec: 6, name: "==",  assoc: "none"}
        elif .not_equal then      {prec: 6, name: "!=",  assoc: "none"}
        elif .less then           {prec: 6, name: "<",   assoc: "none"}
        elif .less_equal then     {prec: 6, name: "<=",  assoc: "none"}
        elif .greater then        {prec: 6, name: ">",   assoc: "none"}
        elif .greater_equal then  {prec: 6, name: ">=",  assoc: "none"}
        elif .plus then           {prec: 7, name: "+",   assoc: "left"}
        elif .dash then           {prec: 7, name: "-",   assoc: "left"}
        elif .star then           {prec: 8, name: "*",   assoc: "left"}
        elif .slash then          {prec: 8, name: "/",   assoc: "left"}
        elif .percent then        {prec: 8, name: "%",   assoc: "left"}
        else null
        end;

      ( _p("query1") as [$rest, $t]
      | $rest
      | def _f($t):
          ( .[0] as $next # peek next
          | ($next | if . != null then _ops end) as $next_op
          | if $next_op and $next_op.prec >= $p then
              ( .[1:] # consume
              | ( if $next_op.assoc == "right" then
                    _op_prec_climb($next_op.prec; filter)
                  elif $next_op.assoc == "left" then
                    _op_prec_climb($next_op.prec+1; filter)
                  else
                    # TODO: none associativity, 1 == 2 == 3 etc, should be error
                    _op_prec_climb($next_op.prec+1; filter)
                  end
                ) as [$rest, $t1]
              | $rest
              # TODO: better way?
              # if functions was defined for the left side they should be
              # move to the op itself to also be available on the right side
              # ex: def f: 123; 1 + f
              | $t as {$func_defs}
              | _f(
                  ( { op: $next_op.name
                    , left: ($t | del(.func_defs))
                    , right: $t1
                    }
                  | if $func_defs then
                      .func_defs = $func_defs
                    else .
                    end
                  )
                )
              )
            else
              [., $t]
            end
          );
        _f($t)
      );

    def _scalar($type; c; f):
      ( _consume(c) as [$rest, $v]
      | [ $rest
        , { term:
              ( $v
              | f
              | .type = $type
              )
          }
        ]
      );

    # {<keyval>...} where keyval is:
    # name
    # "name"
    # $name
    # name: <term>
    # "name": <term>
    # <subquery>: <term>
    def _object:
      ( _consume(.lcurly)[0]
      | _repeat(
          # TODO:
          # string interpolated key
          #   {"\(...)"} -> {"\(...)"": .["\(...)"]}
          #   {"\(...)": ...} -> {"\(...)"": ...}
          # multi query val:
          #    term | ...
          ( ( def _colon_val:
                ( _consume(.colon)[0]
                # keyval_query only allows | operator (, is separator)
                | _p("keyval_query") as [$rest, $val]
                | $rest
                | [ .
                  , { queries: [$val]
                    }
                  ]
                );
              (
                # {a} -> {a: .a}
                # {a: ...} -> {a: ...}
                ( _consume(.ident) as [$rest, {$ident}]
                | $rest
                | _optional(_colon_val) as [$rest, $val]
                | $rest
                | [ .
                  , { key: $ident
                    , val: $val
                    }
                  ]
                )
              //
                # {"a"} -> {a: .a}
                # {"a": ...} -> {a: ...}
                ( _p("string") as [$rest, $string]
                | $rest
                | _optional(_colon_val) as [$rest, $val]
                | $rest
                | [ .
                  , { key_string:
                        {str: $string.term.str}
                    , val: $val
                    }
                  ]
                )
              //
                # {$a} -> {a: $a}
                ( _consume(.binding) as [$rest, {$binding}]
                | [ $rest
                  , {key: $binding}
                  ]
                )
              //
                # {(...): ...} -> {...: ...}
                ( _p("subquery") as [$rest, $query]
                | $rest
                | _colon_val as [$rest, $val]
                | $rest
                | [ .
                  , { key_query: $query
                    , val: $val
                    }
                  ]
                )
              )
            ) as [$rest, $key_vals]
          | $rest
          | ( if .[0].rcurly then
                # keep it to make repeat finish and consumed it below
                [$rest, null]
              else
                # or there must be a comma
                ( _consume(.comma) as [$rest, $_]
                | [$rest, null]
                )
              end
            ) as [$rest, $_]
          | [$rest, $key_vals]
          )
        ) as [$rest, $key_vals]
      | $rest
      | _consume(.rcurly) as [$rest, $_]
      | [ $rest
        , { term:
              { type: "TermTypeObject"
              , object:
                  {key_vals: $key_vals}
              }
          }
        ]
      );

    # destructing pattern:
    # pattern is:
    # $name
    # name: pattern
    # "name": pattern
    # "\(...)": pattern
    # (query): pattern
    # [pattern, ...]
    def _pattern:
      ( ( _consume(.lcurly)[0]
        | _repeat(
            ( ( def _colon_pattern:
                  ( _consume(.colon)[0]
                  | _p("pattern") as [$rest, $pattern]
                  | $rest
                  | [ .
                    , $pattern
                    ]
                  );
                (
                  # {a} -> {a: .a}
                  # {a: ...} -> {a: ...}
                  ( _consume(.ident) as [$rest, {$ident}]
                  | $rest
                  | _optional(_colon_pattern) as [$rest, $val]
                  | $rest
                  | [ .
                    , { key: $ident
                      , val: $val
                      }
                    ]
                  )
                //
                  # {"a"} -> {a: .a}
                  # {"a": ...} -> {a: ...}
                  ( _p("string") as [$rest, $string]
                  | $rest
                  | _optional(_colon_pattern) as [$rest, $val]
                  | $rest
                  | [ .
                    , { key_string:
                          {str: $string.term.str}
                      , val: $val
                      }
                    ]
                  )
                //
                  # {$a} -> {a: $a}
                  ( _consume(.binding) as [$rest, {$binding}]
                  | [ $rest
                    , {key: $binding}
                    ]
                  )
                //
                  # {(...): ...} -> {...: ...}
                  ( _p("subquery") as [$rest, $query]
                  | $rest
                  | _colon_pattern as [$rest, $val]
                  | $rest
                  | [ .
                    , { key_query: $query
                      , val: $val
                      }
                    ]
                  )
                )
              ) as [$rest, $key_patterns]
            | $rest
            | ( if .[0].rcurly then
                  # keep it to make repeat finish and consumed it below
                  [$rest, null]
                else
                  # or there must be a comma
                  ( _consume(.comma) as [$rest, $_]
                  | [$rest, null]
                  )
                end
              ) as [$rest, $_]
            | [$rest, $key_patterns]
            )
          ) as [$rest, $key_patterns]
        | $rest
        | _consume(.rcurly) as [$rest, $_]
        | [ $rest
          , {object: $key_patterns}
          ]
        )
      //
        ( _consume(.lsquare) as [$rest, $_]
        | $rest
        | _repeat(
            ( _p("pattern") as [$rest, $pattern]
            | $rest
            | _optional(
                # TODO: _one() etc?
                ( _consume(.comma) as [$rest, $_]
                | [$rest, null]
                )
              ) as [$rest, $_]
            | [$rest, $pattern]
            )
          ) as [$rest, $pattern]
        | $rest
        | _consume(.rsquare) as [$rest, $_]
        | [ $rest
          , {array: $pattern}
          ]
        )
      //
        ( _consume(.binding) as [$rest, {$binding}]
        | [ $rest
          , {name: $binding}
          ]
        )
      );

    # (<query>)
    def _subquery:
      ( _consume(.lparen)[0]
      | _p("query") as [$rest, $query]
      | $rest
      | _consume(.rparen) as [$rest, $_]
      | [ $rest
        , { term:
              { type: "TermTypeQuery"
              , query: $query
              }
          }
        ]
      );

    # ident
    # ident(<query>[;...])
    def _func:
      ( _consume(.ident) as [$rest, {$ident}]
      | $rest
      | ( _consume(.lparen)[0]
        | _repeat(
            ( _p("query") as [$rest, $arg]
            | $rest
            | _optional(
                # TODO: _one() etc?
                ( _consume(.semicolon) as [$rest, $_]
                | [$rest, null]
                )
              ) as [$rest, $_]
            | [$rest, $arg]
            )
          ) as [$rest, $args]
        | $rest
        | _consume(.rparen)[0]
        | [ .
          , { term:
                { type: "TermTypeFunc"
                , func:
                    { name: $ident
                    , args: $args
                    }
                }
            }
          ]
        )
        //
          [ .
          , { term:
                { type: "TermTypeFunc"
                , func:
                    {name: $ident}
                }
            }
          ]
      );

    # $name
    def _binding:
      ( _consume(.binding) as [$rest, {$binding}]
      | [ $rest
        , { term:
              { type: "TermTypeFunc"
              , func:
                  {name: $binding}
              }
          }
        ]
      );

    # [<query>]
    def _array:
      ( _consume(.lsquare)[0]
      | _optional(_p("query")) as [$rest, $query]
      | $rest
      | _consume(.rsquare) as [$rest, $_]
      | [ $rest
        , { term:
              { type: "TermTypeArray"
              , array:
                  {query: $query}
              }
          }
        ]
      );

    # reduce <term> as <binding> (<start-query>;<update-query>)
    def _reduce:
      ( _keyword("reduce")
      | _p("term") as [$rest, $term]
      | $rest
      | _keyword("as")
      | _p("pattern") as [$rest, $pattern]
      | $rest
      | _consume(.lparen)[0]
      | _p("query") as [$rest, $start]
      | $rest
      | _consume(.semicolon)[0]
      | _p("query") as [$rest, $update]
      | $rest
      | _consume(.rparen)[0]
      | [ .
        , { term:
            { type: "TermTypeReduce"
            , reduce:
                { term: $term.term
                , pattern: $pattern
                , start: $start
                , update: $update
                }
            }
          }
        ]
      );

    # foreach <term> as <binding> (<start-query>;<update-query>[;<extract-query>])
    def _foreach:
      ( _keyword("foreach")
      | _p("term") as [$rest, $term]
      | $rest
      | _keyword("as")
      | _p("pattern") as [$rest, $pattern]
      | $rest
      | _consume(.lparen)[0]
      | _p("query") as [$rest, $start]
      | $rest
      | _consume(.semicolon)[0]
      | _p("query") as [$rest, $update]
      | $rest
      | _optional(
          ( _consume(.semicolon) as [$rest, $_]
          | $rest
          | _p("query")
          )
        ) as [$rest, $extract]
      | $rest
      | _consume(.rparen) as [$rest, $_]
      | $rest
      | [ .
        , { term:
            { type: "TermTypeForeach"
            , foreach:
                ( { term: $term.term
                  , pattern: $pattern
                  , start: $start
                  , update: $update
                  }
                | if $extract then .extract = $extract else . end
                )
            }
          }
        ]
      );

    # if <cond> then <expr>
    # [elif <cond> then <expr>]*
    # [else expr]?
    # end
    def _if:
      ( _keyword("if")
      | _p("query") as [$rest, $cond]
      | $rest
      | _keyword("then")
      | _p("query") as [$rest, $then_]
      | $rest
      | _repeat(
          ( _keyword("elif")
          | _p("query") as [$rest, $cond]
          | $rest
          | _keyword("then")
          | _p("query") as [$rest, $then_]
          | $rest
          | [ .
            , { cond: $cond
              , then: $then_
              }
            ]
          )
        ) as [$rest, $elif_]
      | $rest
      | _optional(
          ( _keyword("else")
          | _p("query")
          )
        ) as [$rest, $else_]
      | $rest
      | _keyword("end")
      | [ .
        , { term:
              { type: "TermTypeIf"
              , if:
                  ( { cond: $cond
                    , then: $then_
                    , else: $else_
                    }
                  | if ($elif_ | length) > 0 then .elif = $elif_
                    else .
                    end
                  )
              }
          }
        ]
      );

    # def a: ...;
    # def a(f): ...;
    # def a(f; $v): ...;
    def _func_defs:
      _repeat(
        ( _keyword("def")
        | ( _consume(.at_ident)
          // _consume(.ident)
          ) as [$rest, $tok]
        | ($tok.ident // $tok.at_ident) as $name
        | $rest
        | ( ( _consume(.lparen)[0]
            | _repeat(
                ( ( ( _consume(.ident) as [$rest, {$ident}]
                    | [$rest, $ident]
                    )
                  //
                    ( _consume(.binding) as [$rest, {$binding}]
                    | [$rest, $binding]
                    )
                  ) as [$rest, $arg]
                | $rest
                | ( _consume(.semicolon)[0]
                  // .
                  )
                | [., $arg]
                )
              ) as [$rest, $args]
            | $rest
            | _consume(.rparen)[0]
            | _consume(.colon)[0]
            | _p("query") as [$rest, $body]
            | $rest
            | _consume(.semicolon) as [$rest, $_]
            | [ $rest
              , { name: $name
                , args: $args
                , body: $body
                }
              ]
            )
          //
            ( _consume(.colon)[0]
            | _p("query") as [$rest, $body]
            | $rest
            | _consume(.semicolon) as [$rest, $_]
            | [ $rest
              , { name: $name
                , body: $body
                }
              ]
            )
          )
        )
      );

    # []
    # .[]
    # [<query>]
    # .[<query>]
    # .name
    # ."name"
    # ?
    # as $v | <query>
    def _suffix:
      (
        # [] iter
        ( _optional(
            # TODO: _one() etc?
            ( _consume(.dot) as [$rest, $_]
            | [$rest, null]
            )
          ) as [$rest, $_]
        | $rest
        | _consume(.lsquare)[0]
        | _consume(.rsquare) as [$rest, $_]
        | [$rest, {iter: true}]
        )
      //
        # [...] query
        ( _optional(
            # TODO: _one() etc?
            ( _consume(.dot) as [$rest, $_]
            | [$rest, null]
            )
          ) as [$rest, $_]
        | $rest
        | _consume(.lsquare)[0]
        | _p("query") as [$rest, $start]
        | $rest
        | _consume(.rsquare) as [$rest, $_]
        | [ $rest
          , { index:
                {start: $start}
            }
          ]
        )
      //
        # [...:...]
        # [:...]
        # [...:]
        ( _consume(.lsquare)[0]
        | _optional(_p("query")) as [$rest, $start]
        | $rest
        | _consume(.colon)[0]
        | _optional(_p("query")) as [$rest, $end_]
        | $rest
        | _consume(.rsquare)[0]
        # fail if both missing
        | if $start == null and $end_ == null then empty end
        | [ .
          , { index:
                ( {is_slice: true}
                | if $start then .start = $start else . end
                | if $end_ then .end = $end_ else . end
                )
            }
          ]
        )
      //
        # .name index
        ( _consume(.index) as [$rest, {$index}]
        | [ $rest
          , { index:
                {name: $index}
            }
          ]
        )
      //
        # ."name" index
        ( _consume(.dot)[0]
        | _p("string") as [$rest, $string]
        | $rest
        | [ .
          , { index:
                { str:
                    {str: $string.term.str}
                }
            }
          ]
        )
      //
        # ? optional (try)
        ( _consume(.qmark) as [$rest, $_]
        | [ $rest
          , {optional: true}
          ]
        )
      //
        ( _keyword("as")
        | _p("pattern") as [$rest, $pattern]
        | $rest
        | _consume(.pipe)[0]
        | _p("query") as [$rest, $body]
        | $rest
        | [ .
          , { bind:
                { body: $body
                , patterns: [$pattern]
                }
              }
          ]
        )
      );

    # .
    def _identity:
      ( _consume(.dot) as [$rest, $_]
      | [ $rest
        , { term:
              {type: "TermTypeIdentity"}
          }
        ]
      );

    # .[<query>]
    # .[<query>:<query>]
    # .name
    # ."name"
    # TODO: share with _suffix? tricky because of leading dot
    def _index:
      ( ( _consume(.dot)[0]
        | _consume(.lsquare)[0]
        | _p("query") as [$rest, $query]
        | $rest
        | _consume(.rsquare) as [$rest, $_]
        | [ $rest
          , { term:
                { type: "TermTypeIndex"
                , index:
                    {start: $query}
              }
            }
          ]
        )
      //
        ( _consume(.dot)[0]
        | _consume(.lsquare)[0]
        | _optional(_p("query")) as [$rest, $start]
        | $rest
        | _consume(.colon)[0]
        | _optional(_p("query")) as [$rest, $end_]
        | $rest
        | _consume(.rsquare)[0]
        # fail is both missing
        | if $start == null and $end_ == null then empty else . end
        | [ .
          , { term:
                { type: "TermTypeIndex"
                , index:
                    ( {is_slice: true}
                    | if $start then .start = $start else . end
                    | if $end_ then .end = $end_ else . end
                    )
                }
            }
          ]
        )
      //
        ( _consume(.index) as [$rest, {$index}]
        | [ $rest
          , { term:
                { type: "TermTypeIndex"
                , index:
                    {name: $index}
                }
            }
          ]
        )
      //
        # ."name" index
        ( _consume(.dot)[0]
        | _p("string") as [$rest, $string]
        | $rest
        | [ .
          , { term:
                { type: "TermTypeIndex"
                , index:
                    { str:
                        {str: $string.term.str}
                    }
                }
            }
          ]
        )
      );

    # "abc"
    def _string_simple:
      _scalar("TermTypeString"; .string; {str: .string});

    # "abc \(123)"
    def _string_query:
      ( _consume(.string_start) as [$rest, {$string_start}]
      | $rest
      | _repeat(
          ( select(length > 0) # make sure there is something
          | _p("query")
          // _scalar("TermTypeString"; .string_middle; {str: .string_middle})
          )
        ) as [$rest, $queries]
      | $rest
      | _consume(.string_end) as [$rest, {$string_end}]
      | [ $rest
        , { term:
            { type: "TermTypeString"
            , queries:
                [ {term: {str: $string_start, type: "TermTypeString"}}
                , ( $queries[]
                  | if .term and .term.type == "TermTypeString" then .
                    else {term: {query: ., type: "TermTypeQuery"}}
                    end
                  )
                , {term: {str: $string_end, type: "TermTypeString"}}
                ]
            }
          }
        ]
      );

    def _string:
      ( _string_simple
      // _string_query
      );

    # @format "abc"
    def _format_string:
      ( _consume(.at_ident) as [$rest, {$at_ident}]
      | $rest
      | _string as [$rest, $string]
      | [ $rest
        , { term:
              { type: "TermTypeFormat"
              , format: $at_ident
              , str: $string
              }
          }
        ]
      );

    # try <query1>
    # try <query1> catch <query1>
    # <query1> is a query not allow infix operators
    def _try:
      ( _keyword("try")
      | _p("query1") as [$rest, $body]
      | $rest
      | _optional(
          ( _keyword("catch")
          | _p("query1")
          )
        ) as [$rest, $catch_]
      | $rest
      | [ .
        , { term:
              { type: "TermTypeTry"
              , try:
                  ( {body: $body}
                  | if $catch_ then .catch = $catch_ else . end
                  )
              }
          }
        ]
      );

    # +<term> etc
    def _unary_op(f; $op):
      ( _consume(f)[0]
      | _p("term") as [$rest, $term]
      | $rest
      | [ .
        , { term:
              { type: "TermTypeUnary"
              , unary:
                  { op: $op
                  , term: $term.term
                  }
              }
          }
        ]
      );

    # ..
    # transform .. into recurse call
    def _recurse:
      ( _consume(.dotdot) as [$rest, $_]
      | [ $rest
        , { term:
              { type: "TermTypeFunc"
              , func:
                  {name: "recurse"}
              }
          }
        ]
      );

    ( . #debug({_p: $type, dot: .})
    | if $type == "query" then
        _op_prec_climb(0; false)
      elif $type == "keyval_query" then
        # keyval query only allows | operator
        _op_prec_climb(0; .pipe | not)
      elif $type == "query1" then
        # used by _op_prec_climb, exist to fix infinite recursion
        # does not include infix operators
        ( _p("func_defs") as [$rest, $func_defs]
        | $rest
        | ( if length == 0 then
              [ .
              , { term:
                    {type: "TermTypeIdentity"}
                }
              ]
            else
              _p("term")
            end
          ) as [$rest, $query]
        | $query
        | if ($func_defs | length) > 0 then
            .func_defs = $func_defs
          else .
          end
        | [$rest, .]
        )
      elif $type == "term" then
        # "keyword" ident parsing first
        ( ( _p("if")
          // _p("reduce")
          // _p("foreach")
          // _p("try")
          // _p("true")
          // _p("false")
          // _p("null")
          // _p("func")
          // _p("number")
          // _p("string")
          // _p("format_string")
          // _p("array")
          // _p("subquery") # TODO: rename?
          // _p("object")
          // _p("index") #.name
          // _p("identity") # .
          // _p("binding")
          // _p("unary_plus")
          // _p("unary_minus")
          // _p("recurse") # ".."
          ) as [$rest, $term]
        | $rest
        | _repeat(_p("suffix")) as [$rest, $suffix_list]
        | $rest
        | [ .
          , ( $term
            | if ($suffix_list | length) > 0 then
                .term.suffix_list = $suffix_list
              else .
              end
            )
          ]
        )
      elif $type == "suffix" then _suffix
      elif $type == "if" then _if
      elif $type == "func_defs" then _func_defs
      elif $type == "true" then _scalar("TermTypeTrue"; .ident == "true"; .)
      elif $type == "false" then _scalar("TermTypeFalse"; .ident == "false"; .)
      elif $type == "null" then _scalar("TermTypeNull"; .ident == "null"; .)
      elif $type == "number" then _scalar("TermTypeNumber"; .number; {number: .number})
      elif $type == "string" then _string
      elif $type == "format_string" then _format_string
      elif $type == "index" then _index
      elif $type == "identity" then _identity
      elif $type == "array" then _array
      elif $type == "object" then _object
      elif $type == "subquery" then _subquery
      elif $type == "func" then _func
      elif $type == "binding" then _binding
      elif $type == "reduce" then _reduce
      elif $type == "foreach" then _foreach
      elif $type == "try" then _try
      elif $type == "unary_plus" then _unary_op(.plus; "+")
      elif $type == "unary_minus" then _unary_op(.dash; "-")
      elif $type == "recurse" then _recurse
      elif $type == "pattern" then _pattern
      else error("unknown type \($type)")
      end
    );
  ( ( _p("query")
    | if .[0] != [] then error("tokens left: \(.)") else . end
    | .[1]
    )
  // error("parse error: \(.)")
  );

def _tojson_stream($opts):
  # see jq jv_print.c:jv_dump_term for the reference color printing logic
  def _c_null: 0;
  def _c_false: 1;
  def _c_true: 2;
  def _c_number: 3;
  def _c_string: 4;
  def _c_array: 5;
  def _c_object: 6;
  def _c_field: 7;
  def _color($id):
    if $opts.colors != null then
      $opts.colors[$id], ., "\u001b[0m"
    else .
    end;
  ( ( if $opts.print_pretty | not then ["", "", ""]
      elif $opts.indent == "tab" then ["\t", " ", "\n"]
      else [$opts.indent * " ", " ", "\n"]
      end
    ) as [$indent, $space, $newline]
  | def _f($prefix):
      ( type as $t
      | if $t == "null" then "null" | _color(_c_null)
        elif $t == "string" then tojson | _color(_c_string)
        elif $t == "number" then tojson | _color(_c_number)
        elif $t == "boolean" then
          if . then "true" | _color(_c_true)
          else "false" | _color(_c_false)
          end
        elif $t == "array" then
          if length == 0 then "[]" | _color(_c_array)
          else
            ( ($prefix + $indent) as $elem_prefix
            | ("[" | _color(_c_array))
            , $elem_prefix, (.[0] | _f($elem_prefix))
            , ( .[1:][]
              | ("," | _color(_c_array))
              , $elem_prefix, _f($elem_prefix)
              )
            , $prefix, ("]" | _color(_c_array))
            )
          end
        elif $t == "object" then
          if length == 0 then "{}" | _color(_c_object)
          else
            ( ($prefix + $indent) as $elem_prefix
            | to_entries as $entries
            | ("{" | _color(_c_object))
            , ( $entries[0]
              | $elem_prefix
              , (.key | tojson | _color(_c_field))
              , (":" | _color(_c_object)), $space
              , (.value | _f($elem_prefix))
              )
            , ( $entries[1:][]
              | ("," | _color(_c_object))
              , $elem_prefix
              , (.key | tojson | _color(_c_field))
              , (":" | _color(_c_object)), $space
              , (.value | _f($elem_prefix))
              )
            , $prefix, ("}" | _color(_c_object))
            )
          end
        else _internal_error("unknown type \($t)")
        end
      );
    _f($newline)
  );
def _tojson: [_tojson_stream({indent: 0})] | join("");

def dump($opts):
  ( if $opts.raw_output and type == "string" then
      if $opts.raw_output0 and contains("\u0000") then
        error("Cannot dump a string containing NUL with --raw-output0 option")
      end
    else _tojson_stream($opts)
    end
  , if $opts.stream_sep != null then $opts.stream_sep else empty end
  );

def undefined_func_error:
  error("undefined function \(.name)");

def func_name($name; $args):
  # no .args if 0 args
  if $name | startswith("$") then $name
  else
    ( ( ($args // []) | length) as $argc_count
    | "\($name)/\($argc_count)"
    )
  end;

# convert func_defs array to env object
# TODO: more efficient env modelling? maybe ok if underlying jq implementation
# uses some kind of data structure sharing
def func_defs_to_env($env):
  reduce .[] as $f (
    $env;
    ( . as $func_env
    | .
    + { (func_name($f.name; $f.args)):
          ( $f
          | .env = $func_env
          )
      }
    )
  );

def eval_ast($query; $path; $env; undefined_func):
  def _setpath($path; $v):
    def _f($p):
      if $p | length == 0 then
        $v
      else
        ( $p[0] as $p0
        | ($p0 | type) as $t
        #| debug({type: type, $t, $p0, $t, dot: .})
        | if . == null then
            if $t == "number" then
              [range($p0+1) | null]
            else {}
            end
          end
        | if (type == "object" and $t == "string") or
            (type == "array" and $t == "number") then
            .[$p0] |= _f($p[1:])
          else
            error("cannot index \(type) with \($t) \($p0)")
          end
        )
      end;
    _f($path);

  def _e($query; $path; $env):
    ( . # debug({c: ., $query, $path, $env})
    | ( $query
      # TODO: jaq: destruct null index
      | if .term == null then .term = {} end
      | if .term.suffix_list == null then .term.suffix_list = [{}] end
      ) as
        { term:
            { type: $type
            , suffix_list: [{$optional}]
            }
        , $op
        , $func_defs
        }
    | ( ( ($func_defs // [])
        | func_defs_to_env($env)
        )
      ) as $query_env
    |
      # eval a index, is also used by _e_suffix
      def _e_index($index; $query_path; $query_input; $opt):
        try
          ( $index as
              { $name
              , $str
              , $is_slice
              , $start
              , end: $end_
              }
          | ( # jaq: for null provide object or array
              if . == null then
                if $name then if $name | type == "string" then {} else [] end
                elif $str then {}
                end
              end
            ) as $input
          | if $name then [($query_path + [$name]), $input[$name]]
            elif $str then [($query_path + [$str.str]), $input[$str.str]]
            elif $is_slice then
              ( $query_input
              | ( if $start then _e($start; []; $query_env)[1]
                  else 0 end
                ) as $vs
              | ( if $end_ then _e($end_; []; $query_env)[1]
                  else $input | length
                  end
                ) as $ve
              | [ [null]
                , $input[$vs:$ve]
                ]
              )
            elif $start then
              ( $query_input
              | _e($start; []; $query_env) as [$_, $v]
              | [ ($query_path + [$v])
                , # jaq: only index if non-null
                  ( $input
                  | if . != null then .[$v] end
                  )
                ]
              )
            else . # TODO: error?
            end
          )
        catch
          if $opt then empty
          else error
          end;

      # "str" or "str \(..) str" with format
      # called with query arg from _string and _format
      def _string($query; $format_func):
        if $query.term.str then [[null], $query.term.str]
        else
          ( . as $input
          | def _f($str_parts):
              if length == 0 then $str_parts | join("")
              else
                ( .[0] as $q
                | .[1:] as $rest
                | $input
                | _e($q; []; $query_env) as [$_, $v]
                | $v
                | ( if $q.term.query then
                      ( _e(
                          { term:
                              { type: "TermTypeFunc"
                              , func: {name: $format_func}
                              }
                          };
                          [];
                          $query_env
                        ) as [$_, $v]
                      | $v
                      )
                    else .
                    end
                  ) as $v
                | $rest
                | _f($str_parts + [$v])
                )
              end;
            $query.term.queries
          | _f([])
          | [[null], .]
          )
        end;

      def _string:
        _string($query; "tostring");

      def _format:
        # @name "abc \(.def)" -> "abc \(.def | @name)"
        _string($query.term.str; $query.term.format);

      # .
      def _identity:
        [$path, .];

      # destructing pattern to env
      def _e_pattern($input):
        ( def _f($input; $env):
            if length == 0 then $env
            else
              if .name then
                ( . as {$name}
                | $env
                | .[$name] = {value: $input}
                )
              elif .array then
                reduce (.array | to_entries)[] as $kv (
                  $env;
                  ( . as $env
                  | $kv.value
                  | _f($input[$kv.key]; $env)
                  )
                )
              elif .object then
                reduce .object[] as $kv (
                  $env;
                  ( . as $env
                  | ( if $kv.key and ($kv.val | not) then [$kv.key[1:], {name: $kv.key}]
                      elif $kv.key then [$kv.key, $kv.val]
                      elif $kv.key_string then [$kv.key_string.str, $kv.val]
                      elif $kv.key_query then
                        # TODO: {a: 1, b: 2} as {("a","b"): $a} | $a -> 1, 2, probably can't use reduce
                        ( _e($kv.key_query; $path; $query_env)[1]
                        | [., $kv.val]
                        )
                      else _internal_error("unreachable")
                      end
                    ) as [$key, $val]
                  | $val
                  | _f($input[$key]; $env)
                  )
                )
              else _internal_error("unreachable")
              end
            end;
          _f($input; {})
        );

      # .name
      def _index:
        _e_index(
          $query.term.index;
          $path;
          .;
          ( $query.term.suffix_list
          | if . != null then .[0].optional
            else false
            end
          )
        );

      def _func:
        def _fromjson:
          def _f:
            ( . as $v
            | .term.type
            | if . == "TermTypeNull" then null
              elif . == "TermTypeTrue" then true
              elif . == "TermTypeFalse" then false
              elif . == "TermTypeString" then $v.term.str
              elif . == "TermTypeNumber" then $v.term.number | tonumber
              elif . == "TermTypeObject" then
                ( $v.term.object.key_vals // []
                | map(
                    { key: .key_string.str
                    , value: (.val.queries[0] | _f)
                    }
                  )
                | from_entries
                )
              elif . == "TermTypeArray" then
                ( def _a: if .op then .left, .right | _a end;
                  [$v.term.array.query // empty | _a | _f]
                )
              else _internal_error("unknown term")
              end
            );
          try
            (lex | parse | _f)
          catch
            error("fromjson only supports constant literals");

        ( . as $input
        | $query.term.func as {$name, $args}
        | func_name($name; $args) as $name
        | $query_env[$name] as $e
        # jaq: null | has() is an error
        | if $e != null and ($e | has("value")) then [[null], $e.value]
          elif $e != null and $e.body then
            ( ($e.args // []) as $func_args
            | ($args // []) as $call_args
            | ( $func_args
              | with_entries(
                  ( ( .value
                    # when using a $<name> binding arg <name> is also available as a lambda
                    | if startswith("$") then .[1:] else . end
                    ) as $name
                  | { key: ($name + "/0")
                    , value:
                        { body: $call_args[.key]
                        # save current env
                        , env: $query_env
                        , lambda: true
                        }
                    }
                  )
                )
              ) as $lambda_env
            # if not lambda inject the function in it's own env to allow recursion
            # TODO: find a better way
            | ( if $e.lambda then {}
                else {($name): $e}
                end
              ) as $self_env
            | $func_args
            | to_entries
            | map(
                ( select(.value | startswith("$"))
                | [ .value
                  , $call_args[.key]
                  ]
                )
              )
            | def _f($env):
                if length == 0 then $env
                else
                  ( .[0] as [$name, $ast]
                  | .[1:] as $rest
                  | $input
                  | _e($ast; []; $query_env) as [$_, $v]
                  | $rest
                  | _f($env | .[$name] = {value: $v})
                  )
                end;
              _f({}) as $bindings_env
            | $input
            | ($e.env + $bindings_env + $lambda_env + $self_env) as $call_env
            | _e($e.body; $path; $call_env)
            )
          else
            ( def a0: _e($args[0]; $path; $query_env)[1];
              def a1: _e($args[1]; $path; $query_env)[1];
              def a2: _e($args[2]; $path; $query_env)[1];
              if $name == "empty/0"    then empty
              elif $name == "debug/0"  then debug as $_ | [$path, .]
              elif $name == "type/0"   then [[null], type]
              elif $name == "length/0" then [[null], length]
              elif $name == "keys/0"   then [[null], keys]
              elif $name == "has/1"    then
                ( a0 as $a0
                | [[null], has($a0)]
                )
              elif $name == "delpaths/1" then
                ( a0 as $a0
                | [[null], _delpaths($a0)]
                )
              elif $name == "explode/0"  then [[null], explode]
              elif $name == "implode/0"  then [[null], implode]
              elif $name == "tonumber/0" then [[null], tonumber]
              # TODO: implement in jqjq?
              elif $name == "tostring/0" then [[null], tostring]
              elif $name == "tojson/0"   then [[null], _tojson]
              elif $name == "fromjson/0" then [[null], _fromjson]
              # TODO: make args general
              # note "null | error" is same as empty
              elif $name == "error/0"    then error
              elif $name == "error/1"    then
                # TODO: see comment in _try
                ( a0 as $a0
                | error($a0)
                )
              elif $name == "halt_error/1" then [[null], halt_error(a0)]
              elif $name == "getpath/1" then
                ( a0 as $a0
                | [ $path+$a0
                  , _getpath($a0)
                  ]
                )
              elif $name == "setpath/2" then
                ( a0 as $a0
                | a1 as $a1
                | [ []
                  , _setpath($a0; $a1)
                  ]
                )
              elif $name == "path/1" then
                ( _e($args[0]; []; $query_env) as [$p, $_v]
                # TODO: try/catch error
                | if $p | length > 0 and first == null then
                    # TODO: include path and value?
                    error("invalid path expression")
                  else .
                  end
                | [[null], $p]
                )
              elif $name == "acos/0"        then [[null], acos]
              elif $name == "acosh/0"       then [[null], acosh]
              elif $name == "asin/0"        then [[null], asin]
              elif $name == "asinh/0"       then [[null], asinh]
              elif $name == "atan/0"        then [[null], atan]
              elif $name == "atanh/0"       then [[null], atanh]
              elif $name == "cbrt/0"        then [[null], cbrt]
              elif $name == "ceil/0"        then [[null], ceil]
              elif $name == "cos/0"         then [[null], cos]
              elif $name == "cosh/0"        then [[null], cosh]
              elif $name == "erf/0"         then [[null], erf]
              elif $name == "erfc/0"        then [[null], erfc]
              elif $name == "exp/0"         then [[null], exp]
              elif $name == "exp10/0"       then [[null], exp10]
              elif $name == "exp2/0"        then [[null], exp2]
              elif $name == "expm1/0"       then [[null], expm1]
              elif $name == "fabs/0"        then [[null], fabs]
              elif $name == "floor/0"       then [[null], floor]
              elif $name == "gamma/0"       then [[null], gamma]
              elif $name == "j0/0"          then [[null], j0]
              elif $name == "j1/0"          then [[null], j1]
              elif $name == "lgamma/0"      then [[null], lgamma]
              elif $name == "log/0"         then [[null], log]
              elif $name == "log10/0"       then [[null], log10]
              elif $name == "log1p/0"       then [[null], log1p]
              elif $name == "log2/0"        then [[null], log2]
              elif $name == "logb/0"        then [[null], logb]
              elif $name == "nearbyint/0"   then [[null], nearbyint]
              #elif $name == "pow10/0"      then [[null], pow10]
              elif $name == "rint/0"        then [[null], rint]
              elif $name == "round/0"       then [[null], round]
              elif $name == "significand/0" then [[null], significand]
              elif $name == "sin/0"         then [[null], sin]
              elif $name == "sinh/0"        then [[null], sinh]
              elif $name == "sqrt/0"        then [[null], sqrt]
              elif $name == "tan/0"         then [[null], tan]
              elif $name == "tanh/0"        then [[null], tanh]
              elif $name == "tgamma/0"      then [[null], tgamma]
              elif $name == "trunc/0"       then [[null], trunc]
              elif $name == "y0/0"          then [[null], y0]
              elif $name == "y1/0"          then [[null], y1]
              elif $name == "match/2"       then match(a0; a1) | [[null], .]
              elif $name == "test/2"        then test(a0; a1) | [[null], .]
              elif $name == "gsub/2"        then gsub(a0; a1) | [[null], .]
              elif $name == "gsub/3"        then gsub(a0; a1; a2) | [[null], .]
              elif $name == "atan2/2"       then [[null], atan2(a0; a1)]
              elif $name == "copysign/2"    then [[null], copysign(a0; a1)]
              elif $name == "drem/2"        then [[null], drem(a0; a1)]
              elif $name == "fdim/2"        then [[null], fdim(a0; a1)]
              elif $name == "fmax/2"        then [[null], fmax(a0; a1)]
              elif $name == "fmin/2"        then [[null], fmin(a0; a1)]
              elif $name == "fmod/2"        then [[null], fmod(a0; a1)]
              # TODO: in jq docs but seem missing
              #elif $name == "frexp/2" then [[null],frexp(a0; a1)]
              elif $name == "hypot/2"       then [[null], hypot(a0; a1)]
              elif $name == "jn/2"          then [[null], jn(a0; a1)]
              elif $name == "ldexp/2"       then [[null], ldexp(a0; a1)]
              # TODO: in jq docs but seem missing
              # elif $name == "modf/2" then [[null],modf(a0; a1)]
              elif $name == "nextafter/2"   then [[null], nextafter(a0; a1)]
              elif $name == "nexttoward/2"  then [[null], nexttoward(a0; a1)]
              elif $name == "pow/2"         then [[null], pow(a0; a1)]
              elif $name == "remainder/2"   then [[null], remainder(a0; a1)]
              elif $name == "scalb/2"       then [[null], scalb(a0; a1)]
              elif $name == "scalbln/2"     then [[null], scalbln(a0; a1)]
              elif $name == "yn/2"          then [[null], yn(a0; a1)]
              elif $name == "fma/3"         then [[null], fma(a0; a1; a2)]
              else
                ( { input: $input
                  , name: $name
                  , path: $path
                  , args: $args
                  , env: $query_env
                  }
                | undefined_func
                )
              end
            )
          end
        );

      # transform key_vals into array of [<key>, <val>] pairs and fill in optional key query if needed.
      # output objects by iterate thru pairs and for each key and val output produce a new object, possibly
      # based on an earlier object, for that output combination
      def _object:
        ( . as $input
        | $query.term.object.key_vals
        | map(
            ( def _term_str:
                { term:
                    { type: "TermTypeString"
                    , str: .
                    }
                };
              . as $kv
            | if $kv.key then
                [ ( $kv.key
                  | if startswith("$") then .[1:] else . end
                  | _term_str
                  )
                , ( # TODO: jaq: null index
                    ( $kv.val
                    | values
                    | .queries[0]
                    )
                  //
                    ( $kv.key
                    | if startswith("$") then
                        { term:
                            { type: "TermTypeFunc"
                            , func: { name: .}
                            }
                        }
                      else
                        { term:
                            { type: "TermTypeIndex"
                            , index: { name: .}
                            }
                        }
                      end
                    )
                  )
                ]
              elif $kv.key_string then
                ( [ ( $kv.key_string.str
                    | _term_str
                    )
                  , ( # TODO: jaq: null index
                      ( $kv.val
                      | values
                      | .queries[0]
                      )
                    //
                      { term:
                          { type: "TermTypeIndex"
                          , index:
                              { name: $kv.key_string.str
                              }
                          }
                      }
                    )
                  ]
                )
              elif $kv.key_query then
                [ $kv.key_query
                , $kv.val.queries[0]
                ]
              else _internal_error("unknown object key")
              end
            )
          )
        | def _f($obj):
            if length == 0 then $obj
            else
              ( .[0] as [$key_ast, $val_ast]
              | .[1:] as $rest
              | $input
              | _e($key_ast; []; $query_env) as [$_, $k]
              | _e($val_ast; []; $query_env) as [$_, $v]
              | $rest
              | _f($obj | .[$k] = $v)
              )
            end;
          _f({})
        | [[null], .]
        );

      def _array:
        [ [null]
        # .query only set if there was a query
        , [ _e($query.term.array.query // empty; []; $query_env) as [$_, $v]
          | $v
          ]
        ];

      # transform into array of [<cond>, <then>] ast pairs and fill in last else.
      # jq if works by evaluating pairs in order with same input, for each truthiness output of
      # <cond> evaluate <then> and for each falseness output evaluate next pair.
      def _if:
        ( . as $input
        | $query.term as {if: $if_}
        | [ # if <cond> then <then> ...
            [ $if_.cond
            , $if_.then
            ]
            # [elif <cond> then <then>]*
            , ( $if_.elif[]?
              | [.cond, .then]
              )
            , if $if_.else then
                # else <then> end -> else true then <then> end
                [ {term: {type: "TermTypeTrue"}}
                , $if_.else
                ]
              else
                # end -> else true then . end
                [ {term: {type: "TermTypeTrue"}}
                , {term: {type: "TermTypeIdentity"}}
                ]
              end
          ]
        | def _f:
            # does not have base case as we know last else will be true
            ( .[0] as [$cond, $then_]
            | ( $input
              | _e($cond; $path; $query_env)
              ) as [$_, $v]
            | if $v then
                ( $input
                | _e($then_; $path; $query_env)
                )
              else .[1:] | _f
              end
            );
          _f
        );

      def _reduce:
        # TODO: possible to do with function instead of reduce?
        # TODO: path(reduce) behavior
        ( $query.term.reduce as
            { $term
            , pattern: {name: $name}
            , start: $start
            , update: $update
            }
        | _e($start; $path; $query_env) as [$start_path, $start_v]
        | reduce _e({term: $term}; $start_path; $query_env) as [$p, $v] (
            [$start_path, $start_v];
            ( . as [$p, $state]
            | $state
            | _e($update; $p; $query_env + {($name): {value: $v}})
            )
          )
        );

      def _foreach:
        # TODO: possible to do with function instead of foreach?
        # TODO: path(foreach) behavior
        ( $query.term.foreach as
            { $term
            , pattern: {name: $name}
            , start: $start
            , update: $update
            , extract: $extract
            }
        | ( $extract
          //
            { term:
                { type: "TermTypeIdentity"}
            }
          ) as $extract
        | _e($start; $path; $query_env) as [$start_path, $start_v]
        | foreach _e({term: $term}; $path; $query_env) as [$_, $v] (
            [$start_path, $start_v];
            ( . as [$p, $state]
            | $state
            | _e($update; $p; $query_env + {($name): {value: $v}})
            );
            ( . as [$update_p, $update_v]
            | $update_v
            | _e($extract; $update_p; $query_env + {($name): {value: $v}})
            )
          )
        );

      def _try:
        ( $query.term.try as {$body, catch: $catch_}
        | ( $catch_
          //
            { term:
                { type: "TermTypeFunc"
                , func:
                    { name: "empty"
                    }
                ,
              }
            }
          ) as $catch_
        # TODO: will catch jqjq bugs causing error
        # do own backtracking? sentinel value somehow?
        | try
            _e($body; $path; $query_env)
          catch
            if _is_internal_error then error
            else _e($catch_; $path; $query_env)
            end
        );

      def _unary:
        ( $query.term.unary as {$op, $term}
        | def _f: _e({term: $term}; $path; $query_env);
          # TODO: not +. as jq don't support + unary operator
          if $op == "+" then _f[1] | [[null], .]
          elif $op == "-" then _f[1] | [[null], -.]
          else _internal_error("unsupported unary op: \($query)")
          end
        );

      # TODO: [["a","b"],["c","d"]][0,1][0,1] -> "a","c","b","d"
      # TODO: each suffix gets the term input as input
      # TODO: rewrite this
      def _e_suffix($suffix; $path; $input; $opt):
        ( . as [$p, $v]
        | $input
        | if $suffix.bind then
            # as $name | <body>
            ( $suffix.bind as {$body, patterns: [$pattern]}
            | ($pattern | _e_pattern($v)) as $pattern_env
            | _e(
                $suffix.bind.body;
                $path;
                $query_env + $pattern_env
              )
            )
          elif $suffix.index then
            # .<index>
            try
              ( $v
              | _e_index($suffix.index; $p; $input; $opt)
              )
            catch
              # TODO: share optional code somehow below and in _e_index
              if $opt then empty
              else error
              end
          elif $suffix.iter then
            # .[]
            try
              ( $v
              | keys[] as $key
              | [($p + [$key]), $v[$key]]
              )
            catch
              if $opt then empty
              else error
              end
          else _internal_error("unknown suffix: \($suffix)")
          end
        );

      def _e_suffix_list($input; $path):
        ( $query as {term: {$suffix_list}}
        | def _f($suffix_list):
            if ($suffix_list | length) == 0 then .
            # .a.b?? case, just skip extra optional "?"
            elif $suffix_list[0].optional then _f($suffix_list[1:])
            else
              ( ($suffix_list[1] // {}).optional as $opt # TOOO: jaq null index
              | $suffix_list[if $opt then 2 else 1 end:] as $n
              | _e_suffix($suffix_list[0]; $path; $input; $opt)
              | _f($n)
              )
            end;
          _f($suffix_list)
        );

      if $type then
        ( . as $input
        | try
            if $type == "TermTypeNull"       then [[], null] # should be [null] also? jq bug?
            elif $type == "TermTypeNumber"   then [[null], ($query.term.number | tonumber)]
            elif $type == "TermTypeString"   then _string
            elif $type == "TermTypeFormat"   then _format
            elif $type == "TermTypeTrue"     then [[null], true]
            elif $type == "TermTypeFalse"    then [[null], false]
            elif $type == "TermTypeIdentity" then _identity
            elif $type == "TermTypeIndex"    then _index
            elif $type == "TermTypeFunc"     then _func
            elif $type == "TermTypeObject"   then _object
            elif $type == "TermTypeArray"    then _array
            elif $type == "TermTypeIf"       then _if
            elif $type == "TermTypeReduce"   then _reduce
            elif $type == "TermTypeForeach"  then _foreach
            elif $type == "TermTypeQuery"    then _e($query.term.query; $path; $query_env)
            elif $type == "TermTypeTry"      then _try
            elif $type == "TermTypeUnary"    then _unary
            else _internal_error("unsupported term: \($query)")
            end
          catch
            if _is_internal_error then error # forward internal eror
            elif $optional then empty # query?
            else error
            end
        | if $query.term.suffix_list then _e_suffix_list($input; $path)
          else .
          end
        )
      elif $op then
        ( $query as {$left, $right}
        | def _l: _e($left; $path; $query_env);
          def _r: _e($right; $path; $query_env);
          if $op == "," then _l, _r
          elif $op == "|" then
            ( _e($left; $path; $query_env) as [$p, $v]
            | $v
            | _e($right; $p; $query_env)
            )
          elif $op == "or"  then _l[1] or _r[1] | [[null], .]
          elif $op == "and" then _l[1] and _r[1] | [[null], .]
          elif $op == "=="  then _l[1] == _r[1] | [[null], .]
          elif $op == "!="  then _l[1] != _r[1] | [[null], .]
          elif $op == "<"   then _l[1] < _r[1] | [[null], .]
          elif $op == "<="  then _l[1] <= _r[1] | [[null], .]
          elif $op == ">"   then _l[1] > _r[1] | [[null], .]
          elif $op == ">="  then _l[1] >= _r[1] | [[null], .]
          elif $op == "+"   then _l[1] + _r[1] | [[null], .]
          elif $op == "-"   then _l[1] - _r[1] | [[null], .]
          elif $op == "*"   then _l[1] * _r[1] | [[null], .]
          elif $op == "/"   then _l[1] / _r[1] | [[null], .]
          elif $op == "%"   then _l[1] % _r[1] | [[null], .]
          elif $op |
              . == "=" or
              . == "|=" or
              . == "+=" or
              . == "-=" or
              . == "*=" or
              . == "/=" or
              . == "%=" or
              . == "//" then
            # transform <lhr> <op> <rhs> to _assign/_update(lhr; "<op>"; rhs)
            _e(
              { term:
                  { type: "TermTypeFunc"
                  , func:
                      { name:
                          { "=":  "_assign"
                          , "|=": "_update"
                          , "+=": "_update"
                          , "-=": "_update"
                          , "*=": "_update"
                          , "/=": "_update"
                          , "%=": "_update"
                          , "//": "_alt"
                          }[$op]
                      , args:
                          [ $left
                          , { term:
                                { type: "TermTypeString"
                                , str: $op
                                }
                            }
                          , $right
                          ]
                      }
                  }
              };
              $path;
              $query_env
            )
          else _internal_error("unsupported op: \($query)")
          end
        )
      else _internal_error("unsupported query: \($query)")
      end
    );
  try
    _e($query; []; $env)
  catch
    if _is_internal_error then _unwrap_internal_error | error("internal error: \(.)")
    else error
    end;
def eval_ast($ast):
  eval_ast($ast; []; {}; undefined_func_error);

def _builtins_src: "
def debug(msgs): (msgs | debug | empty), .;
def halt_error: halt_error(5);

# used to implement lhs = rhs
def _assign(lhs; $op; rhs):
  ( rhs as $v
  | reduce path(lhs) as $p (
      .;
      setpath($p; $v)
    )
  );

# used to implement lhs |= rhs and lhs op= rhs
def _update(lhs; $op; rhs):
  ( . as $c
  | def _f:
      if $op == \"|=\" then rhs
      elif $op == \"+=\" then . + ($c | rhs)
      elif $op == \"-=\" then . - ($c | rhs)
      elif $op == \"*=\" then . * ($c | rhs)
      elif $op == \"/=\" then . / ($c | rhs)
      elif $op == \"%=\" then . % ($c | rhs)
      else error(\"unknown _update op\")
      end;
    reduce path(lhs) as $p (
      .;
      setpath($p; getpath($p) | _f)
    )
  );

# used to implement lhs // rhs
# TODO: rewrite mess, use label/break once added?
def _alt(lhs; $op; rhs):
  ( \"__jqjq_alt_break\" as $b
  | try
      ( foreach (
            ( (lhs | [\"lhs\",.])
            , [\"end\",null]
            , (rhs | [\"rhs\",.])
            )
          ) as $v (
          0;
          if $v[0] == \"lhs\" then
            if $v[1] then . + 1
            else .
            end
          elif $v[0] == \"end\" then
            if . > 0 then error($b)
            else .
            end
          else .
          end;
          # TODO: jaq: foreach empty update backtracks
          if $v[0] == \"lhs\" then
            if $v[1] then $v
            else empty
            end
          elif $v[0] == \"end\" then
            empty
          else $v
          end
        )
      | .[1]
      )
    catch
      if . == $b then empty
      else error
      end
  );

def _is_array: type == \"array\";
def _is_boolean: type == \"boolean\";
def _is_null: type == \"null\";
def _is_number: type == \"number\";
def _is_object: type == \"object\";
def _is_string: type == \"string\";
def _is_scalar:
  _is_boolean or
  _is_null or
  _is_number or
  _is_string;

# some are early as they are used by others

def not: if . then false else true end;

def select(f): if f then . else empty end;

def map(f): [.[] | f];

def arrays: select(_is_array);
def objects: select(_is_object);
def iterables: select(_is_array or _is_object);
def booleans: select(_is_boolean);
def numbers: select(_is_number);
# TODO:
# def normals: select(_is_normal);
# def finites: select(_is_finite);
def strings: select(_is_string);
def nulls: select(_is_null);
def values: select(_is_null | not);
def scalars: select(_is_scalar);

def add: reduce .[] as $v (null; . + $v);

def startswith($s): .[0:$s | length] == $s;
def endswith($s): .[$s | -length:] == $s;

def _nwise($n):
  def n:
    if length <= $n then .
    else .[0:$n], (.[$n:] | n)
    end;
  n;
def splits($re; flags):
  ( . as $s
  | [match($re; \"g\" + flags) | (.offset, .offset + .length)]
  | [0] + . + [$s | length]
  | _nwise(2)
  | $s[.[0]:.[1]]
  );
def splits($re): splits($re; null);
def split($re; flags): [splits($re; flags)];

def _strsplit($delim; $acc):
  if . == \"\" then $acc
  elif startswith($delim) then
    $acc, (.[$delim | length:] | _strsplit($delim; \"\"))
  else .[:1] as $c | .[1:] | _strsplit($delim; $acc + $c)
  end;
def _strsplit0:
  if . == \"\" then empty else .[:1], (.[1:] | _strsplit0) end;
def split($s):
  if type != \"string\" or ($s | type != \"string\") then
    error(\"split input and separator must be strings\")
  elif . == \"\" then []
  elif $s == \"\" then [_strsplit0]
  else [_strsplit($s; \"\")]
  end;

def join($s):
  if length == 0 then \"\"
  else
    ( map(
        ( if . == null then \"\" end
        | $s
        , if _is_scalar then tostring end
        )
      )[1:]
    | add
    )
  end;

def _minmax(f; c):
  if . == null then null
  else reduce .[] as $v (
      .[0];
      if [(. | f), ($v | f)] | c then . else $v end
    )
  end;
def min_by(f): _minmax(f; .[0] <= .[1]);
def min: min_by(.);
def max_by(f): _minmax(f; .[0] > .[1]);
def max: max_by(.);

def range($from; $to; $by):
  def _f(stop):
    if stop then empty
    else ., (. + $by | _f(stop))
    end;
  if $by == 0 then empty
  elif $by > 0 then $from | _f(. >= $to)
  else $from | _f(. <= $to)
  end;
def range($from; $to): range($from; $to; 1);
def range($to): range(0; $to; 1);

def recurse(f): def _f: ., (f | _f); _f;
def recurse(f; cond): recurse(f | select(cond));
def recurse: recurse(.[]?);
def reverse: length as $l | [.[$l-1-range($l)]];

def sort_by(f):
  def _merge($xs; $ys):
    if $xs == [] then $ys[]
    elif $ys == [] then $xs[]
    else
      ( $xs[0] as $x
      | $ys[0] as $y
      | if [$x | f] <= [$y | f] then
          $x, _merge($xs[1:]; $ys)
        else
          $y, _merge($xs; $ys[1:])
        end
      )
    end;
  def _sort:
    if length < 2 then .
    else
      ( (length / 2 | floor) as $l
      | [_merge(.[:$l] | _sort; .[$l:] | _sort)]
      )
    end;
  _sort;
def sort: sort_by(.);

def group_by(f):
  if length == 0 then []
  else
    ( sort_by(f)
    | reduce .[1:][] as $v (
        [[.[0]]];
        if [$v | f] == [.[-1][0] | f] then .[-1] += [$v]
        else . + [[$v]]
        end
      )
    )
  end;
def group: group_by(.);

def unique_by(f): group_by(f) | map(.[0]);
def unique: unique_by(.);

def repeat(f):
  def _f: f, _f;
  _f;

def while(cond; update):
  def _f: if cond then ., (update | _f) else empty end;
  _f;

def until(cond; next):
  def _f: if cond then . else next | _f end;
  _f;

def to_entries:
  ( . as $o
  | keys
  | map({key: ., value: $o[.]})
  );
def from_entries:
  reduce .[] as $kv (
    {};
    .[$kv | .key // .Key // .name // .Name] =
      ($kv | if has(\"value\") then .value else .Value end)
  );
def with_entries(f): to_entries | map(f) | from_entries;

# TODO: rewrite this, seems objects are one level flatten?
def _flatten($depth):
  def _f($d):
    if _is_array and ($depth == -1 or $d <= $depth) then .[] | _f($d+1)
    else .
    end;
  [ if _is_object then .[] | _f($depth)
    else _f(0)
    end
  ];
def flatten($depth):
  if $depth < 0 then error(\"flatten depth must not be negative\")
  else _flatten($depth)
  end;
def flatten: _flatten(-1);

def transpose:
  ( (map(length) | max) as $max
  | . as $v
  | [ range($max) as $i
    | map(.[$i])
    ]
  );

# TODO: should use label/break when supported instead of special sentinel value
def limit($n; f):
  ( \"__jqjq_limit_break\" as $b
  | if $n == 0 then empty
    else
      try
        foreach f as $v (
          0;
          .+1;
          ( $v
          , if . == $n then error($b)
            else empty
            end
          )
        )
      catch
        if . == $b then empty
        else error(.)
        end
    end
  );

def first(f): limit(1; f);
def first: .[0];
def last(f): reduce f as $v (null; [$v]) | arrays[0];
def last: .[-1];
def nth($n; f):
  if $n < 0 then error(\"nth doesn't support negative indices\")
  else
    foreach limit($n+1; f) as $v (
      -1;
      .+1;
      select(. == $n) | $v
    )
  end;
def nth($n): .[$n];
def in(xs): . as $x | xs | has($x);

def isempty(f): [limit(1; f)] == [];

# Assuming the input array is sorted, bsearch/1 returns
# the index of the target if the target is in the input array; and otherwise
# (-1 - ix), where ix is the insertion point that would leave the array sorted.
# If the input is not sorted, bsearch will terminate but with irrelevant results.
def bsearch($target):
  if length == 0 then -1
  elif length == 1 then
    if $target == .[0] then 0 elif $target < .[0] then -1 else -2 end
  else
    ( . as $in
    # State variable: [start, end, answer]
    # where start and end are the upper and lower offsets to use.
    | [0, length - 1, null]
    | until(
        .[0] > .[1];
        if .[2] != null then .[1] = -1  # break
        else
          ( ((.[1] + .[0]) / 2 | floor) as $mid
          | $in[$mid] as $middle
          | if $middle == $target  then .[2] = $mid  # success
            elif .[0] == .[1]      then .[1] = -1    # failure
            elif $middle < $target then .[0] = $mid + 1
            else .[1] = $mid - 1
            end
          )
        end
      )
    | if .[2] == null then  # compute the insertion point
        if $in[.[0]] < $target then -2 - .[0]
        else -1 - .[0]
        end
      else .[2]
      end
    )
  end;

def _strindices($i):
  ( . as $s
  | [range(length) | select($s[.:] | startswith($i))]
  );
def indices($i):
  if _is_array and ($i | _is_array) then .[$i]
  elif _is_array then .[[$i]]
  elif _is_string and ($i | _is_string) then _strindices($i)
  else .[$i]
  end;
def index($i):  indices($i) | .[0];
def rindex($i): indices($i) | .[-1:][0];

def match($val):
  ( ($val | type) as $vt
  | if $vt == \"string\" then match($val; null)
    elif $vt == \"array\" and ($val | length) > 1 then match($val[0]; $val[1])
    elif $vt == \"array\" and ($val | length) > 0 then match($val[0]; null)
    else error($vt + \" not a string or array\")
    end
  );
def test($val):
  ( ($val | type) as $vt
  | if $vt == \"string\" then test($val; null)
    elif $vt == \"array\" and ($val | length) > 1 then test($val[0]; $val[1])
    elif $vt == \"array\" and ($val | length) > 0 then test($val[0]; null)
    else error($vt + \" not a string or array\")
    end
  );
def capture(re; mods):
  ( match(re; mods)
  | reduce (
      ( .captures[]
      | select(.name != null)
      | {(.name): .string}
      )
    ) as $pair (
      {};
      . + $pair
    )
  );
def capture($val):
  ( ($val | type) as $vt
  | if $vt == \"string\" then capture($val; null)
    elif $vt == \"array\" and ($val | length) > 1 then capture($val[0]; $val[1])
    elif $vt == \"array\" and ($val | length) > 0 then capture($val[0]; null)
    else error($vt + \" not a string or array\")
    end
  );

def all(gen; cond): first((gen | select(cond | not) | false), true);
def all(cond): all(.[]; cond);
def all: all(.);

def any(gen; cond): first((gen | select(cond) | true), false);
def any(cond): any(.[]; cond);
def any: any(.);

def del(p): delpaths([path(p)]);

def paths: path(..) | select(. != []);

def @text: tostring;
def @json: tojson;

def _utf8_bytes:
  [ explode[]
  | if . < 128 then
      # 1-byte = 0xxxxxxx
      .
    elif . < 2048 then
      # 2-byte = 110xxxxx 10xxxxxx
      ( ((. / 64) | floor) as $x1
      | (. - ($x1 * 64)) as $x0
      | 192 + $x1
      , 128 + $x0
      )
    elif . < 65536 then
      # 3-byte = 1110xxxx 10xxxxxx 10xxxxxx
      ( ((. / 4096) | floor) as $x2
      | ((. - ($x2 * 4096)) / 64 | floor) as $x1
      | (. - ($x2 * 4096) - ($x1 * 64)) as $x0
      | 224 + $x2
      , 128 + $x1
      , 128 + $x0
      )
    else
      # 4-byte = 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
      ( ((. / 262144) | floor) as $x3
      | ((. - ($x3 * 262144)) / 4096 | floor) as $x2
      | ((. - ($x3 * 262144) - ($x2 * 4096)) / 64 | floor) as $x1
      | (. - ($x3 * 262144) - ($x2 * 4096) - ($x1 * 64)) as $x0
      | 240 + $x3
      , 128 + $x2
      , 128 + $x1
      , 128 + $x0
      )
    end
  ];

def @uri:
  gsub(\"(?<c>[^A-Za-z0-9-_\\\\.~])\";
    ( # A (65) - 10 = 55
      def _hex: . + if . < 10 then 48 else 55 end;
      .c
    | [ _utf8_bytes[]
      | 37 # %
      , (((. / 16) | floor) | _hex)
      , ((. % 16) | _hex)
      ]
    | implode
    )
  );

def _ascii_map($l; $u; f):
  ( explode
  | map(if . >= $l and . <= $u then f end)
  | implode
  );
def ascii_upcase: _ascii_map(97; 122; .-32);
def ascii_downcase: _ascii_map(65; 90; .+32);

";

def _builtins_env:
  try
    ( _builtins_src
    | lex
    | parse
    | .func_defs
    | func_defs_to_env({})
    )
  catch
    error("builtins: \(.)");

def builtin_undefined_func($globals; $builtins_env):
  ( . as $f
  | if $f.name | startswith("$") then
      if $globals | has($f.name) then
        [[null], $globals[$f.name]]
      else
        undefined_func_error
      end
    elif $f.name == "env/0" then
      [[null], $globals["$ENV"]]
    elif $f.name == "input/0" then
      [[null], input]
    elif $f.name == "inputs/0" then
      inputs | [[null], .]
    elif $f.name == "eval/1" then
      # behaves as eval($expr)
      ( $f.input
      | eval_ast(
          $f.args[0];
          [];
          $f.env;
          builtin_undefined_func($globals; $builtins_env)
        ) as [$_path, $expr]
      | $f.input
      | eval_ast(
          $expr | lex | parse;
          [];
          $builtins_env;
          builtin_undefined_func($globals; $builtins_env)
        ) as [$path, $value]
      | [ if $path == [null] then $path
          else $f.path + $path
          end
        , $value
        ]
      )
    else
      undefined_func_error
    end
  );

def eval($expr; $globals; $builtins_env):
  ( eval_ast(
      $expr | lex | parse;
      [];
      $builtins_env;
      builtin_undefined_func($globals; $builtins_env)
    ) as [$path, $value]
  # if we have a path make eval be a valid path expression by using getpath
  # otherwise just return the value
  # TODO: does not work with jq yet because issue with bind patterns
  # $ gojq -cn -L . 'include "jqjq"; {} | {a:1} | eval(".a") += 1'
  # {"a":2}
  | if $path | . == [] or . == [null] then $value
    else _getpath($path)
    end
  );
def eval($expr):
  eval($expr; {}; _builtins_env);

def die: "jqjq: \(.)\n" | halt_error(2);
def TODO: "not implemented: \(.)" | die;

def usage:
  ( "jqjq - jq implementation of jq\n"
  + "Usage: jqjq [OPTIONS] [--] [EXPR]\n"
  + "\n"
  + "Options:\n"
  + "  --jq PATH                 Host jq implementation to run with\n"
  + "  --lex                     Lex EXPR\n"
  + "  --parse                   Lex then parse EXPR\n"
  + "  --repl                    REPL\n"
  + "  --no-builtins             Don't include builtins\n"
  + "\n"
  + "  --null-input / -n         Null input\n"
  + "  --slurp / -s              Slurp inputs into an array\n"
  + "  --compact-output / -c     Output each object on one line\n"
  + "  --raw-output / -r         Output strings raw with newline\n"
  + "  --raw-output0             Output strings raw with NUL\n"
  + "  --join-output             Output strings raw\n"
  + "  --color-output / -C       Force colored output\n"
  + "  --monochrome-output / -M  Disable colored output\n"
  + "  --tab                     Use tabs for indentation\n"
  + "  --indent n                Use n spaces for indentation\n"
  + "  --unbuffered              Use unbuffered output with the host jq\n"
  + "  --from-file / -f          Load filter from a file\n"
  + "  --arg name value          Set $name to the string value\n"
  + "  --argjson name value      Set $name to the JSON value\n"
  + "  --rawfile name file       set $name to string contents of file\n"
  + "  --args                    Consume arguments as positional strings\n"
  + "  --jsonargs                Consume arguments as positional JSON\n"
  + "  --run-tests               Run jq tests from stdin\n"
  + "\n"
  + "Environment:\n"
  + "  $JQ                       Host jq implementation to run with\n"
  );

# parses CLI options just like jq
def parse_options:
  def option($short; $long; on_option):
    ( if .args.is_short then
        if $short != null and (.args.curr | startswith($short)) then
          .args.curr |= (.[$short | length:] | if length == 0 then null end)
        else empty
        end
      else
        if .args.curr == $long then
          .args.curr = null
        else empty
        end
      end
    | on_option
    );
  def handle_library_path:
    if .args.curr != null then
      ( .lib_search_paths += [.args.curr]
      | .args.curr = null
      )
    elif .args.rest | length >= 1 then
      ( .lib_search_paths += [.args.rest[0]]
      | .args.rest = .args.rest[1:]
      )
    else
      "-L takes a parameter: (e.g. -L /search/path or -L/search/path)" | die
    end;
  def handle_indent:
    if .args.rest | length < 1 then
      "--indent takes one parameter" | die
    else
      try
        ( (.args.rest[0] | tonumber) as $indent
        | .args.rest = .args.rest[1:]
        # allow indentation over 7 spaces, unlike jq
        | if $indent | . != trunc or . < -1 then error end
        | .indent = ($indent | if . == -1 then "tab" end)
        | .print_pretty = true
        )
      catch
        ("--indent takes a number or -1 for tab" | die)
    end;
  def handle_arg($option; $value_name):
    if .args.rest | length < 2 then
      "--\($option) takes two parameters (e.g. --\($option) varname \($value_name))" | die
    else
      ( .args.rest[0] as $key | .args.rest[1] as $value
      | .args.rest = .args.rest[2:]
      | if .program_args.named | has($key) | not then
          # jq parses values only of the first occurrence of each key
          .program_args.named[$key] = {type: $option, $value}
        end
      )
    end;
  def handle_jq:
    if .args.rest | length < 1 then
      "--jq takes one parameter" | die
    else
      ( .jq = .args.rest[0]
      | .args.rest = .args.rest[1:]
      )
    end;
  def parse_option:
    (  option("s"; "slurp"; .slurp = true)
    // option("r"; "raw-output"; .raw_output = true)
    // option(null; "raw-output0"; (.raw_output, .raw_no_lf, .raw_output0) = true)
    // option("j"; "join-output"; (.raw_output, .raw_no_lf) = true)
    // option("c"; "compact-output"; .indent = 0 | .print_pretty = false)
    // option("C"; "color-output"; .color_output = true)
    // option("M"; "monochrome-ouput"; .no_color_output = true)
    # // option("a"; "ascii-output"; .ascii_output = true)
    // option(null; "unbuffered"; .unbuffered_output = true)
    # // option("S"; "sort-keys"; .sorted_output = true)
    # // option("R"; "raw-input"; .raw_input = true)
    // option("n"; "null-input"; .null_input = true)
    // option("f"; "from-file"; .from_file = true)
    # // option("L"; "library-path"; handle_library_path)
    // option("b"; "binary"; .binary_input_output = true)
    // option(null; "tab"; .indent = "tab" | .print_pretty = true)
    // option(null; "indent"; handle_indent)
    # // option(null; "seq"; .seq = true)
    # // option(null; "stream"; .parse_streaming = true)
    # // option(null; "stream-errors"; (.parse_streaming, .parse_stream_errors) = true)
    # // option("e"; "exit-status"; .exit_status = true)
    // option(null; "args"; .args.rest_are_positional = "arg")
    // option(null; "jsonargs"; .args.rest_are_positional = "argjson")
    // option(null; "arg"; handle_arg("arg"; "value"))
    // option(null; "argjson"; handle_arg("argjson"; "text"))
    // option(null; "rawfile"; handle_arg("rawfile"; "filename"))
    // option(null; "slurpfile"; handle_arg("slurpfile"; "filename"))
    # // option(null; "debug-dump-disasm"; .debug_dump_disasm = true)
    # // option(null; "debug-trace=all"; .debug_trace_all = true)
    # // option(null; "debug-trace"; .debug_trace = true)
    // option("h"; "help"; .action = "help")
    # // option("V"; "version"; .action = "version")
    # // option(null; "build-configuration"; .action = "build-configuration")
    // option(null; "run-tests"; .action = "run-tests")
    # jqjq extensions:
    // option(null; "jq"; handle_jq)
    // option(null; "repl"; .mode = "repl")
    // option(null; "lex"; .mode = "lex")
    // option(null; "parse"; .mode = "parse")
    // option(null; "no-builtins"; .no_builtins = true)
    //
      ( if .args.is_short then "-\(.args.curr[:1])" else "--\(.args.curr)" end
      | "Unknown option: \(.)" | die
      )
    );
  def parse_options_in_arg:
    ( parse_option
    | if .args.curr != null and .action == null then parse_options_in_arg end
    );
  def parse_arg:
    ( .args.rest[0] as $arg
    | .args.rest = .args.rest[1:]
    | if .args.done or ($arg | test("^-[\\-a-zA-Z]") | not) then
        if .program == null then
          .program = $arg
        elif .args.rest_are_positional != null then
          .program_args.positional += [{type: .args.rest_are_positional, value: $arg}]
        else
          .files += [$arg]
        end
      elif $arg == "--" then
        .args.done = true
      else
        ( if $arg | startswith("--") then
            .args.curr = $arg[2:] | .args.is_short = false
          else
            .args.curr = $arg[1:] | .args.is_short = true
          end
        | parse_options_in_arg
        )
      end
    );
  def parse_args:
    if (.args.rest | length > 0) and .action == null then
      parse_arg | parse_args
    else del(.args)
    end;
  ( { args: { rest: . }
    , program_args:
        { positional: []
        , named: {}
        }
    , indent: 2
    , print_pretty: true
    }
  | parse_args
  );

# constructs a bash command which executes jqjq with the host jq and passes all
# necessary files as named arguments
def construct_jqjq_command:
  # instead of @sh to not always quote (as per quoting rules of ${var@Q})
  def sh_escape:
    if . == "" or test("[^A-Za-z0-9%+\\-./:@_]") then
      "'" + gsub("'"; "'\\''") + "'"
    end;
  ( . as $args
  | parse_options
  | (env.JQ // .jq // "jq") as $jq
  | ($jq | test("(^|[/\\\\])(gojq|jaq)[^/\\\\]*$") | not) as $host_is_jq
  | [ ($jq | sh_escape)
    , if .action == "run-tests" then "-nsRr"
      elif .mode == "repl" then "-njR"
      else "-nj"
      end
      # only jq supports --unbuffered and --binary, not gojq or jaq
    , if .unbuffered_output and $host_is_jq then "--unbuffered" else empty end
    , if .binary_input_output and $host_is_jq then "--binary" else empty end
    , "-L", "\"$(dirname \"$(realpath \"${BASH_SOURCE[0]}\")\")\""
    , "'include \"jqjq\"; jqjq($ARGS.positional; $ENV)'"
    , ( [ if .from_file then .program? else empty end
        , .files[]?
        , (.program_args.named[]? | select(.type | . == "rawfile" or . == "slurpfile").value)
        ]
      | unique[]
      | "--rawfile", "file:\(sh_escape)", sh_escape
      )
    , "--args", "--", ($args[] | sh_escape)
    ]
  | join(" ")
  );

# entrypoint for jqjq wrapper
# what argument jq will run with depends as bit on some arguments, --repl, --run-tests
# etc, see wrapper script
def jqjq($args; $env):
  # get the ANSI color codes for printing values
  # corresponds to jv_set_colors in jq and its usage in main
  def _parse_colors($opts; $env):
    # color order: null, false, true, number, string, array, object, field
    ( ["0;90", "0;39", "0;39", "0;39", "0;32", "1;39", "1;39", "1;34"] as $default
    | if $env | has("JQ_COLORS") then
        # only up to the first 8 color sequences are used
        ( ($env.JQ_COLORS | split(":")[:8]) as $custom
        | if $custom | all(test("^[0-9;]*$")) then
            $custom + $default[$custom | length:]
          else
            "Failed to set $JQ_COLORS\n" | stderr | $default
          end
        )
      else $default
      end
    | if $opts.no_color_output or
          (($opts.color_output | not) and ($env.NO_COLOR | . != null and . != "")) then
        null
      end
    | if . != null then map("\u001b[\(.)m") end
    );

  def _parse_opts($opts; $env):
    def get_file:
      $ARGS.named["file:\(.)"] // ("file \(.) not provided by host" | die);
    ( $opts
    | if .program == null and .mode != "repl" then usage | halt_error(2) end
    | if .from_file then .program |= get_file end
    | .files |= map(get_file)?
    | .program_args.positional |= map(
        ( .type as $type
        | .value
        | if $type == "argjson" then
            try fromjson catch ("invalid JSON text passed to --jsonargs: \(.)\n" | die)
          end
        )
      )
    | .program_args.named |= with_entries(
        ( .key as $key | .value.type as $type
        | .value |= (
            ( .value
            | if $type == "argjson" then
                try fromjson catch ("invalid JSON text passed to --argjson: \(.)\n" | die)
              elif $type == "rawfile" then get_file
              elif $type == "slurpfile" then "--slurpfile" | TODO
              end
            )
          )
        )
      )
    | .colors = _parse_colors($opts; $env)
    | .stream_sep = (
        ( if $opts.raw_no_lf then "" else "\n" end
        | if $opts.raw_output0 then . + "\u0000" end
        )
      )
    );

  def _repl($opts):
    def _repeat_break(f):
      try repeat(f)
      catch
        if . == "break" then empty
        else error
        end;
    ( _parse_opts($opts; $env) as $opts
    | ( if $opts.no_builtins then {}
        else _builtins_env
        end
      ) as $builtins_env
    | _repeat_break(
        ( "> "
        , ( try input
            catch error("break")
          | . as $expr
          | null
          | try
              ( ( eval($expr; {"$ENV": $env}; $builtins_env)
                | dump($opts)
                )
              , if $opts.raw_no_lf then "\n" else empty end
              )
            catch
              "error: \(.)\n"
          )
        )
      )
    , "\n" # input interrupted so no line entered
    );

  def _run_tests:
    # read jq test format:
    # # comment
    # expr
    # input
    # output*
    # <blank>+
    # ...
    # <next test>
    def _from_jqtest:
      [ foreach (split("\n")[], "") as $l (
          { current_line: 0
          , nr: 1
          , emit: true
          };
          ( .current_line += 1
          | if .emit then
              ( .expr = null
              | .input = null
              | .output = []
              | .fail = null
              | .emit = null
              | .error = null
              )
            else .
            end
          | if $l | test("^\\s*#") then .
            elif $l | test("^\\s*$") then
              if .expr then
                ( .emit =
                    { line
                    , nr
                    , expr
                    , input
                    , output
                    , fail
                    , error
                    }
                | .nr += 1
                )
              else .
              end
            elif $l | test("^\\s*%%FAIL") then
              .fail = $l
            else
              if .expr == null then
                ( .line = .current_line
                | .expr = $l
                )
              elif .fail and .error == null then .error = $l
              elif .input == null then .input = $l
              else .output += [$l]
              end
            end
          );
          if .emit then .emit
          else empty
          end
        )
      ];
    def _f:
      # TODO: jaq: [] | join("") -> null
      # TODO: jaq: join works with more than strings
      def _join($s): if . == [] then "" else join($s) end;
      def _to_unslurped: map(tojson) | _join(",");
      ( _builtins_env as $builtins_env
      | _from_jqtest[]
      | . as $c
      | try
          if .error | not then
            ( ( .input
              , .output[]
              ) |= fromjson
            )
          else .
          end
        catch
          ( . as $err
          | $c
          | .fromjson_error = $err
          )
      | select(.fromjson_error | not)
      | "line \(.line): \(.input | tojson) | \(.expr) -> \(.output | _to_unslurped)" as $test_name
      | . as $test
      | try
          ( ($test.expr | lex | parse) as $ast
          | $test.input
          | [ eval_ast(
                $ast;
                [];
                $builtins_env;
                builtin_undefined_func({"$ENV": {"jqjq": 123}}; $builtins_env)
              ) as [$_path, $value]
            | $value
            ] as $actual_output
          | if $test.output == $actual_output then
              ( "OK: \($test_name)"
              , {ok: true}
              )
            else
              ( "DIFF: \($test_name)"
              , "  Expected: \($test.output | _to_unslurped)"
              , "    Actual: \($actual_output | _to_unslurped)"
              , {error: true}
              )
            end
          )
        catch
          if $test.fail then
            if . == $test.error then
              ( "OK: \($test_name)"
              , {ok: true}
              )
            else
              ( "FAIL DIFF: \($test_name)"
              , "  Expected: \($test.error)"
              , "    Actual: \(.)"
              , {error: true}
              )
            end
          else
            ( "ERROR: \($test_name)"
            , "  \(.)"
            , {error: true}
            )
          end
      );
    # this mess make it possible to run all tests and exit with non-zero if any test failed
    ( foreach (_f, {end: true}) as $l (
        { errors: 0
        , oks: 0
        };
        if ($l | type) == "object" then
          ( .line = false
          | if $l.error then .errors += 1
            elif $l.ok then .oks += 1
            elif $l.end then .end = true
            else .
            end
          )
        else . + {line: $l}
        end;
        .
      )
    | if .end then
        ( "\(.oks) of \(.oks + .errors) tests passed"
        , if .errors > 0 then "" | halt_error(1) else empty end
        )
      elif .line then .line
      else empty
      end
    );

  # TODO: raw output
  # TODO: refactor env undefined_func_error code
  def _filter($opts):
    ( def _inputs:
        if $opts.null_input then null
        elif $opts.slurp then [inputs]
        else inputs
        end;
      _parse_opts($opts; $env) as $opts
    | ( $opts.program_args.named | with_entries(.key |= ("$" + .))
      + { "$ENV": $env # $ENV has precedence over args
        , "$ARGS": $opts.program_args
        }
      ) as $globals
    | ( if $opts.no_builtins then {}
        else _builtins_env
        end
      ) as $builtins_env
    | _inputs
    | eval($opts.program; $globals; $builtins_env)
    | dump($opts)
    );

  ( ($args | parse_options) as $opts
  | if $opts.action == "help"        then usage
    elif $opts.mode == "lex"         then $opts.program | lex, "\n"
    elif $opts.mode == "parse"       then $opts.program | lex | parse, "\n"
    elif $opts.mode == "repl"        then _repl($opts)
    elif $opts.action == "run-tests" then input | _run_tests
    else _filter($opts)
    end
  );
