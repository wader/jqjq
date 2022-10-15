# jqjq - jq implementation of jq
# Copyright (c) 2022 Mattias Wadman
# MIT License
#
# TODO:
# jq bug with error undefined function
# ".end" lex
# test assoc 1|2|3?
# add some term builder helper, _term("TermTypeArray"; {query: ...}) etc?
# "a |" parse as "a | .", should be error
#
# Notes:
# AST is more or less identical to the one used by gojq to make it easer to test parser
# jq bindings $<name>_ is used if <name> is a keyword as jq (not gojq) does not allow it
#

def debug(f): . as $c | f | debug | $c;

# TODO: keep track of position?
def lex:
  def _token:
    def _re($re; f):
      ( .remain
      | . as $v
      | match($re; "m").string
      | { result: f
        , remain: $v[length:]
        }
      );
    if .remain == "" then empty
    else
      (  _re("^\\s+"; {whitespace: .})
      // _re("^#[^\n]*"; {comment: .})
      // _re("^\\.[_a-zA-Z][_a-zA-Z0-9]*"; {index: .[1:]})
      // _re("^[_a-zA-Z][_a-zA-Z0-9]*"; {ident: .})
      // _re("^\\$[_a-zA-Z][_a-zA-Z0-9]*"; {binding: .})
      # 1.23, .123, 123e2, 1.23e2, 123E2 or 123
      // _re("^(?:[0-9]*\\.[0-9]+|[0-9]+)(?:[eE][0-9]+)?"; {number: .})
      # match " <any non-"-or-\> or <\ + any> "
      // _re("^\"(?:[^\"\\\\]|\\\\.)*?\"";
          ( .[1:-1]
          | gsub("\\\\(?<c>.)";
              ( . as {$c}
              | { "n": "\n"
                , "r": "\r"
                , "\"": "\""
                , "\\": "\\"
                }[$c]
              | if not then error("unknown escape: \\\($c)") else . end
              )
          )
          | {string: .}
          )
        )
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
      // _re("^/";      {slash: .})
      // _re("^%";      {percent: .})
      // _re("^\\(";    {lparen: .})
      // _re("^\\)";    {rparen: .})
      // _re("^\\[";    {lsquare: .})
      // _re("^\\]";    {rsquare: .})
      // _re("^{";      {lcurly: .})
      // _re("^}";      {rcurly: .})
      // _re("^\\.\\."; {dotdot: .})
      // _re("^\\.";    {dot: .})
      // _re("^\\?";    {qmark: .})
      // error("unknown token: \(.remain)")
      )
    end;
  def _lex:
    ( {remain: ., result: {whitespace: ""}}
    | recurse(_token)
    | .result
    | select((.whitespace // .comment) | not)
    );
  [_lex];

def parse:
  def _consume(f): select(.[0] | f) | .[1:];
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

  def _p($type):
    # based on:
    # https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#climbing
    # filter is used to disable operators, ex in keyval query
    def _op_prec_climb($p; filter):
      def _ops:
        if filter then false
        elif .pipe then           {prec: 0, name: "|",   assoc: "right"}
        # TODO: understand why jq has left assoc for "," but right seems to give correct parse tree
        elif .comma then          {prec: 1, name: ",",   assoc: "right"}
        elif .equal then          {prec: 2, name: "=",   assoc: "none"}
        elif .pipe_equal then     {prec: 2, name: "|=",  assoc: "none"}
        elif .equal_plus then     {prec: 2, name: "+=",  assoc: "none"}
        elif .equal_dash then     {prec: 2, name: "-=",  assoc: "none"}
        elif .equal_star then     {prec: 2, name: "*=",  assoc: "none"}
        elif .equal_slash then    {prec: 2, name: "/=",  assoc: "none"}
        elif .equal_percent then  {prec: 2, name: "%=",  assoc: "none"}
        elif .ident == "or" then  {prec: 3, name: "or",  assoc: "left"}
        elif .ident == "and" then {prec: 4, name: "and", assoc: "left"}
        elif .equal_equal then    {prec: 5, name: "==",  assoc: "none"}
        elif .not_equal then      {prec: 5, name: "!=",  assoc: "none"}
        elif .less then           {prec: 5, name: "<",   assoc: "none"}
        elif .less_equal then     {prec: 5, name: "<=",  assoc: "none"}
        elif .greater then        {prec: 5, name: ">",   assoc: "none"}
        elif .greater_equal then  {prec: 5, name: ">=",  assoc: "none"}
        elif .plus then           {prec: 6, name: "+",   assoc: "left"}
        elif .dash then           {prec: 6, name: "-",   assoc: "left"}
        elif .star then           {prec: 7, name: "*",   assoc: "left"}
        elif .slash then          {prec: 7, name: "/",   assoc: "left"}
        elif .percent then        {prec: 7, name: "%",   assoc: "left"}
        else false
        end;

      ( _p("query1") as [$rest, $t]
      | $rest
      | def _f($t):
          ( .[0] as $next # peek next
          | ($next | _ops) as $next_op
          | if $next_op and $next_op.prec >= $p then
              ( .[1:] # consume
              | ( if $next_op.assoc == "right" then
                    _op_prec_climb($next_op.prec; filter)
                  elif $next_op.assoc == "left" then
                    _op_prec_climb($next_op.prec+1; filter)
                  else
                    # TODO: none assoc, 1 == 2 == 3 etc
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

    # {<keyval>...} where keyval is:
    # name
    # "name"
    # $name
    # name: <term>
    # "name": <term>
    # <subquery>: <term>
    # TODO: val should be query but don't allow comma
    def _object:
      ( _consume(.lcurly)
      | _repeat(
          # TODO:
          # string interpolated key
          #   {"\(...)"} -> {"\(...)"": .["\(...)"]}
          #   {"\(...)": ...} -> {"\(...)"": ...}
          # multi query val:
          #    term | ...
          ( ( def _colon_val:
                ( _consume(.colon)
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
                ( .[0] as $ident
                | _consume(.ident)
                | _optional(_colon_val) as [$rest, $val]
                | $rest
                | [ .
                  , { key: $ident.ident
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
                        { str: $string.term.str
                        }
                    , val: $val
                    }
                  ]
                )
              //
                # {$a} -> {a: $a}
                ( .[0] as $binding
                | _consume(.binding)
                | [ .
                  , { key: $binding.binding
                    }
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
          | _optional(
              # TODO: _one() etc?
              ( _consume(.comma)
              | [., null]
              )
            ) as [$rest, $_]
          | [$rest, $key_vals]
          )
        ) as [$rest, $key_vals]
      | $rest
      | _consume(.rcurly)
      | [ .
        , { term:
              { type: "TermTypeObject"
              , object:
                  { key_vals: $key_vals
                  }
              }
          }
        ]
      );

    # (<query>)
    def _subquery:
      ( _consume(.lparen)
      | _p("query") as [$rest, $query]
      | $rest
      | _consume(.rparen)
      | [ .
        , { term:
              { type: "TermTypeQuery",
                query: $query
              }
          }
        ]
      );

    # ident
    # ident(<query>[;...])
    def _func:
      ( . as [$first]
      | _consume(.ident)
      | ( _consume(.lparen)
        | _repeat(
            ( _p("query") as [$rest, $arg]
            | $rest
            | _optional(
                # TODO: _one() etc?
                ( _consume(.semicolon)
                | [., null]
                )
              ) as [$rest, $_]
            | [$rest, $arg]
            )
          ) as [$rest, $args]
        | $rest
        | _consume(.rparen)
        | [ .
          , { term:
                { type: "TermTypeFunc"
                , func:
                    { name: $first.ident
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
                    { name: $first.ident
                    }
                }
            }
          ]
      );

    # $name
    def _binding:
      ( . as [$first]
      | _consume(.binding)
      # TODO: args
      | [ .
        , { term:
              { type: "TermTypeFunc"
              , func:
                  { name: $first.binding
                  }
              }
          }
        ]
      );

    # [<query>]
    def _array:
      ( _consume(.lsquare)
      | _optional(_p("query")) as [$rest, $query]
      | $rest
      | _consume(.rsquare)
      | [ .
        , { term:
              { type: "TermTypeArray",
                array:
                  { query: $query
                  }
              }
          }
        ]
      );

    # reduce <term> as <binding> (<start-query>;<update-query>)
    def _reduce:
      ( _consume(.ident == "reduce")
      | _p("term") as [$rest, $term]
      | $rest
      | _consume(.ident == "as")
      | .[0] as $binding # TODO: pattern
      | _consume(.binding)
      | _consume(.lparen)
      | _p("query") as [$rest, $start]
      | $rest
      | _consume(.semicolon)
      | _p("query") as [$rest, $update]
      | $rest
      | _consume(.rparen)
      | [ .
        , { term:
            { type: "TermTypeReduce"
            , reduce:
              { term: $term.term
              , pattern: {name: $binding.binding}
              , start: $start
              , update: $update
              }
            }
          }
        ]
      );

    # foreach <term> as <binding> (<start-query>;<update-query>[;<extract-query>])
    def _foreach:
      ( _consume(.ident == "foreach")
      | _p("term") as [$rest, $term]
      | $rest
      | _consume(.ident == "as")
      | .[0] as $binding # TODO: pattern
      | _consume(.binding)
      | _consume(.lparen)
      | _p("query") as [$rest, $start]
      | $rest
      | _consume(.semicolon)
      | _p("query") as [$rest, $update]
      | $rest
      | _optional(
          ( _consume(.semicolon)
          | _p("query")
          )
        ) as [$rest, $extract]
      | $rest
      | _consume(.rparen)
      | [ .
        , { term:
            { type: "TermTypeForeach"
            , foreach:
                ( { term: $term.term
                  , pattern: {name: $binding.binding}
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
      ( _consume(.ident == "if")
      | _p("query") as [$rest, $cond]
      | $rest
      | _consume(.ident == "then")
      | _p("query") as [$rest, $then_]
      | $rest
      | _repeat(
          ( _consume(.ident == "elif")
          | _p("query") as [$rest, $cond]
          | $rest
          | _consume(.ident == "then")
          | _p("query") as [$rest, $then_]
          | $rest
          | [., {cond: $cond, then: $then_}]
          )
        ) as [$rest, $elif_]
      | $rest
      | _optional(
          ( _consume(.ident == "else")
          | _p("query")
          )
        ) as [$rest, $else_]
      | $rest
      | _consume(.ident == "end")
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
    # def a(f) ...;
    # def a(f; $v) ...;
    def _func_defs:
      _repeat(
        ( _consume(.ident == "def")
        | . as [{ident: $name}]
        | _consume(.ident)
        | ( ( _consume(.lparen)
            | _repeat(
                ( .[0] as $arg
                | ( ( _consume(.ident)
                    | [., $arg.ident]
                    )
                  //
                    ( _consume(.binding)
                    | [., $arg.binding]
                    )
                  ) as [$rest, $arg]
                | $rest
                | ( _consume(.semicolon)
                  // .
                  )
                | [., $arg]
                )
              ) as [$rest, $args]
            | $rest
            | _consume(.rparen)
            | _consume(.colon)
            | _p("query") as [$rest, $body]
            | $rest
            | _consume(.semicolon)
            | [ .
              , { name: $name
                , args: $args
                , body: $body
                }
              ]
            )
          //
            ( _consume(.colon)
            | _p("query") as [$rest, $body]
            | $rest
            | _consume(.semicolon)
            | [ .
              , { name: $name
                , body: $body
                }
              ]
            )
          )
        )
      );

    # []
    # [<query>]
    # .name
    # ."name"
    # ?
    # as $v | <query>
    # TODO: [start:stop]
    def _suffix:
      (
        # [] iter
        ( _consume(.lsquare)
        | _consume(.rsquare)
        | [., {iter: true}]
        )
      //
        # [...] query
        ( _consume(.lsquare)
        | _p("query") as [$rest, $start]
        | $rest
        | _consume(.rsquare)
        | [ .
          , { index:
                {start: $start}
            }
          ]
        )
      //
        # [...:...]
        # [:...]
        # [...:]
        ( _consume(.lsquare)
        | _optional(_p("query")) as [$rest, $start]
        | $rest
        | _consume(.colon)
        | _optional(_p("query")) as [$rest, $end_]
        | $rest
        | _consume(.rsquare)
        # fail if both missing
        | if $start == null and $end_ == null then empty else . end
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
        ( .[0] as $index
        | _consume(.index)
        | [ .
          , { index:
                { name: $index.index
                }
            }
          ]
        )
      //
        # ."name" index
        # TODO: ."string" for TermTypeIndex?
        ( _consume(.dot)
        | _p("string") as [$rest, $string]
        | $rest
        | [ .
          , { index:
                { str:
                    { str: $string.str
                    }
                }
            }
          ]
        )
      //
        # ? optional (try)
        ( _consume(.qmark)
        | [ .
          , {optional: true}
          ]
        )
      //
        ( _consume(.ident == "as")
        | .[0] as $binding
        | _consume(.binding)
        | _consume(.pipe)
        | _p("query") as [$rest, $body]
        | $rest
        | [ .
          , { bind:
                { body: $body
                , patterns:
                    [ { name: $binding.binding
                      }
                    ]
                }
              }
          ]
        )
      );

    # .
    def _identity:
      ( _consume(.dot)
      | [ .
        , { term:
              { type: "TermTypeIdentity"
              }
          }
        ]
      );

    # .[<query>]
    # .name
    def _index:
      ( ( _consume(.dot)
        | _consume(.lsquare)
        | _p("query") as [$rest, $query]
        | $rest
        | _consume(.rsquare)
        | [ .
          , { term:
                { type: "TermTypeIndex"
                , index:
                  { start: $query
                  }
              }
            }
          ]
        )
      //
        ( _consume(.dot)
        | _consume(.lsquare)
        | _optional(_p("query")) as [$rest, $start]
        | $rest
        | _consume(.colon)
        | _optional(_p("query")) as [$rest, $end_]
        # TODO: share with _suffix?
        | $rest
        | _consume(.rsquare)
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
        ( .[0] as $index
        | _consume(.index)
        | [ .
          , { term:
                { type: "TermTypeIndex"
                , index:
                  { name: $index.index
                  }
                }
            }
          ]
        )
      );

    # try <query>
    # try <query> catch <query>
    # TODO: query should not support |?
    def _try:
      ( _consume(.ident == "try")
      | _p("query") as [$rest, $body]
      | $rest
      | _optional(
          ( _consume(.ident == "catch")
          | _p("query")
          )
        ) as [$rest, $catch_]
      | $rest
      | [ .
        , { term:
            { type: "TermTypeTry"
            , try:
                ( { body: $body
                  }
                | if $catch_ then .catch = $catch_ else . end
                )
            }
          }
        ]
      );

    # +<term> etc
    def _unary_op(f; $op):
      ( _consume(f)
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
      ( _consume(.dotdot)
      | [ .
        , { term:
              { type: "TermTypeFunc"
              , func:
                 { name: "recurse"
                 }
              }
          }
        ]
      );

    ( .# debug({_p: $type})
    | if $type == "query" then
        # query1, used by _op_prec_climb, exist to fix infinite recursion
        _op_prec_climb(0; false)
      elif $type == "keyval_query" then
        # keyval query only allows | operator
        _op_prec_climb(0; .pipe | not)
      elif $type == "query1" then
        ( _p("func_defs") as [$rest, $func_defs]
        | $rest
        | ( if length == 0 then
              [., {term: {type: "TermTypeIdentity"}}]
            else
              _p("term")
            end
          ) as [$rest, $query]
        | $query
        | if ($func_defs | length) > 0 then
            .func_defs = $func_defs
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
              end
            )
          ]
        )
      elif $type == "suffix" then _suffix
      elif $type == "if" then _if
      elif $type == "func_defs" then _func_defs
      elif $type == "true" then _consume(.ident == "true") | [., {term: {type: "TermTypeTrue"}}]
      elif $type == "false" then _consume(.ident == "false") | [., {term: {type: "TermTypeFalse"}}]
      elif $type == "null" then _consume(.ident == "null") | [., {term: {type: "TermTypeNull"}}]
      elif $type == "number" then
        ( . as [$first]
        | _consume(.number) | [., {term: {type: "TermTypeNumber", number: $first.number}}]
        )
      elif $type == "string" then
        ( . as [$first]
        | _consume(.string) | [., {term: {type: "TermTypeString", str: $first.string}}]
        )
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
      else error("unknown type \($type)")
      end
    );
  ( ( _p("query")
    | if .[0] != [] then error("tokens left: \(.)") end
    | .[1]
    )
  // error("parse error: \(.)")
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
  def _e($query; $path; $env):
    ( .
    #| debug({c: ., $query, $path, $env})
    | $query as {term: {type: $type}, $op, $func_defs}
    | ( ( ($func_defs // [])
        | func_defs_to_env($env)
        )
      ) as $query_env
    | # .
      def _identity:
        [$path, .];

      # eval a index, is also used by _e_suffix
      def _e_index($index; $query_path; $query_input):
        ( . as $input
        | $index as
            { $name
            , $is_slice
            , $start
            , end: $end_
            }
        | if $name then [($query_path + [$name]), $input[$name]]
          elif $is_slice then
            ( $query_input
            | ( if $start then _e($start; []; $query_env)[1]
                else 0 end
              )  as $vs
            | ( if $end_ then _e($end_; []; $query_env)[1]
                else $input | length
                end
              ) as $ve
            | [[null], $input[$vs:$ve]]
            )
          elif $start then
            ( $query_input
            | _e($start; []; $query_env) as [$_, $v]
            | [($query_path + [$v]), $input[$v]]
            )
          else . # TODO: error?
          end
        );

      # .name
      def _index:
        _e_index($query.term.index; $path; .);

      def _func:
        ( $query.term.func as {$name, $args}
        | func_name($name; $args) as $name
        | if $name == "empty/0" then empty
          elif $name == "debug/0" then debug as $_ | [$path, .]
          elif $name == "type/0" then [[null], type]
          elif $name == "length/0" then [[null], length]
          elif $name == "keys/0" then [[null], keys]
          elif $name == "explode/0" then [[null], explode]
          elif $name == "implode/0" then [[null], implode]
          # TODO: implement in jqjq?
          elif $name == "tostring/0" then [[null], tostring]
          elif $name == "tojson/0" then [[null], tojson]
          elif $name == "fromjson/0" then [[null], fromjson]
          # TODO: make args general
          # TODO: path
          elif $name == "error/1" then
            # TODO: see comment in _try
            ( _e($args[0]; $path; $query_env)[1] as $a0
            | error($a0)
            )
          elif $name == "getpath/1" then
            ( _e($args[0]; $path; $query_env)[1] as $a0
            | [ $path+$a0
              , getpath($a0)
              ]
            )
          elif $name == "setpath/2" then
            ( _e($args[0]; $path; $query_env)[1] as $a0
            | _e($args[1]; $path; $query_env)[1] as $a1
            | [ []
              , setpath($a0; $a1)
              ]
            )
          elif $name == "path/1" then
            ( _e($args[0]; []; $query_env) as [$p, $_]
            # TODO: try/catch error
            | if $p == [null] then error("invalid path expression") end
            | [[null], $p]
            )
          else
            ( . as $input
            | $query_env[$name] as $e
            | if $e | has("value") then [[null], $e.value]
              elif $e.body then
                ( ($e.args // []) as $func_args
                | ($args // []) as $call_args
                | ( $func_args
                  | with_entries(
                      ( ( .value
                        # when using a $<name> binding arg <name> is also available as a lambda
                        | if startswith("$") then .[1:] end
                        ) as $name
                      | { key: "\($name)/0"
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
        | $query.term.object as {$key_vals}
        | $key_vals
        | map(
            ( def _term_str:
                { term:
                    { type: "TermTypeString",
                      str: .
                    }
                };
              . as $kv
            | ( if $kv.key then
                  [ ( $kv.key
                    | if startswith("$") then .[1:] end
                    | _term_str
                    )
                  , ( $kv.val.queries[0]
                    //
                      ( $kv.key
                      | if startswith("$") then
                          { term:
                              { type: "TermTypeFunc",
                                func:
                                  { name: .
                                  }
                              }
                          }
                        else
                          { term:
                              { type: "TermTypeIndex",
                                index:
                                  { name: .
                                  }
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
                    , ( $kv.val.queries[0]
                      //
                        { term:
                            { type: "TermTypeIndex",
                              index:
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
                else error("unknown object key")
                end
              )
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
        | [[], .]
        );

      def _array:
        ( [ []
          # .query only set if there was a query
          , [ _e($query.term.array.query // empty; []; $query_env) as [$_, $v]
            | $v
            ]
          ]
        );

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
            # [elif <cond> then <cond>]*
            , ( $if_.elif[]?
              | [.cond, .then]
              )
            , if $if_.else then
                # else <then> end
                # set <cond> to true
                [ {term: {type: "TermTypeTrue"}}
                , $if_.else
                ]
              else
                # end
                # set <cond> to true
                # set <then> to .
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
            ( . as [$p, $v]
            | $v
            | _e($extract; $p; $query_env + {($name): {value: $v}})
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
            _e($catch_; $path; $query_env)
        );

      def _unary:
        ( $query.term.unary as {$op, $term}
        | def _f: _e({term: $term}; $path; $query_env);
          # TODO: not +. as jq don't support + unary operator
          if $op == "+" then _f[1] | [[null], .]
          elif $op == "-" then _f[1] | [[null], -.]
          else error("unsupported unary op: \($query)")
          end
        );

      # TODO: optional
      # TODO: [["a","b"],["c","d"]][0,1][0,1] -> "a","c","b","d"
      # TODO: each suffix gets the term input as input
      # TODO: rewrite this
      def _e_suffix($suffix; $path; $input):
        ( . as [$p, $v]
        | $input
        | if $suffix.bind then
            # as $name | <body>
            ( $suffix.bind as {$body, patterns: [{$name}]} # TODO: patterns array)
            | _e(
                $suffix.bind.body;
                $path;
                $query_env + {($name): {value: $v}}
              )
            )
          elif $suffix.index then
            # .<index>
            ( $v
            | _e_index($suffix.index; $p; $input)
            )
          elif $suffix.iter then
            # .[]
            ( $v
            | keys[] as $key
            | [($p + [$key]), $v[$key]]
            )
          else error("unknown suffix: \($suffix)")
          end
        );

      def _e_suffix_list($input; $path):
        ( $query as {term: {$suffix_list}}
        | def _f($suffix_list):
            if ($suffix_list | length) == 0 then .
            else
              ( _e_suffix($suffix_list[0]; $path; $input)
              | _f($suffix_list[1:])
              )
            end;
          _f($suffix_list)
        );

      if $type then
        ( . as $input
        | if $type == "TermTypeNull" then [[], null]
          elif $type == "TermTypeNumber" then [[null], ($query.term.number | tonumber)]
          elif $type == "TermTypeString" then [[null], $query.term.str]
          elif $type == "TermTypeTrue" then [[null], true]
          elif $type == "TermTypeFalse" then [[null], false]
          elif $type == "TermTypeIdentity" then _identity
          elif $type == "TermTypeIndex" then _index
          elif $type == "TermTypeFunc" then _func
          elif $type == "TermTypeObject" then _object
          elif $type == "TermTypeArray" then _array
          elif $type == "TermTypeIf" then _if
          elif $type == "TermTypeReduce" then _reduce
          elif $type == "TermTypeForeach" then _foreach
          elif $type == "TermTypeQuery" then _e($query.term.query; $path; $query_env)
          elif $type == "TermTypeTry" then _try
          elif $type == "TermTypeUnary" then _unary
          else error("unsupported term: \($query)")
          end
        | if $query.term.suffix_list then
            ( .
            | _e_suffix_list($input; $path)
            )
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
          elif $op == "or" then _l[1] or _r[1] | [[null], .]
          elif $op == "and" then _l[1] and _r[1] | [[null], .]
          elif $op == "==" then _l[1] == _r[1] | [[null], .]
          elif $op == "!=" then _l[1] != _r[1] | [[null], .]
          elif $op == "<" then _l[1] < _r[1] | [[null], .]
          elif $op == "<=" then _l[1] <= _r[1] | [[null], .]
          elif $op == ">" then _l[1] > _r[1] | [[null], .]
          elif $op == ">=" then _l[1] >= _r[1] | [[null], .]
          elif $op == "+" then _l[1] + _r[1] | [[null], .]
          elif $op == "-" then _l[1] - _r[1] | [[null], .]
          elif $op == "*" then _l[1] * _r[1] | [[null], .]
          elif $op == "/" then _l[1] / _r[1] | [[null], .]
          elif $op == "%" then _l[1] % _r[1] | [[null], .]
          elif $op |
              . == "=" or
              . == "|=" or
              . == "+=" or
              . == "-=" or
              . == "*=" or
              . == "/=" or
              . == "%=" then
            # transform <lhr> <op> <rhs> to _assign/_update(lhr; "<op>"; rhs)
            _e(
              { term:
                  { type: "TermTypeFunc",
                    func:
                      { name: (if $op == "=" then "_assign" else "_update" end),
                        args:
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
          else error("unsupported op: \($query)")
          end
        )
      else error("unsupported query: \($query)")
      end
    );
  ( _e($query; []; $env) as [$_, $v]
  | $v
  );
def eval_ast($ast):
  eval_ast($ast; []; {}; undefined_func_error);

def _builtins_src: "
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

def scalars: select(_is_scalar);

def add: reduce .[] as $v (null; . + $v);

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
    else ., (.+$by | _f(stop))
    end;
  if $by == 0 then empty
  elif $by > 0 then $from | _f(. >= $to)
  else $from | _f(. <= $to)
  end;
def range($from; $to): range($from; $to; 1);
def range($to): range(0; $to; 1);

def recurse(f): def _f: ., (f | _f); _f;
# TODO: .[]?
def recurse: recurse(try .[] catch empty);
def reverse: length as $l | [.[$l-1-range($l)]];

def _quicksort(f):
  def _partition($pivot):
    def _f($smaller; $larger):
      if length == 0 then [$smaller, $larger]
      else
        ( .[0] as $head
        | .[1:] # tail
        | if [$head, $pivot] | f then _f([$head] + $smaller; $larger)
          else _f($smaller; [$head] + $larger)
          end
        )
      end;
    _f([]; []);
  if length < 2 then .
  else
    ( .[0] as $pivot
    | .[1:] # tail
    | _partition($pivot)
    | .[0] as $smaller
    | .[1] as $larger
    | ( ($smaller | _quicksort(f))
      + [$pivot]
      + ($larger | _quicksort(f))
      )
    )
  end;
# [f] to support multiple outputs
def sort_by(f): _quicksort((.[0] | [f]) < (.[1] | [f]));
def sort: sort_by(.);

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
    .[$kv.key] = $kv.value
  );
def with_entries(f): to_entries | map(f) | from_entries;

# TODO: rewrite this, objects are one level flatten?
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
";

def builtins_env:
  try
    ( _builtins_src
    | lex
    | parse
    | .func_defs
    | func_defs_to_env({})
    )
  catch
    error("builtins: \(.)");

def eval($expr; $globals):
  eval_ast(
    $expr | lex | parse;
    [];
    builtins_env;
    ( . as $f
    | if $f.name | startswith("$") then
        if $globals | has($f.name) then
          [[null], $globals[$f.name]]
        else
          undefined_func_error
        end
      else
        undefined_func_error
      end
    )
  );
def eval($expr):
  eval($expr; {});

# read jq test format:
# # comment
# expr
# input
# output*
# <blank>+
# ...
# <next test>
def fromjqtest:
  [ foreach (splits("\n"), "") as $l (
      { current_line: 0
      , nr: 1
      , emit: true
      };
      ( .current_line += 1
      | if .emit then
          ( .expr = null
          | .input = null
          | .output = []
          | .emit = null
          )
        end
      | if $l | test("^\\s*#") then .
        elif $l | test("^\\s*$") then
          if .input then
            ( .emit =
                { line
                , nr
                , expr
                , input
                , output
                }
            | .nr += 1
            )
          end
        else
          if .expr == null then
            ( .line = .current_line
            | .expr = $l
            )
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

# entrypoint for jqjq wrapper
# what argument jq will run with depends as bit on some arguments, --repl, --run-tests
# etc, see wrapper script
def jqjq($args; $env):
  def _parse_args:
    def _f:
      if length == 0 then empty
      elif .[0] == "-h" or .[0] == "--help" then
        {help: true}, (.[1:] | _f)
      elif .[0] == "--jq" then
        {jq: .[1]}, (.[2:] | _f)
      elif .[0] == "--lex" then
        {lex: true}, (.[1:] | _f)
      elif .[0] == "--no-builtins" then
        {no_builtins: true}, (.[1:] | _f)
      elif .[0] == "-n" or .[0] == "--null-input" then
        {null_input: true}, (.[1:] | _f)
      elif .[0] == "--parse" then
        {parse: true}, (.[1:] | _f)
      elif .[0] == "--repl" then
        {repl: true}, (.[1:] | _f)
      elif .[0] == "--run-tests" then
        {run_tests: true}, (.[1:] | _f)
      elif .[0] == "--" then
        {filter: .[1]}, (.[2:] | _f)
      elif .[0] | startswith("-") then
        error("unknown argument: \(.[0])")
      else
        {filter: .[0]}, (.[2:] | _f)
      end;
    ( [_f]
    | add
    );

  def _help:
    ( "jqjq - jq implementation of jq"
    , "Usage: jqjq [OPTIONS] [--] [EXPR]"
    , "  --jq PATH        jq implementation to run with"
    , "  --lex            Lex EXPR"
    , "  --no-builtins    Don't include builtins"
    , "  --null-input,-n  Null input"
    , "  --parse          Lex then parse EXPR"
    , "  --repl           REPL"
    , "  --run-tests      Run jq tests from stdin"
    );

  def _repl:
    def _repeat_break(f):
      try repeat(f)
      catch
        if . == "break" then empty
        else error
        end;
    ( _repeat_break(
        ( "> "
        , ( try input
            catch error("break")
          | . as $expr
          | null
          | try
              ( eval($expr)
              | tojson
              , "\n"
              )
            catch
              "error: \(.)\n"
          )
        )
      )
    , "\n" # input interrupted so no line entered
    );

  def _run_tests:
    def _f:
      ( builtins_env as $builtins_env
      | fromjqtest[]
      | . as $c
      | try
          ( ( .input
            , .output[]
            ) |= fromjson
          )
        catch
          ( . as $err
          | $c
          | .error = $err
          )
      | select(.error | not)
      | "\(.nr) (line \(.line)): [\(.input | tojson) | \(.expr)] -> \(.output | tojson)" as $test_name
      | . as $test
      | try
          ( ($test.expr | lex | parse) as $ast
          | $test.input
          | [ eval_ast(
                $ast;
                [];
                $builtins_env;
                undefined_func_error
              )
            ] as $actual_output
          | if $test.output == $actual_output then
              ( "OK: \($test_name)"
              , {ok: true}
              )
            else
              ( "DIFF: \($test_name)"
              , "  Expected: \($test.output | tojson)"
              , "    Actual: \($actual_output | tojson)"
              , {error: true}
              )
            end
          )
        catch
          ( "ERROR: \($test_name)"
          , "  \(.)"
          , {error: true}
          )
      );
    # this mess make it possible to run all tests and exit with non-zero if any test failed
    ( foreach (_f, {end: true}) as $l (
        { errors: 0
        , oks: 0
        };
        if ($l | type) == "object" then
          ( .line = false
          | if $l.error then .errors +=1
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
        , if .errors > 0 then null | halt_error(1) else empty end
        )
      elif .line then .line
      else empty
      end
    );

  # TODO: slurp
  # TODO: raw output
  # TODO: refactor env undefined_func_error code
  # TODO: indented json output?
  def _filter($filter; $null_input; $no_builtins):
    ( ($filter| lex | parse) as $ast
    | ( if $no_builtins then {}
        else builtins_env
        end
      ) as $builtins_env
    | ( if $null_input then null
        else inputs
        end
      )
    | { "$ENV": $env
      } as $globals
    | eval_ast(
        $ast;
        [];
        $builtins_env;
        ( . as $f
        | if $f.name | startswith("$") then
            if $globals | has($f.name) then
              [[null], $globals[$f.name]]
            else
              undefined_func_error
            end
          elif $f.name == "input/0" then
            [[null], input]
          elif $f.name == "inputs/0" then
            inputs | [[null], .]
          elif $f.name == "eval/1" then
            # behaves as eval($expr)
            ( eval_ast(
                $f.args[0];
                [];
                $builtins_env;
                undefined_func_error
              ) as $expr
            | $f.input
            | eval_ast(
                $expr | lex | parse;
                [];
                $builtins_env;
                undefined_func_error
              )
            | [[null], .]
            )
          else
            undefined_func_error
          end
        )
      )
    | tojson
    );

  ( ( $args
    | { filter: "."
      , null_input: false
      , no_builtins: false
      }
      + _parse_args
    ) as $p
  | if $p.help then _help
    elif $p.lex then $p.filter | lex
    elif $p.parse then $p.filter | lex | parse
    elif $p.repl then _repl
    elif $p.run_tests then input | _run_tests
    else
      _filter(
        $p.filter;
        $p.null_input;
        $p.no_builtins
      )
    end
  );
