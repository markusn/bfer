-module(erlbf).

-export([ compile/1 ]).

-type token()     :: statement() | 'begin_loop' | 'end_loop'.
-type statement() :: 'left'|'right'|'add'|'subtract'|'put'|'get'.
-type ast()       :: [statement() | {loop,ast()}].

%%=============================================================================
%% API functions
-spec compile(string()) -> binary().
compile(Code) -> do_compile(parse(lex(Code))).

%%=============================================================================
%% Internal functions
-spec do_compile(ast()) -> binary().
do_compile(_Tree) -> ok.

-spec lex(string()) -> [token()].
lex([])         -> [];
lex("<" ++ Str) -> ['left'       | lex(Str)];
lex(">" ++ Str) -> ['right'      | lex(Str)];
lex("+" ++ Str) -> ['add'        | lex(Str)];
lex("-" ++ Str) -> ['subtract'   | lex(Str)];
lex("." ++ Str) -> ['put'        | lex(Str)];
lex("," ++ Str) -> ['get'        | lex(Str)];
lex("[" ++ Str) -> ['begin_loop' | lex(Str)];
lex("]" ++ Str) -> ['end_loop'   | lex(Str)];
lex([_A|Str])   -> lex(Str).

%%-----------------------------------------------------------------------------
%% Parsing functions
-spec parse([token()]) -> ast().
parse(Tokens) ->
  %% At this point all tokens should have been consumed, otherwise something
  %% is seriously broken
  {[], Ast} = code(Tokens, []),
  Ast.

%% @hidden code ::= statement code_prim
%%                | 'begin_loop' code 'end_loop' code_prim
%% @end
-spec code([token()], ast()) -> {[token()], ast()}.
code(['end_loop'|_], _Ast)        -> throw(parse_error);
code(['begin_loop'|Tokens0], Ast) ->
  %% code ::= 'begin_loop' code 'end_loop' code_prim
  {['end_loop' | Tokens], LoopAst} = code(Tokens0, []),
  code_prim(Tokens, Ast++[{loop,LoopAst}]);
code([_|_] = Tokens, Ast0)        -> %% hd(Tokens) is a statement
  %% code ::= statement code_prim
  {Rest, Ast} = statement(Tokens, Ast0),
  code_prim(Rest, Ast).

%% @hidden code_prim ::= nil | code
-spec code_prim([token()], ast()) -> {[token()], ast()}.
code_prim([], Ast)                        -> {[], Ast}; %% code_prim ::= nil
code_prim(['end_loop' | _] = Tokens, Ast) -> {Tokens, Ast}; %% look ahead!
code_prim(Tokens, Ast)                    -> code(Tokens, Ast).

%% @hidden statement ::= left | right | add | substract | put | get
-spec statement([token()], ast()) -> {[token()], ast()}.
statement([Token|Tokens], Ast) -> {Tokens, Ast ++ [Token]}.

%%=============================================================================
%% Test cases
-include_lib("eunit/include/eunit.hrl").

-define(HELLOWORLD,
        "+++++ +++++             initialize counter (cell #0) to 10"
        "[                       use loop to set the next four cells to"
        "70/100/30/10"
        "> +++++ ++              add  7 to cell #1"
        "> +++++ +++++           add 10 to cell #2"
        "> +++                   add  3 to cell #3"
        "> +                     add  1 to cell #4"
        "<<<< -                  decrement counter (cell #0)"
        "]                   "
        "> ++ .                  print 'H'"
        "> + .                   print 'e'"
        "+++++ ++ .              print 'l'"
        ".                       print 'l'"
        "+++ .                   print 'o'"
        "> ++ .                  print ' '"
        "<< +++++ +++++ +++++ .  print 'W'"
        "> .                     print 'o'"
        "+++ .                   print 'r'"
        "----- - .               print 'l'"
        "----- --- .             print 'd'"
        "> + .                   print '!'"
        "> .                     print '\n'").

lex_test() ->
  ?assertEqual([add,add,add,subtract,begin_loop,add,put,end_loop],
               lex("++ +foo-[+.]")).

parse_test() ->
  ?assertEqual([put,get,{loop, [add,subtract,put,get,{loop, [get]}]}, add],
               parse([put, get, begin_loop, add, subtract, put, get, begin_loop,
                      get,end_loop,end_loop,add])).

%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

