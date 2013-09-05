%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Brainfuck compiler
%%% @end
%%% @author Markus Näsman <markus@botten.org>
%%% @copyright 2013 (c) Markus Näsman <markus@botten.org>
%%% @license Copyright (c) 2013, Markus Näsman
%%% All rights reserved.
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of the <organization> nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL MARKUS NÄSMAN BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%%% THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Module declaration
-module(bfer_parser).

%%=============================================================================
%% Exports

-export([parse/1]).

%%=============================================================================
%% Defines

%% parser guard
-define(IS_STATEMENT(S),
        S =:= left  orelse
        S =:= right orelse
        S =:= add   orelse
        S =:= sub   orelse
        S =:= put   orelse
        S =:= get).

%%=============================================================================
%% API functions

%% @doc Parse tokens using recursive descent and a LL(1) Brainfuck grammar:
%%      code      ::= statement code_prim
%%                  | begin_loop code end_loop code_prim
%%      code_prim ::= nil | code
%%      statement ::= left | right | add | sub | put | get
%% @end
-spec parse([bfer_lib:token()]) -> bfer_lib:ast().
parse(Tokens) ->
  %% NOTE: At this point all tokens should be consumed otherwise the tokens were
  %%       not part of the language specified by the grammar.
  {[], Ast} = code(Tokens, []),
  Ast.

%%=============================================================================
%% Internal functions

%% @hidden code ::= statement code_prim
%%                | begin_loop code end_loop code_prim
%% @end
-spec code([bfer_lib:token()], bfer_lib:ast())
          -> {[bfer_lib:token()], bfer_lib:ast()}.
%% code ::= begin_loop code end_loop code_prim
code([begin_loop | Tokens0], Ast)  ->
  %% We assert that the next symbol is end_loop
  {[end_loop | Tokens], LoopAst} = code(Tokens0, []),
  code_prim(Tokens, Ast++[{loop, LoopAst}]);
%% code ::= statement code_prim
code([Token|_] = Tokens, Ast0) when ?IS_STATEMENT(Token) ->
  {Rest, Ast} = statement(Tokens, Ast0),
  code_prim(Rest, Ast).

%% @hidden code_prim ::= nil | code
-spec code_prim([bfer_lib:token()], bfer_lib:ast())
               -> {[bfer_lib:token()], bfer_lib:ast()}.
code_prim([            ]          , Ast) -> {[], Ast};         %% nil
code_prim([end_loop | _] = Tokens , Ast) -> {Tokens, Ast};     %% look ahead!
code_prim([_        | _] = Tokens , Ast) -> code(Tokens, Ast). %% code

%% @hidden statement ::= left | right | add | sub | put | get
-spec statement([bfer_lib:token()], bfer_lib:ast())
               -> {[bfer_lib:token()], bfer_lib:ast()}.
statement([Token|Tokens], Ast) when ?IS_STATEMENT(Token) ->
  {Tokens, Ast ++ [Token]}.

%%=============================================================================
%% Unit tests
%%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
  [ {"Well-formed program",
     ?_assertEqual([put, get, {loop, [add, sub, put, get, {loop, [get]}]}, add],
                   parse([put, get, begin_loop, add, sub, put, get, begin_loop,
                          get, end_loop, end_loop, add]))}
  , {"Loop start without loop end rejected",
     ?_assertError(_,
                   parse([put,begin_loop]))}
  , {"Loop end without loop start rejected",
     ?_assertError(_,
                   parse([end_loop]))}
  , {"Empty program rejected",
     ?_assertError(_,
                   parse([]))}
  ].

%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
