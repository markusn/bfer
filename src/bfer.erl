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
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL MARKUS NÄSMAN BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=============================================================================
%% Module declaration
-module(bfer).

%%=============================================================================
%% Exports
-export([ compile/1 ]).

%%=============================================================================
%% Types

%% @doc Tokens
-type token()     :: statement()
                   | begin_loop
                   | end_loop.
%% @doc Abstract Syntax Tree nodes
-type statement() :: left
                   | right
                   | add
                   | sub
                   | put
                   | get.
%% @doc Abstract Syntax Tree
-type ast()       :: [statement() | {loop, ast()}].

%%=============================================================================
%% Type exports
-export_type([ token/0
             , statement/0
             , ast/0
             ]).

%%=============================================================================
%% API functions

%% @doc Brainfuck to LLVM ASM
-spec compile(string()) -> string().
compile(Code) ->
  lists:foldl(fun(F, Acc) -> F(Acc) end, Code, steps()).

%%=============================================================================
%% Internal functions

%% @hidden Compilation steps
-spec steps() -> [fun()].
steps() ->
  [ fun bfer_lexer:lex/1                    %% lex the raw BF
  , fun bfer_parser:parse/1                 %% parse the tokens
  , fun bfer_code_generator:generate_code/1 %% generate LLVM code
  ].

%%=============================================================================
%% Test cases
%%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

compile_helloworld_test() ->
  TestDir = code:lib_dir(bfer) ++ "/test/",
  {ok, LLCode}  = file:read_file(TestDir ++ "helloworld.ll"),
  {ok, BFCode}  = file:read_file(TestDir ++ "helloworld.bf"),
  ?assertEqual(binary_to_list(LLCode), compile(binary_to_list(BFCode))).

-define(HELLOWORLD,
        "+++++ +++++             initialize counter (cell #0) to 10"
        "[                       use loop to set the next four cells to"
        "70/100/30/10"
        "> +++++ ++              add  7 to cell #1"
        "> +++++ +++++           add 10 to cell #2"
        "> +++                   add  3 to cell #3"
        "> +                     add  1 to cell #4"
        "<<<< -                  decrement counter (cell #0)"
        "]"
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

%%-endif.
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
