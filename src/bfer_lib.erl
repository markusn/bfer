%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Brainfuck compiler
%%% @end
%%% @author Markus Ekholm <markus@botten.org>
%%% @copyright 2013 (c) Markus Ekholm <markus@botten.org>
%%% @license Copyright (c) 2013, Markus Ekholm
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
-module(bfer_lib).

%%=============================================================================
%% Exports
-export([ compile/2 ]).

%%=============================================================================
%% Types

-type token()     :: statement()
                   | begin_loop
                   | end_loop.
-type statement() :: left
                   | right
                   | add
                   | sub
                   | put
                   | get.

-type ast()       :: [statement() | {loop, ast()}].

-type ir()        :: [ put
                     | get
                     | {move, integer()}
                     | {add, integer()}
                     | {loop, ir()}
                     ].

%%=============================================================================
%% Type exports
-export_type([ token/0
             , statement/0
             , ast/0
             , ir/0
             ]).

%%=============================================================================
%% API functions

%% @doc Brainfuck to LLVM ASM
-spec compile(string(), boolean()) -> string().
compile(Code, Optimize) ->
  lists:foldl(fun(F, Acc) -> F(Acc) end, Code, steps(Optimize)).

%%=============================================================================
%% Internal functions

%% @hidden Compilation steps
-spec steps(boolean())-> [fun(),...].
steps(Optimize) ->
  [ fun bfer_lexer:lex/1                    %% lex the raw BF
  , fun bfer_parser:parse/1                 %% parse the tokens
  , fun bfer_ir_generator:generate_code/1   %% generate IR
  , optimizer(Optimize)                     %% Optimize if specified
  , fun bfer_llvm_generator:generate_code/1 %% generate LLVM code
  ].

optimizer(false) -> fun(X) -> X end;
optimizer(true)  -> fun bfer_ir_optimizer:optimize/1.

%%=============================================================================
%% Test cases
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

compile_helloworld_test() ->
  TestDir         = code:lib_dir(bfer) ++ "/test/fixtures/",
  {ok, BFCode}    = file:read_file(TestDir ++ "helloworld.bf"),
  {ok, LLCode}    = file:read_file(TestDir ++ "helloworld.ll"),
  {ok, LLCodeOpt} = file:read_file(TestDir ++ "helloworld.optimized.ll"),
  ?assertEqual(binary_to_list(LLCode), compile(binary_to_list(BFCode), false)),
  ?assertEqual(binary_to_list(LLCodeOpt),compile(binary_to_list(BFCode), true)).

-endif.
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
