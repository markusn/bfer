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
-module(bfer_ir_generator).

%%=============================================================================
%% Defines

-define(IS_ADDSUB_OR_LEFTRIGHT(N) ,
        N =:= add orelse N=:= sub orelse N =:= left orelse N=:=right).

%%=============================================================================
%% Records

%%=============================================================================
%% Types

%%=============================================================================
%% Exports

-export([ generate_code/1 ]).

%%=============================================================================
%% API functions

%% @doc Converts an bfer_lib:ast() to IR.
-spec generate_code(bfer_lib:ast()) -> bfer_lib:ir().
generate_code([])                                              ->
  [];
generate_code([Node|Nodes]) when ?IS_ADDSUB_OR_LEFTRIGHT(Node) ->
  [{generate_op(Node), generate_val(Node)} | generate_code(Nodes)];
generate_code([{loop, LoopNodes}|Nodes]) ->
  [{loop, generate_code(LoopNodes)} | generate_code(Nodes)];
generate_code([Node | Nodes]) when Node =:= put; Node =:= get ->
  [Node | generate_code(Nodes)].

generate_op(add)   -> add;
generate_op(sub)   -> add;
generate_op(left)  -> move;
generate_op(right) -> move.

generate_val(add)   -> 1;
generate_val(sub)   -> -1;
generate_val(left)  -> -1;
generate_val(right) -> 1.

%%=============================================================================
%% Test cases
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

generate_code_test_() ->
  [ ?_assertEqual(
       [{add, -1}],
       generate_code([sub]))
  , ?_assertEqual(
       [{move, 1}],
       generate_code([right]))
  , ?_assertEqual(
       [{loop, [{move, -1}]}],
       generate_code([{loop, [left]}]))
  , ?_assertEqual(
       [{loop, [{loop, [{add, 1}]}]}],
       generate_code([{loop, [{loop, [add]}]}]))
  , ?_assertEqual(
       [put, get, {loop, [{add, 1}, {add, -1}, put,get, {loop, [get]}]},
        {add, 1}],
       generate_code([put, get,{loop, [add, sub, put,get, {loop, [get]}]},
                      add]))
  ].

-endif.
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
