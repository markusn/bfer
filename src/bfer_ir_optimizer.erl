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
-module(bfer_ir_optimizer).

%%=============================================================================
%% Defines

%%=============================================================================
%% Records

%%=============================================================================
%% Types

%%=============================================================================
%% Exports

-export([ optimize/1 ]).

%%=============================================================================
%% API functions

%% @doc Optimize ir()
-spec optimize(bfer_lib:ir()) -> bfer_lib:ir().

optimize(Code) ->
  lists:foldl(fun(F, Acc) -> F(Acc) end, Code, steps()).

%%=============================================================================
%% Internal functions

steps() ->
  [ fun minimize/1
  , fun remove_nops/1
  ].

%% @hidden Minimize moves and adds
minimize([])                             ->
  [];
minimize([{move, X}, {move, Y} | Nodes]) ->
  minimize([{move, X+Y} | Nodes]);
minimize([{add, X}, {add, Y} | Nodes])   ->
  minimize([{add, X+Y} | Nodes]);
minimize([{loop, LoopNodes} | Nodes])    ->
  [{loop, minimize(LoopNodes)} | minimize(Nodes)];
minimize([Node | Nodes])                 ->
  [Node | minimize(Nodes)].

%% @hidden Remove all null moves and adds
remove_nops([])                                             ->
  [];
remove_nops([{loop, LoopNodes} | Nodes])                    ->
  [{loop, remove_nops(LoopNodes)} | remove_nops(Nodes)];
remove_nops([{Op, 0} | Nodes]) when Op =:= add; Op =:= move ->
  remove_nops(Nodes);
remove_nops([Node | Nodes])                                 ->
  [Node | remove_nops(Nodes)].

%%=============================================================================
%% Test cases
%%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

optimize_test_() ->
  [ ?_assertEqual(
       [{move, 3}, put, {loop, [{add, 2}]}, get],
       optimize([{move, 1}, {move, 1}, {move, -1}, {move, 1}, {move, 1},
                 put, {loop, [{add, 1}, {add, -1}, {add, 1}, {add, 1}]}, get,
                 {move, 0}]))
  ].

%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
