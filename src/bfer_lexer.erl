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
-module(bfer_lexer).

%%=============================================================================
%% Exports

-export([lex/1]).

%%=============================================================================
%% API functions

-spec lex(string()) -> [bfer_lib:token()].
lex([])         -> [                     ];
lex("<" ++ Str) -> [left       | lex(Str)];
lex(">" ++ Str) -> [right      | lex(Str)];
lex("+" ++ Str) -> [add        | lex(Str)];
lex("-" ++ Str) -> [sub        | lex(Str)];
lex("." ++ Str) -> [put        | lex(Str)];
lex("," ++ Str) -> [get        | lex(Str)];
lex("[" ++ Str) -> [begin_loop | lex(Str)];
lex("]" ++ Str) -> [end_loop   | lex(Str)];
lex([_A|Str])   -> lex(Str). %% Ignore everything else

%%=============================================================================
%% Unit tests
%%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

lex_test_() ->
  [ {"Well-formed program",
     ?_assertEqual([left,right,add,add,add,sub,begin_loop,add,put,get,end_loop],
                   lex("<>++ +foo-[+.,]"))}
  ].

%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
