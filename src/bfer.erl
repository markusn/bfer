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
-module(bfer).

%%=============================================================================
%% Exports
-export([ main/1 ]).

%%=============================================================================
%% API functions

%% @doc Main method when invoked as escript, uses llc and cc to compile
%%      brainfuck code into native code
%% @end
main(Args) -> main(Args, "llc", "cc").

%%=============================================================================
%% Internal functions

main([], _Llc, _Cc)            -> io:format("Usage: bfer [-o] IN OUT~n");
main([In, Out], Llc, Cc)       -> compile(In, false, Out, Llc, Cc);
main(["-o", In, Out], Llc, Cc) -> compile(In, true, Out, Llc, Cc).

compile(In, Optimize, Out, Cc, Llc) ->
  {ok, InFile}  = file:read_file(In),
  Code          = bfer_lib:compile(binary_to_list(InFile), Optimize),
  ok            = file:write_file(Out++".ll", Code),
  ResLlc        = os:cmd(Llc ++ " " ++ Out ++ ".ll"),
  [io:format("~s~n", [ResLlc]) || ResLlc =/= []],
  ResCc         = os:cmd(Cc ++ " " ++ Out ++ ".s" ++ " -o " ++ Out),
  [io:format("~s~n", [ResCc]) || ResCc =/= []],
  ok.

%%=============================================================================
%% Test cases
%%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Just assert that nothing crashes
main_test_() ->
  TestDir  = code:lib_dir(bfer) ++ "/test/fixtures/",
  TestFile = TestDir ++ "helloworld.bf",
  { setup
  , fun()        -> string:strip(?cmd("mktemp"), right, $\n) end
  , fun(Fname)   -> ok = file:delete(Fname) end
  , fun(TmpFile) ->
        [ ?_assertEqual(ok, main([], "echo", "echo"))
        , ?_assertEqual(ok, main([TestFile, TmpFile], "echo", "echo"))
        , ?_assertEqual(ok, main(["-o", TestFile, TmpFile], "echo", "echo"))
        ]
    end
  }.


%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
