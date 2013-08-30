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
-module(bfer_integration_tests).

%%=============================================================================
%% Includes
-include_lib("eunit/include/eunit.hrl").

%%=============================================================================
%% Tests

%% NOTE: for environments without llvm installed, do not run
-ifndef(NO_INTEGRATION_TESTS).

hello_world_test_() ->
  { setup,
    setup("helloworld.bf"),
    teardown(),
    fun(Fname) -> ?_assertCmdOutput("Hello World!\n", "lli " ++ Fname) end
  }.

echo2_test_() ->
  { setup,
    setup("echo2.bf"),
    teardown(),
    fun(Fname) ->
      ?_assertCmdOutput("echo chamber\n", "echo echo chamber | lli " ++ Fname)
    end
  }.

%%=============================================================================
%% Internal functions

setup(BfFile) ->
  fun() ->
    Code  = bfer_lib:compile(read_file(BfFile)),
    Fname = string:strip(?cmd("mktemp"), right, $\n),
    ok    = file:write_file(Fname, Code),
    Fname
  end.

teardown() -> fun(Fname) -> ok = file:delete(Fname) end.

read_file(Fname) ->
  TestDir           = code:lib_dir(bfer) ++ "/test/",
  {ok, FileContent} = file:read_file(TestDir ++ Fname),
  binary_to_list(FileContent).

-endif.

%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
