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
-module(bfer_code_generator).

%%=============================================================================
%% Defines

%%=============================================================================
%% Records
-record(s, { head  = 0
           , label = 0
           , tape  = 0
           , test  = 0
           }).

%%=============================================================================
%% Types

%% @doc Code generator state
-type s() :: #s{}.

%%=============================================================================
%% Exports

-export([ generate_code/1 ]).

%%=============================================================================
%% API functions

%% @doc Converts an bfer:ast() to LLVM ASM.
-spec generate_code(bfer:ast()) -> string().
generate_code(Tree) ->
  Steps     = steps(),
  {[],_S,C} = lists:foldl(fun(F, {N,S,AC}) -> F(N,S,AC) end, init(Tree), Steps),
  C.

%%=============================================================================
%% Internal functions

%%-----------------------------------------------------------------------------
%% Initialization functions

%% @hidden Create initial arguments
-spec init(bfer:ast()) -> {bfer:ast(), s(), string()}.
init(Tree) -> {Tree, #s{}, ""}.

%% @hidden Returns the steps neccessary to convert an AST to LLVM Asm
-spec steps() -> [fun()].
steps() ->
 [ fun header/3         %% Add header
 , fun tree_to_string/3 %% Convert AST to code
 , fun footer/3         %% Add footer
 ].

%%-----------------------------------------------------------------------------
%% Code generation steps

%% @hidden declarations and such
-spec header(bfer:ast(), s(), string()) -> {bfer:ast(), s(), string()}.
header(Nodes, #s{label=Label, head=Head} = S, _Code0 = "") ->
  Str = "declare void @llvm.memset.i32(i8* nocapture, i8, i32, i32) nounwind~n"
        "declare i32 @getchar()~n"
        "declare i32 @putchar(i32)~n"
        "declare noalias i8* @malloc(i32) nounwind~n"
        "declare void @free(i8*) nounwind~n"
        "define void @main() {~n"
        "main.~p:~n"
        "  %arr = call i8* @malloc(i32 65536)~n"
        "  call void @llvm.memset.i32(i8* %arr, i8 0, i32 65536, i32 1)~n"
        "  %head.~p = getelementptr i8* %arr, i32 32768~n",
 Code = format(Str, [Label, Head]),
 {Nodes, S, Code}.

%% @hidden Convert an ast() to LLVM ASM given state
-spec tree_to_string(bfer:ast(), s(), string()) -> {bfer:ast(), s(), string()}.
tree_to_string(Nodes = [], S, Code) ->
  {Nodes, S, Code};
tree_to_string([Node | Nodes], #s{head=Head, tape=Tape0} = S0, Code0)
  when Node =:= add;
       Node =:= sub
       ->
  Tape1 = Tape0 + 1,
  Tape  = Tape1 + 1,
  Num   = case Node of
            sub -> -1;
            add -> 1
          end,
  Args  = [ Code0
          , Tape1, Head
          , Tape, Tape1, Num
          , Tape, Head
          ],
  Str   = "~s"
          "  %tape.~p = load i8* %head.~p~n"
          "  %tape.~p = add i8 %tape.~p, ~p~n"
          "  store i8 %tape.~p, i8* %head.~p~n",
  Code  = format(Str, Args),
  S     = S0#s{tape=Tape},
    tree_to_string(Nodes, S, Code);
tree_to_string([Node | Nodes], #s{head=Head0} = S0, Code0)
  when Node =:= left;
       Node =:= right
       ->
  Head  = Head0 + 1,
  Num   = case Node of
            left  -> -1;
            right -> 1
          end,
  Args  = [ Code0, Head, Head0, Num ],
  Str   = "~s"
          "  %head.~p = getelementptr i8* %head.~p, i32 ~p~n",
  Code  = format(Str, Args),
  S     = S0#s{head=Head},
  tree_to_string(Nodes, S, Code);
tree_to_string([get | Nodes], #s{tape=Tape0, head=Head} = S0, Code0) ->
  Tape1 = Tape0 + 1,
  Tape  = Tape1 + 1,
  Args  = [Code0, Tape0, Tape1, Tape0, Tape1, Head],
  Str   = "~s"
          "  %tape.~p = call i32 @getchar()~n"
          "  %tape.~p = trunc i32 %tape.~p to i8~n"
          "  store i8 %tape.~p, i8* %head.~p~n",
  Code  = format(Str, Args),
  S     = S0#s{tape=Tape},
  tree_to_string(Nodes, S, Code);
tree_to_string([put | Nodes], #s{tape=Tape0, head=Head} = S0, Code0) ->
  Tape1 = Tape0 + 1,
  Tape  = Tape1 + 1,
  Args  = [Code0, Tape1, Head, Tape, Tape1, Tape],
  Str   = "~s"
          "  %tape.~p = load i8* %head.~p~n"
          "  %tape.~p = sext i8 %tape.~p to i32~n"
          "  call i32 @putchar(i32 %tape.~p)~n",
  Code  = format(Str, Args),
  S     = S0#s{tape=Tape},
  tree_to_string(Nodes, S, Code);
tree_to_string([{loop, LoopNodes} | Nodes], #s{} = S0, Code0) ->
  #s{head  = Head0, label = Label0}  = S0,
  Head1              = Head0 + 1,
  LoopTest           = Label0 + 1,
  LoopBody           = LoopTest + 1,
  LoopAfter          = LoopBody + 1,
  S1                 = S0#s{head=Head1, label=LoopAfter},
  {[], S2, LoopCode} = tree_to_string(LoopNodes, S1, ""),
  Tape               = S2#s.tape + 1,
  Test               = S2#s.test + 1,
  Head2              = S2#s.head,
  Head               = S2#s.head + 1,
  Str  = "~s"
         "  br label %main.~p ; [~n"
         "  main.~p: ; loop-body~n"
         "~s"
         "  br label %main.~p ; ]~n"
         "  main.~p: ; loop-test~n"
         "  %head.~p = phi i8* [%head.~p, %main.~p], [%head.~p, %main.~p]~n"
         "  %tape.~p = load i8* %head.~p~n"
         "  %test.~p = icmp eq i8 %tape.~p, 0~n"
         "  br i1 %test.~p, label %main.~p, label %main.~p~n"
         "  main.~p: ; loop-after~n"
         "  %head.~p = phi i8* [%head.~p, %main.~p]~n",
  Args = [ Code0
         , LoopTest
         , LoopBody
         , LoopCode
         , LoopTest
         , LoopTest
         , Head1
         , Head0
         , Label0
         , Head2
         , LoopBody
         , Tape
         , Head1
         , Test
         , Tape
         , Test
         , LoopAfter
         , LoopBody
         , LoopAfter
         , Head
         , Head1
         , LoopTest
         ],
  S    = S2#s{tape=Tape, test=Test, head=Head},
  Code = format(Str, Args),
  tree_to_string(Nodes, S, Code).

%% @hidden return and free memory
-spec footer(bfer:ast(), s(), string()) -> {bfer:ast(), s(), string()}.
footer(Nodes = [], S, Code0) ->
  FooterCode = "  call void @free(i8* %arr)\n"
               "  ret void\n"
               "}\n",
  Code       = format("~s~s", [Code0, FooterCode]),
  {Nodes, S, Code}.

%%-----------------------------------------------------------------------------
%% Helpers

format(S, A) -> lists:flatten(io_lib:format(S,A)).

%%=============================================================================
%% Test cases
%%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

tree_to_string_test_() ->
  [ ?_assertEqual("  %tape.1 = load i8* %head.0\n"
                  "  %tape.2 = add i8 %tape.1, 1\n"
                  "  store i8 %tape.2, i8* %head.0\n",
                  element(3,tree_to_string([add], #s{}, "")))
  , ?_assertEqual("  %tape.1 = load i8* %head.0\n"
                  "  %tape.2 = add i8 %tape.1, -1\n"
                  "  store i8 %tape.2, i8* %head.0\n",
                  element(3,tree_to_string([sub], #s{}, "")))
  , ?_assertEqual("  %head.1 = getelementptr i8* %head.0, i32 -1\n",
                  element(3,tree_to_string([left], #s{}, "")))
  , ?_assertEqual("  %head.1 = getelementptr i8* %head.0, i32 1\n",
                  element(3,tree_to_string([right], #s{}, "")))
  , ?_assertEqual("  %tape.1 = load i8* %head.0\n"
                  "  %tape.2 = sext i8 %tape.1 to i32\n"
                  "  call i32 @putchar(i32 %tape.2)\n",
                  element(3,tree_to_string([put], #s{}, "")))
  , ?_assertEqual("  %tape.0 = call i32 @getchar()\n"
                  "  %tape.1 = trunc i32 %tape.0 to i8\n"
                  "  store i8 %tape.1, i8* %head.0\n",
                  element(3,tree_to_string([get], #s{}, "")))
  , ?_assertEqual("  br label %main.1\ ; [\n"
                  "  main.2: ; loop-body\n"
                  "  br label %main.1 ; ]\n"
                  "  main.1: ; loop-test\n"
                  "  %head.1 = phi i8* [%head.0, %main.0], [%head.1, %main.2]\n"
                  "  %tape.1 = load i8* %head.1\n"
                  "  %test.1 = icmp eq i8 %tape.1, 0\n"
                  "  br i1 %test.1, label %main.3, label %main.2\n"
                  "  main.3: ; loop-after\n"
                  "  %head.2 = phi i8* [%head.1, %main.1]\n",
                  element(3,tree_to_string([{loop, []}], #s{}, "")))
  ].

%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
