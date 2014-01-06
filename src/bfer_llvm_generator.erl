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
-module(bfer_llvm_generator).

%%=============================================================================
%% Defines

%%=============================================================================
%% Records

-record(s, { head         = 0
           , label        = 0
           , tape         = 0
           , test         = 0
           , indent_depth = 0
           , indent_level = 2
           , indent_chr   = $ % space
           }).

%%=============================================================================
%% Types

-type s() :: #s{}.

%%=============================================================================
%% Exports

-export([ generate_code/1 ]).

%%=============================================================================
%% API functions

%% @doc Converts bfer_lib:ir() to LLVM ASM.
-spec generate_code(bfer_lib:ir()) -> string().
generate_code(Tree) ->
  Steps     = steps(),
  {[],_S,C} = lists:foldl(fun(F, {N,S,AC}) -> F(N,S,AC) end, init(Tree), Steps),
  lists:flatten(C).

%%=============================================================================
%% Internal functions

%%-----------------------------------------------------------------------------
%% Initialization functions

%% @hidden Create initial arguments
-spec init(bfer_lib:ast()) -> {bfer_lib:ast(), s(), io_lib:chars()}.
init(Tree) -> {Tree, #s{}, ""}.

%% @hidden Returns the steps neccessary to convert IR to LLVM Asm
-spec steps() -> [fun()].
steps() ->
 [ fun header/3         %% Add header
 , fun tree_to_chars/3 %% Convert IR to code
 , fun footer/3         %% Add footer
 ].

%%-----------------------------------------------------------------------------
%% Code generation steps

%% @hidden declarations and such
-spec header(bfer_lib:ast(), s(), string())
            -> {bfer_lib:ast(), s(), io_lib:chars()}.
header(Nodes, #s{label=Label, head=Head, indent_depth=N} = S0, _Code0 = "") ->
  S       = S0#s{indent_depth=N+(2*S0#s.indent_level)},
  Indent0 = indent_chars(S0#s{indent_depth=N+S0#s.indent_level}),
  Indent  = indent_chars(S),
  Str     = "declare void @llvm.memset.p0i8.i32(i8* nocapture, i8, i32, i32, i1"
            ") nounwind~n"
            "declare i32 @getchar()~n"
            "declare i32 @putchar(i32)~n"
            "declare noalias i8* @malloc(i32) nounwind~n"
            "declare void @free(i8*) nounwind~n"
            "define void @main() {~n"
            "~smain.~p:~n"
            "~s%arr = call i8* @malloc(i32 65536)~n"
            "~scall void @llvm.memset.p0i8.i32(i8* %arr, i8 0, i32 65536, i32 1"
            ", i1 false)~n"
            "~s%head.~p = getelementptr i8* %arr, i32 32768~n",
 Code = format(Str, [Indent0, Label, Indent, Indent, Indent, Head]),
 {Nodes, S, Code}.

%% @hidden Convert an ir() to LLVM ASM given state
-spec tree_to_chars(bfer_lib:ir(), s(), io_lib:chars())
                    -> {bfer_lib:ir(), s(), io_lib:chars()}.
tree_to_chars(Nodes = [], S, Code) ->
  {Nodes, S, Code};
tree_to_chars([{add, Num} | Nodes], #s{head=Head, tape=Tape0} = S0, Code0) ->
  Tape1 = Tape0 + 1,
  Tape  = Tape1 + 1,
  Indent= indent_chars(S0),
  Args  = [ Code0
          , Indent
          , Indent, Tape1, Head
          , Indent, Tape, Tape1, Num
          , Indent, Tape, Head
          ],
  Str   = "~s"
          "~s; add/sub~n"
          "~s%tape.~p = load i8* %head.~p~n"
          "~s%tape.~p = add i8 %tape.~p, ~p~n"
          "~sstore i8 %tape.~p, i8* %head.~p~n",
  Code  = format(Str, Args),
  S     = S0#s{tape=Tape},
  tree_to_chars(Nodes, S, Code);
tree_to_chars([{move, Num} | Nodes], #s{head=Head0} = S0, Code0) ->
  Head  = Head0 + 1,
  Indent= indent_chars(S0),
  Args  = [ Code0
          , Indent
          , Indent, Head, Head0, Num
          ],
  Str   = "~s"
          "~s; left/right~n"
          "~s%head.~p = getelementptr i8* %head.~p, i32 ~p~n",
  Code  = format(Str, Args),
  S     = S0#s{head=Head},
  tree_to_chars(Nodes, S, Code);
tree_to_chars([get | Nodes], #s{tape=Tape0, head=Head} = S0, Code0) ->
  Tape1 = Tape0 + 1,
  Tape  = Tape1 + 1,
  Indent= indent_chars(S0),
  Args  = [ Code0
          , Indent
          , Indent, Tape1
          , Indent, Tape, Tape1
          , Indent, Tape, Head
          ],
  Str   = "~s"
          "~s; get~n"
          "~s%tape.~p = call i32 @getchar()~n"
          "~s%tape.~p = trunc i32 %tape.~p to i8~n"
          "~sstore i8 %tape.~p, i8* %head.~p~n",
  Code  = format(Str, Args),
  S     = S0#s{tape=Tape},
  tree_to_chars(Nodes, S, Code);
tree_to_chars([put | Nodes], #s{tape=Tape0, head=Head} = S0, Code0) ->
  Tape1 = Tape0 + 1,
  Tape  = Tape1 + 1,
  Indent= indent_chars(S0),
  Args  = [ Code0
          , Indent
          , Indent, Tape1, Head
          , Indent, Tape, Tape1
          , Indent, Tape
          ],
  Str   = "~s"
          "~s; put~n"
          "~s%tape.~p = load i8* %head.~p~n"
          "~s%tape.~p = sext i8 %tape.~p to i32~n"
          "~scall i32 @putchar(i32 %tape.~p)~n",
  Code  = format(Str, Args),
  S     = S0#s{tape=Tape},
  tree_to_chars(Nodes, S, Code);
tree_to_chars([{loop, LoopNodes} | Nodes], #s{} = S0, Code0) ->
  #s{head  = Head0, label = Label0, indent_depth=N}  = S0,
  Indent0            = indent_chars(S0),
  Head1              = Head0 + 1,
  LoopTest           = Label0 + 1,
  LoopBody           = LoopTest + 1,
  S1                 = S0#s{ head         = Head1
                           , label        = LoopBody
                           , indent_depth = N+S0#s.indent_level},
  Indent1            = indent_chars(S1),
  {[], S2, LoopCode} = tree_to_chars(LoopNodes, S1, ""),
  LastLabel          = S2#s.label,
  LoopAfter          = LastLabel + 1,
  Tape               = S2#s.tape + 1,
  Test               = S2#s.test + 1,
  Head2              = S2#s.head,
  Head               = S2#s.head + 1,
  Str  = "~s"
         "~sbr label %main.~p ; [~n"
         "~smain.~p: ; loop-body~n"
         "~s"
         "~sbr label %main.~p ; ]~n"
         "~smain.~p: ; loop-test~n"
         "~s%head.~p = phi i8* [%head.~p, %main.~p], [%head.~p, %main.~p]~n"
         "~s%tape.~p = load i8* %head.~p~n"
         "~s%test.~p = icmp eq i8 %tape.~p, 0~n"
         "~sbr i1 %test.~p, label %main.~p, label %main.~p~n"
         "~smain.~p: ; loop-after~n"
         "~s%head.~p = phi i8* [%head.~p, %main.~p]~n",
  Args = [ Code0
         , Indent0, LoopTest
         , Indent0, LoopBody
         , LoopCode
         , Indent1, LoopTest
         , Indent0, LoopTest
         , Indent1, Head1, Head0, Label0, Head2, LastLabel
         , Indent1, Tape, Head1
         , Indent1, Test, Tape
         , Indent1, Test, LoopAfter, LoopBody
         , Indent0, LoopAfter
         , Indent1, Head, Head1, LoopTest
         ],
  S    = S2#s{ tape         = Tape
             , test         = Test
             , label        = LoopAfter
             , head         = Head
             , indent_depth = S1#s.indent_depth
             },
  Code = format(Str, Args),
  tree_to_chars(Nodes, S, Code).

%% @hidden return and free memory
-spec footer(bfer_lib:ir(), s(), io_lib:chars())
            -> {bfer_lib:ir(), s(), io_lib:chars()}.
footer(Nodes = [], S, Code0) ->
  Indent      = indent_chars(S),
  FooterCode0 = "~scall void @free(i8* %arr)\n"
                "~sret void\n"
                "}\n",
  FooterCode  = format(FooterCode0, [Indent, Indent]),
  Code        = format("~s~s", [Code0, FooterCode]),
  {Nodes, S, Code}.

%%-----------------------------------------------------------------------------
%% Helpers

indent_chars(#s{indent_chr = _IndentChr , indent_depth = 0} = _S) -> "";
indent_chars(#s{indent_chr = IndentChr  , indent_depth = N} = S)  ->
  [IndentChr | indent_chars(S#s{indent_depth=N-1})].

format(S, A) -> io_lib:format(S,A).

%%=============================================================================
%% Test cases
%%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ident_str_test_() ->
  [ ?_assertEqual(""   , indent_chars(#s{indent_depth=0}))
  , ?_assertEqual("  " , indent_chars(#s{indent_depth=2}))
  ].

tree_to_chars_test_() ->
  [ ?_assertEqual("; add/sub\n"
                  "%tape.1 = load i8* %head.0\n"
                  "%tape.2 = add i8 %tape.1, 1\n"
                  "store i8 %tape.2, i8* %head.0\n",
                  get_flattened_code(tree_to_chars([{add, 1}], #s{}, "")))
  , ?_assertEqual("; add/sub\n"
                  "%tape.1 = load i8* %head.0\n"
                  "%tape.2 = add i8 %tape.1, -1\n"
                  "store i8 %tape.2, i8* %head.0\n",
                  get_flattened_code(tree_to_chars([{add, -1}], #s{}, "")))
  , ?_assertEqual("; left/right\n"
                  "%head.1 = getelementptr i8* %head.0, i32 -1\n",
                  get_flattened_code(tree_to_chars([{move, -1}], #s{}, "")))
  , ?_assertEqual("; left/right\n"
                  "%head.1 = getelementptr i8* %head.0, i32 1\n",
                  get_flattened_code(tree_to_chars([{move, 1}], #s{}, "")))
  , ?_assertEqual("; put\n"
                  "%tape.1 = load i8* %head.0\n"
                  "%tape.2 = sext i8 %tape.1 to i32\n"
                  "call i32 @putchar(i32 %tape.2)\n",
                  get_flattened_code(tree_to_chars([put], #s{}, "")))
  , ?_assertEqual("; get\n"
                  "%tape.1 = call i32 @getchar()\n"
                  "%tape.2 = trunc i32 %tape.1 to i8\n"
                  "store i8 %tape.2, i8* %head.0\n",
                  get_flattened_code(tree_to_chars([get], #s{}, "")))
  , ?_assertEqual("br label %main.1 ; [\n"
                  "main.2: ; loop-body\n"
                  "  br label %main.1 ; ]\n"
                  "main.1: ; loop-test\n"
                  "  %head.1 = phi i8* [%head.0, %main.0], [%head.1, %main.2]\n"
                  "  %tape.1 = load i8* %head.1\n"
                  "  %test.1 = icmp eq i8 %tape.1, 0\n"
                  "  br i1 %test.1, label %main.3, label %main.2\n"
                  "main.3: ; loop-after\n"
                  "  %head.2 = phi i8* [%head.1, %main.1]\n",
                  get_flattened_code(tree_to_chars([{loop, []}], #s{}, "")))
  , ?_assertEqual(
       "br label %main.1 ; [\n"
       "main.2: ; loop-body\n"
       "  br label %main.3 ; [\n"
       "  main.4: ; loop-body\n"
       "    br label %main.3 ; ]\n"
       "  main.3: ; loop-test\n"
       "    %head.2 = phi i8* [%head.1, %main.2], [%head.2, %main.4]\n"
       "    %tape.1 = load i8* %head.2\n"
       "    %test.1 = icmp eq i8 %tape.1, 0\n"
       "    br i1 %test.1, label %main.5, label %main.4\n"
       "  main.5: ; loop-after\n"
       "    %head.3 = phi i8* [%head.2, %main.3]\n"
       "  br label %main.1 ; ]\n"
       "main.1: ; loop-test\n"
       "  %head.1 = phi i8* [%head.0, %main.0], [%head.3, %main.5]\n"
       "  %tape.2 = load i8* %head.1\n"
       "  %test.2 = icmp eq i8 %tape.2, 0\n"
       "  br i1 %test.2, label %main.6, label %main.2\n"
       "main.6: ; loop-after\n"
       "  %head.4 = phi i8* [%head.1, %main.1]\n",
       get_flattened_code(tree_to_chars([{loop, [{loop, []}]}], #s{}, "")))
  ].

get_flattened_code(T) -> lists:flatten(element(3,T)).

%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
