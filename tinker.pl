/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022-2025, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(tinker,
          [ cls/0,		%
            pp/1,               % +Term
            html/1,		% +HTMLTerm
            tinker_run/2,	% +TinkerQuery, :QueryString
            tinker_query/1,     % -TinkerQuery
            dump_var/2
          ]).
:- use_module(library(wasm)).
:- use_module(library(dom)).
:- use_module(library(prolog_wrap), [wrap_predicate/4]).
:- use_module(library(ansi_term), [ansi_format/3]).
:- use_module(library(debug), [debug/3]).
:- use_module(library(uri), [uri_is_global/1]).
:- use_module(library(option), [option/2]).
:- autoload(library(apply), [maplist/3]).
:- autoload(library(listing), [listing/1]).
:- autoload(library(pairs), [transpose_pairs/2, group_pairs_by_key/2]).
:- autoload(library(prolog_stack),
            [get_prolog_backtrace/3, print_prolog_backtrace/3,
             prolog_stack_frame_property/2]).
:- autoload(library(utf8), [utf8_codes/3]).
:- autoload(library(dcg/basics), [string/3, number/3, remainder//1]).
:- autoload(library(http/term_html), [term//2]).
:- autoload(library(ansi_term), [ansi_format/3]).
:- autoload(library(lists), [append/3]).
:- autoload(library(pprint), [print_term/2]).

:- create_prolog_flag(html_term, true, [keep(true)]).

:- meta_predicate
    html(:),
    tinker_run(+, :).

:- html_meta
    html(html).

% imported from library(wasm).  This enables development.
:- op(700, xfx, :=).           % Result := Expression
:- op(50,  fx,  #).            % #Value
:- op(40,  yf,  []).           % Expr[Expr]

:- public
    tinker_init/1,
    tty_link/1,
    trace_action/2,
    complete_input/4.

%!  tinker_init(+UserDir)

tinker_init(UserDir) :-
    set_prolog_flag(tty_control, true),
    set_prolog_flag(color_term, true),
    set_prolog_flag(hyperlink_term, true),
    set_stream(user_input, tty(true)),
    set_stream(user_output, tty(true)),
    set_stream(user_error, tty(true)),
    working_directory(_, UserDir).

%!  tinker_run(+TinkerQuery, :Query:string)

tinker_run(TinkerQuery, Query) :-
    setup_call_cleanup(
        nb_setval(tinker_query, TinkerQuery),
        wasm_query(Query),
        nb_delete(tinker_query)).

%!  tinker_query(-TinkerQuery) is det.
%
%   TinkerQuery is the JavaScript object running this query.

tinker_query(TinkerQuery) :-
    nb_current(tinker_query, TinkerQuery).

:- multifile
    prolog_edit:edit_source/1,
    prolog_edit:exists_location/1,
    prolog_edit:user_select/2.

%!  prolog_edit:edit_source(++Spec)
%
%   Make edit/1 work by filling the editor and trying to select the
%   right line.

prolog_edit:edit_source(Spec) :-
    edit_source(Spec).

edit_source(Spec) :-
    Source := tinker.source,
    edit_source(Source, Spec).

edit_source(Source, Options) :-
    #{file:File} :< Options,
    (   File := Source.files.current
    ->  true
    ;   load_file(File, String),
        _ := Source.addFileOption(#File),
        _ := Source.switchToFile(#File),
        Source.value := String
    ),
    (   #{line:Line} :< Options
    ->  _ := Source.goto(Line, Options)
    ;   true
    ).

prolog_edit:exists_location(Spec) :-
    #{file:File} :< Spec,
    sub_atom(File, 0, _, _, '/swipl/').

load_file(Spec, String) :-
    uri_is_global(Spec),
    !,
    fetch(Spec, text, String).
load_file(Spec, String) :-
    exists_file(Spec),
    !,
    setup_call_cleanup(
        open(Spec, read, In),
        read_string(In, _Len, String),
        close(In)).
load_file(Spec, String) :-
    sub_atom(Spec, 0, _, _, '/swipl/'),
    atom_concat('/wasm', Spec, URL),
    fetch(URL, text, String),
    setup_call_cleanup(
        open(Spec, write, In),
        format(In, '~s', [String]),
        close(In)).

% As the location is a hyperlink, there is no need to ask for
% keyboard interaction.
prolog_edit:user_select(_Max, _I) :-
    !.

%!  tty_link(+Link) is det.
%
%   Handle a terminal hyperlink to ``file://`` links

tty_link(Link) :-
    string_concat("file://", Encoded, Link),
    !,
    debug(tty(hyperlink), 'Opening tty file link ~p~n', [Link]),
    string_codes(Encoded, Codes),
    phrase(percent_decode(UTF8), Codes),
    phrase(utf8_codes(FileCodes), UTF8),
    phrase(link_location(Location), FileCodes),
    edit_source(Location).
tty_link(Link) :-
    debug(tty(hyperlink), 'Opening tty link ~p~n', [Link]),
    string_codes(Link, Codes),
    phrase(percent_decode(UTF8), Codes),
    phrase(utf8_codes(URLCodes), UTF8),
    phrase(link_location(Location), URLCodes),
    memberchk(file(URL), Location),
    uri_is_global(URL),
    file_name_extension(_, pl, URL),
    !,
    edit_source(Location).

percent_decode([H|T]) -->
    "%", [D1, D2],
    { code_type(D1, xdigit(X1)),
      code_type(D2, xdigit(X2)),
      !,
      H is (X1<<4) + X2
    },
    percent_decode(T).
percent_decode([H|T]) -->
    [H],
    !,
    percent_decode(T).
percent_decode([]) -->
    [].

link_location([file(File),line(Line),linepos(Column)]) -->
    string(Codes), "#", number(Line), ":", number(Column),
    !,
    { atom_codes(File, Codes) }.
link_location([file(File),line(Line)]) -->
    string(Codes), "#", number(Line),
    !,
    { atom_codes(File, Codes) }.
link_location([file(File)]) -->
    remainder(Codes),
    !,
    { atom_codes(File, Codes) }.


%!  trace_action(+Action, +Message) is det.
%
%   Perform actions on behalf of the debugger, such as printing the
%   current goal, etc.
%
%   @arg Message is a term frame(Frame, Choice, Port, PC) as provided
%   by PL_get_trace_context()

trace_action(print, Msg) =>
    print_message(debug, Msg),
    (   show_source_location(Msg)
    ->  true
    ;   true
    ).
trace_action(goals, frame(Frame,_Choice,_Port,_PC)) =>
    dbg_backtrace(Frame, 5).
trace_action(listing, frame(Frame,_Choice,_Port,_PC)) =>
    prolog_frame_attribute(Frame, predicate_indicator, Pred),
    listing(Pred).
trace_action(help, _) =>
    tinker_query(Q),
    Actions := Q.shortcuts("trace"),
    dict_pairs(Actions, _, Pairs),
    transpose_pairs(Pairs, Transposed),
    group_pairs_by_key(Transposed, Grouped),
    print_message(information, trace_help_table(Grouped)).

dbg_backtrace(Frame, Depth) :-
    get_prolog_backtrace(Depth, Stack,
                         [ frame(Frame),
                           goal_term_depth(10)
                         ]),
    print_prolog_backtrace(user_error, Stack,
                           [ show_files(basename)
                           ]).


show_source_location(frame(Frame, _Choice, Port, _PC)) :-
    prolog_frame_attribute(Frame, pc, PC),
    prolog_frame_attribute(Frame, parent, Parent),
    prolog_frame_attribute(Parent, clause, Clause),
    prolog_stack_frame_property(frame(_,clause(Clause,PC),_),
                                location(File:Line)),
    !,
    show_trace_source(Port, File:Line).
show_source_location(frame(Frame, _Choice, Port, _PC)) :-
    prolog_frame_attribute(Frame, clause, Clause),
    clause_property(Clause, file(File)),
    clause_property(Clause, line_count(Line)),
    !,
    show_trace_source(Port, File:Line).
show_source_location(frame(Frame, _Choice, Port, _PC)) :-
    prolog_frame_attribute(Frame, goal, Goal),
    predicate_property(Goal, file(File)),
    predicate_property(Goal, line_count(Line)),
    !,
    show_trace_source(Port, File:Line).

%!  show_trace_source(Port, File:Line) is semidet.
%
%   Show the source location in the editor.  Note that we can not
%   download files if we are in a non-async callback.
%
%   @tbd Can we start an async call   from here? Alternatively, we could
%   return the information and  let   Query#traceAction  create an async
%   goal to show the source.

show_trace_source(Port, File:Line) :-
    (   is_async
    ->  true
    ;   access_file(File, read)
    ),
    port_css_class(Port, CSSClass, Title),
    tinker_query(Q),
    Source := Q.console.source,
    edit_source(Source, #{file: File,
                          line: Line,
                          className: CSSClass,
                          title:Title
                         }).

port_css_class(call,           "CodeMirror-trace-call",      "Trace call port").
port_css_class(exit,           "CodeMirror-trace-exit",      "Trace exit port").
port_css_class(fail,           "CodeMirror-trace-fail",      "Trace fail port").
port_css_class(redo(_PC),      "CodeMirror-trace-redo",      "Trace redo port").
port_css_class(exception(_Ex), "CodeMirror-trace-exception", "Trace exception").


                /*******************************
                *         TTY SUPPORT          *
                *******************************/

:- abolish(system:get_single_char/1).
system:get_single_char(Code) :-
    tinker_query(Q),
    Promise := Q.get_single_char(),
    (   integer(Promise)                % typically -1 for error
    ->  Code = Promise
    ;   await(Promise, Code)
    ).

system:tty_size(Rows, Columns) :-
    tinker_query(Q),
    [Rows,Columns] := Q.tty_size().

reading_tty :-
    current_input(Input),
    reading_tty(Input).

reading_tty(Input) :-
    stream_property(Input, tty(true)).

read_from_user(What, Term, Options) :-
    await(What, Text),
    echo(Text),
    term_string(Term, Text, Options).

:- wrap_predicate(system:read(Term), tty, Closure,
                  (   reading_tty
                  ->  read_from_user(term, Term, [])
                  ;   Closure
                  )).
:- wrap_predicate(system:read(Stream, Term), tty, Closure,
                  (   reading_tty(Stream)
                  ->  read_from_user(term, Term, [])
                  ;   Closure
                  )).
:- wrap_predicate(system:read_term(Term, Options), tty, Closure,
                  (   reading_tty
                  ->  read_from_user(term, Term, Options)
                  ;   Closure
                  )).
:- wrap_predicate(system:read_term(Stream, Term, Options), tty, Closure,
                  (   reading_tty(Stream)
                  ->  read_from_user(term, Term, Options)
                  ;   Closure
                  )).
% Single character input
:- wrap_predicate(system:get_code(Code), tty, Closure,
                  (   reading_tty
                  ->  read_tty(code, Code)
                  ;   Closure
                  )).
:- wrap_predicate(system:get_code(Stream, Code), tty, Closure,
                  (   reading_tty(Stream)
                  ->  read_tty(code, Code)
                  ;   Closure
                  )).
:- wrap_predicate(system:get_char(Code), tty, Closure,
                  (   reading_tty
                  ->  read_tty(char, Code)
                  ;   Closure
                  )).
:- wrap_predicate(system:get_char(Stream, Code), tty, Closure,
                  (   reading_tty(Stream)
                  ->  read_tty(char, Code)
                  ;   Closure
                  )).
:- wrap_predicate(system:get0(Code), tty, Closure,
                  (   reading_tty
                  ->  read_tty(code, Code)
                  ;   Closure
                  )).
:- wrap_predicate(system:get(Code), tty, Closure,
                  (   reading_tty
                  ->  read_tty(get, Code)
                  ;   Closure
                  )).

read_tty(As, Result) :-
    nb_current(tty_line, Line),
    Line = [H|T],
    !,
    nb_setval(tty_line, T),
    read_type(As, H, Result).
read_tty(As, Result) :-
    await(line, Text),
    echo(Text),
    string_codes(Text, Codes),
    append(Codes, [10], Line),
    nb_setval(tty_line, Line),
    read_tty(As, Result).

echo(Text) :-
    current_prompt(Prompt),
    ansi_format(bold, '~w', [Prompt]),
    ansi_format(fg(blue), '~w~n', [Text]).

current_prompt(Prompt) :-
    prompt1(Prompt),
    !.
current_prompt(Prompt) :-
    prompt(Prompt, Prompt),
    !.

read_type(code, Code, Res) =>
    Res = Code.
read_type(char, Code, Char) =>
    char_code(Char, Code).
read_type(get, Code, Res) =>
    (   code_type(Code, space)
    ->  read_tty(get, Res)
    ;   Res = Code
    ).

:- wrap_predicate(system:'$consult_user'(_Id), tty, _Closure,
                  (   print_message(warning, wasm(consult_user)),
                      fail)).

:- wrap_predicate(system:absolute_file_name(Spec, Path, Options), wasm, Closure,
                  (   wasm_absolute_file_name(Spec, Path, Options)
                  ->  true
                  ;   Closure
                  )).

wasm_absolute_file_name(Spec, Path, Options) :-
    compound(Spec),
    compound_name_arity(Spec, _Alias, 1),
    nonvar(Options),                    % Deal with old arguments
    option_extension(Ext, Options),
    \+ memberchk(wasm(true), Options),
    absolute_file_name(Spec, Path0, [wasm(true),solutions(all)]),
    uri_is_global(Path0),
    ensure_extension(Path0, Ext, Path).

option_extension(Ext, Options) :-
    option(file_type(Type), Options),
    type_extension(Type, Ext).

type_extension(prolog, pl).
type_extension(source, pl).

ensure_extension(File0, Ext, File) :-
    file_name_extension(_, Ext, File0),
    !,
    File = File0.
ensure_extension(File0, Ext, File) :-
    file_name_extension(File0, Ext, File).

%!  complete_input(+Before, +After, -Delete, -Completions) is det.
%
%   Perform completion on a query.

:- dynamic complete_input_loaded/0.
complete_input(Before,After,Delete,Completions) :-
    complete_input_loaded,
    !,
    prolog:complete_input(Before,After,Delete,Completions).
complete_input(Before,After,Delete,Completions) :-
    use_module(library(console_input)),
    asserta(complete_input_loaded),
    !,
    prolog:complete_input(Before,After,Delete,Completions).

%!  pp(@Term) is det.
%
%   Pretty print a term to the  console.   When  used  in a clause, call
%   dump_var/2 to add the name.

user:goal_expansion(pp(Term), Pos, dump_var(Name, Term), Pos) :-
%	prolog_load_context(term_position, Start),
	var_property(Term, name(Name)).

pp(Term) :-
	print_term(Term, [output(user_error)]).

%!  dump_var(+Name, @Value) is det.

dump_var(Name, Value) :-
	with_output_to(user_error, dump_var2(Name, Value)).

dump_var2(Name, Value) :-
	format('~N'),
	ansi_format([bold,fg(magenta)], '~w', [Name]),
	format(' = '),
	atom_length(Name, NameLen),
	LeftMargin is NameLen + 3,
	print_term(Value,
		   [ output(current_output),
		     left_margin(LeftMargin)
		   ]),
	nl.

                /*******************************
                *         HTML SUPPORT         *
                *******************************/

%!  cls
%
%   Clear the output window.

cls :-
    tinker_query(Q),
    _ := Q.console.clear().

%!  html(:Term)
%
%   Insert  HTML  into  the  current answer  using  SWI-Prolog's  HTML
%   generation infra structure.   For example:
%
%       ?- html(['Hello ', b(world)]).

html(Term) :-
    tinker_query(Q),
    Answer := Q.answer,
    append_html(Answer, Term).

:- multifile
    term_html:blob_rendering//3.

term_html:blob_rendering(_Type, Blob, _Options) -->
    { format(string(String), '~q', [Blob])
    },
    html(span(class('pl-blob'), String)).

:- multifile
    prolog:message_line_element/3.

prolog:message_line_element(S, '~W'-[T,Options]) :-
    current_prolog_flag(html_term, true),
    stream_property(S, tty(true)),
    flush_output(S),
    memberchk(html(true), Options),
    html(\term(T, [emit(html)|Options])).

modify_write_options(true,  Opts, Opts1) =>
    Opts1 = [html(true)|Opts].
modify_write_options(false, Opts, Opts1) =>
    delete(Opts, html(_), Opts1).

modify_write_options(Enable, Flag) =>
    current_prolog_flag(Flag, Opts0),
    modify_write_options(Enable, Opts0, Opts),
    set_prolog_flag(Flag, Opts).

enable_html_console(Bool) :-
    modify_write_options(Bool, answer_write_options),
    modify_write_options(Bool, debugger_write_options),
    set_prolog_flag(html_term, Bool).

:- enable_html_console(true).


                /*******************************
                *           MESSAGES           *
                *******************************/
:- multifile
    prolog:message//1.

prolog:message(trace_help_table(Entries)) -->
    help_table(Entries).
prolog:message(wasm(consult_user)) -->
    [ ansi(code, '?- [user].', []), ' is not supported in the browser', nl,
      'version. Please use the scratch.pl file or create a new file.'
    ].

help_table([]) ==>
    [].
help_table([H1]) ==>
    help_entry(H1, 1).
help_table([H1,H2|T]) ==>
    help_entry(H1, 1),
    help_entry(H2, 40),
    [nl],
    help_table(T).


help_entry(Action-Keys, Column) ==>
    { maplist(key_name, Keys, Keys1),
      atomics_to_string(Keys1, ", ", Key),
      HelpCol is Column+12
    },
    [ '~t~*|'-[Column],  ansi([bold, fg(black)], '~w', Key),
      '~t~*|'-[HelpCol]
    ],
    action_help(Action).

key_name(' ', 'SPC') :- !.
key_name('Enter', 'RET') :- !.
key_name(Key, Key).

action_help(abort)   ==> ['Abort query'].
action_help(creep)   ==> ['Step to next port'].
action_help(goals)   ==> ['Print backtrace'].
action_help(help)    ==> ['Show this menu'].
action_help(leap)    ==> ['Continue to next spy point'].
action_help(listing) ==> ['List current predicate'].
action_help(nodebug) ==> ['Continue without debugging'].
action_help(retry)   ==> ['Retry current goal'].
action_help(skip)    ==> ['Step over current goal'].
action_help(up)      ==> ['Step out current goal'].
action_help(A)       ==> ['~w'-[A]].
