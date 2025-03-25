:- module(highlight,
          [ highlight_all/0,
            clear_highlight/0
          ]).
:- use_module(library(wasm)).
:- use_module(library(prolog_colour)).
:- use_module(library(prolog_xref)).

:- public
    refresh_all/0,
    refresh_clause/2.

refresh_all :-
    clear_highlight,
    highlight_all.

refresh_clause(Source, Info) :-
    _{file:File, text:Text, start_char:Offset} :< Info,
    format(atom(SourceId), 'edit:~w', [File]),
    setup_call_cleanup(
        open_string(Text, In),
        prolog_colourise_term(In, SourceId, colour_item(Offset, Source), []),
        close(In)).

colour_item(Offset, Source, Class, Start, Len) :-
    TheStart is Start+Offset,
    colour_item(Source, Class, TheStart, Len).

%!  highlight_all

highlight_all :-
    Source := tinker.source,
    highlight_all(Source).

highlight_all(Source) :-
    Text := Source.value,
    File := Source.files.current,
    format(atom(SourceId), 'edit:~w', [File]),
    xref_editor(SourceId, Text),
    setup_call_cleanup(
        open_string(Text, In),
        prolog_colourise_stream(In, SourceId, colour_item(Source)),
        close(In)).

colour_item(Source, Class, Start, Len) :-
    (   class_css(Class, CSSClass, Attrs)
    ->  mark(Source, Start, Len, CSSClass, Attrs)
    ;   true
    ).

mark(Source, Start, Len, CSSClass, -) =>
    End is Start+Len,
    _ := Source.mark(Start, End, #{className:CSSClass}).
mark(Source, Start, Len, CSSClass, Attrs) =>
    End is Start+Len,
    _ := Source.mark(Start, End, #{className:CSSClass, attributes:Attrs}).

class_css(goal(built_in,_),     "cm-goal_built_in", -).
class_css(goal(global(_,_),_),  "cm-goal_global", -).
class_css(goal(local(Line),G),  "cm-goal_local", #{title:Title}) :-
    pi_head(PI, G),
    format(string(Title), '~q is defined at line ~d', [PI, Line]).
class_css(goal(recursion,_),    "cm-goal_recursion", -).
class_css(goal(undefined,_),    "cm-goal_undefined", -).
class_css(head(unreferenced,_), "cm-head_unreferenced", -).
class_css(head(local(_Line),_),  "cm-head", -). %#{title:"zero"}).
class_css(nofile,               "cm-nofile", -).

clear_highlight :-
    _ := tinker.source.clearMarks().


                /*******************************
                *      CROSS REFERENCING       *
                *******************************/

:- multifile
    prolog:xref_source_identifier/2.

xref_editor(SourceId, Source) :-
    setup_call_cleanup(
        open_string(Source, In),
        xref_source(SourceId,
                    [ silent(true),
                      stream(In)
                    ]),
        close(In)).

prolog:xref_source_identifier(Id, Id) :-
    sub_atom(Id, 0, _, _, 'edit:').
