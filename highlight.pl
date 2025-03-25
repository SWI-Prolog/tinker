:- module(highlight,
          [ highlight/0,
            clear_highlight/0
          ]).
:- use_module(library(wasm)).
:- use_module(library(prolog_colour)).
:- use_module(library(prolog_xref)).

refresh :-
    clear_highlight,
    highlight.

refresh_clause(Info) :-
    pp(Info).

highlight :-
    Source := tinker.source.value,
    File := tinker.source.files.current,
    format(atom(SourceId), 'edit:~w', [File]),
    xref_editor(SourceId, Source),
    setup_call_cleanup(
        open_string(Source, In),
        prolog_colourise_stream(In, SourceId, colour_item),
        close(In)).

colour_item(Class, Start, Len) :-
    (   class_css(Class, CSSClass, Attrs)
    ->  mark(Start, Len, CSSClass, Attrs)
    ;   true
    ).

mark(Start, Len, CSSClass, -) =>
    End is Start+Len,
    _ := tinker.source.mark(Start, End, #{className:CSSClass}).
mark(Start, Len, CSSClass, Attrs) =>
    End is Start+Len,
    _ := tinker.source.mark(Start, End, #{className:CSSClass, attributes:Attrs}).

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
