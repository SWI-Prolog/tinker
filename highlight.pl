:- module(highlight,
          [ highlight/0,
            clear_highlight/0
          ]).
:- use_module(library(wasm)).
:- use_module(library(prolog_colour)).

highlight :-
    Source := tinker.source.value,
    setup_call_cleanup(
        open_string(Source, In),
        prolog_colourise_stream(In, tinker, colour_item),
        close(In)).

colour_item(Class, Start, Len) :-
    (   class_css(Class, CSSClass)
    ->  mark(Start, Len, CSSClass)
    ;   true
    ).

mark(Start, Len, CSSClass) :-
    End is Start+Len,
    _ := tinker.source.mark(Start, End, #{className:CSSClass}).

class_css(goal(built_in,_),     "cm-goal_built_in").
class_css(goal(global(_,_),_),  "cm-goal_global").
class_css(goal(recursion,_),    "cm-goal_recursion").
class_css(goal(undefined,_),    "cm-goal_undefined").
class_css(head(unreferenced,_), "cm-head_unreferenced").
class_css(nofile,               "cm-nofile").

clear_highlight :-
    _ := tinker.source.clearMarks().
