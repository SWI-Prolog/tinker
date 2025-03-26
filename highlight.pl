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
    (   CV = Info.get(current_variable)
    ->  Options = [current_variable(CV)]
    ;   Options = []
    ),
    format(atom(SourceId), 'edit:~w', [File]),
    setup_call_cleanup(
        open_string(Text, In),
        prolog_colourise_term(In, SourceId, colour_item(Offset, Source), Options),
        close(In)).

colour_item(Offset, Source, range, Start, Len) :-
    !,
    From is Start+Offset,
    To   is From+Len,
    _ := Source.clearMarks(From, To).
colour_item(Offset, Source, Class, Start, Len) :-
    TheStart is Start+Offset,
    colour_item(Source, Class, TheStart, Len).

%!  highlight_all

highlight_all :-
    Source := tinker.source,
    highlight_all(Source).

highlight_all(Source) :-
    Text := Source.getValueAsPrologString(),
    File := Source.files.current,
    format(atom(SourceId), 'edit:~w', [File]),
    setup_call_cleanup(
        open_string(Text, In),
        ( xref_editor(SourceId, In),
          prolog_colourise_stream(In, SourceId, colour_item(Source))
        ),
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
class_css(goal(recursion,_),    "cm-goal_recursion", #{title:"Recursive call"}).
class_css(goal(undefined,_),    "cm-goal_undefined", -).
class_css(goal(dynamic(_),_),   "cm-goal_dynamic", -).
class_css(goal(imported(_File),_), "cm-goal_imported", -).
class_css(head(exported, _),    "cm-head_exported", -).
class_css(head(public(_Line),_),"cm-head_public",
          #{title:"Public predicates may be called externally"}).
class_css(head(unreferenced,_), "cm-head_unreferenced", -).
class_css(head(local(_Line),_),  "cm-head", -).
class_css(function,		"cm-function", -).
class_css(no_function,		"cm-no_function", -).
class_css(module(_Module),	"cm-module", -).
class_css(nofile,               "cm-nofile", -).
class_css(file(_Path),          "cm-file", -).
class_css(file_no_depend(_Path),"cm-file_no_depends",
          #{title:"Imported file resolves no dependencies"}).
class_css(singleton,		"cm-singleton",
         #{title:"Singleton variable.  Use _ or _Name"}).
class_css(current_variable,	"cm-current_var", -).
class_css(syntax_error(Msg,_Range), "cm-syntax_error", #{title:Msg}).
class_css(dict_tag,		"cm-identifier", -).
class_css(identifier,		"cm-identifier", -).
class_css(keyword(_Kwd),        "cm-keyword", -).
class_css(decl_option(_Option), "cm-decl_option", -).
class_css(dict_key,		"cm-identifier", -).
class_css(option_name,		"cm-option_name", -).
class_css(no_option_name,	"cm-no_option_name", -).
class_css(flag_name(_Name),	"cm-flag_name", -).
class_css(no_flag_name(_Name),	"cm-no_flag_name", -).
class_css(type_error(Type),	"cm-error", #{title:Title}) :-
    format(string(Title), 'Expected ~p', [Type]).
class_css(op_type(_),		"cm-op_type", -).
class_css(qq(open),		"cm-qq_open", -).
class_css(qq(sep),		"cm-qq_sep", -).
class_css(qq_type,		"cm-qq_type", -).
class_css(qq_content,		"cm-qq_content", -).

clear_highlight :-
    _ := tinker.source.clearMarks().


                /*******************************
                *      CROSS REFERENCING       *
                *******************************/

:- multifile
    prolog:xref_source_identifier/2.

xref_editor(SourceId, Stream) :-
    stream_property(Stream, position(Pos)),
    call_cleanup(
        xref_source(SourceId,
                    [ silent(true),
                      stream(Stream)
                    ]),
        set_stream_position(Stream, Pos)).

prolog:xref_source_identifier(Id, Id) :-
    sub_atom(Id, 0, _, _, 'edit:').
