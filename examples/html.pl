% This demo illustrates how we can manipulate the output window.  The
% output is inserted into a <div> with `id="output"`.  Each query is a
% <div> with class `query-container` that holds the query and a series
% of <div> with class `answer`.  The current answer div is accessible
% through the function `current_answer()`.

:- use_module(library(http/html_write), [html//1, print_html/1, op(_,_,_)]).
:- use_module(library(dcg/high_order), [foreach//2]).
:- use_module(library(solution_sequences), [order_by/2]).

%!  cls
%
%   Clear the output window. The predicate `thinker_query/1` returns the
%   JavaScript element that represents the  running query. The read-only
%   property `TinkerQuery.console` returns the   TinkerConsole`Q,  which
%   implements `clear()`. Note that this predicate   is also provided by
%   Tinker itself.

cls :-
    tinker_query(Q),
    _ := Q.console.clear().

%!  html
%
%   Insert  HTML  into  the  current answer  using  SWI-Prolog's  HTML
%   generation infra structure.   For example:
%
%       ?- html(['Hello ', b(world)]).
%
%    Note that this predicate is also provided by Tinker itself.

:- html_meta
    html(html).

html(Term) :-
    phrase(html(Term), Tokens),
    with_output_to(string(HTML), print_html(Tokens)),
    Div := document.createElement("div"),
    Div.innerHTML := HTML,
    tinker_query(Q),
    _ := Q.answer.appendChild(Div).

%!  flag_table
%
%   Emit a table holding all current Prolog flags and their value.

flag_table :-
    html(\flag_table).

flag_table -->
    html(table(\foreach(order_by([asc(Name)], current_prolog_flag(Name, Value)),
                        html(tr([th(Name), td('~p'-[Value])]))))).
