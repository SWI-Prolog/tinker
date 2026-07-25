# SWI-Tinker: SWI-Prolog in your browser

Public demo at https://wasm.swi-prolog.org/wasm/tinker

This repository implements SWI-Tinker, a SWI-Prolog playground running
in    your    browser    based   on    SWI-Prolog    compiled    using
[Emscripten](https://emscripten.org/)                               to
[WASM](https://webassembly.org/).

The  current   system  is  primarily  a   proof-of-concept.   You  are
encouraged to  help improving  it.  The  [TODO](TODO.md) file  in this
repo gives a list of possible improvements.

## Achieved functionality

  - Run SWI-Prolog in your browser
  - Basic REPL loop window
  - Basic editor support based on [CodeMirror](https://codemirror.net/)
  - Saves command history and programs to your browser _local store_
  - Allows uploading and downloading programs
  - Allows for loading (compiling) these programs as well a loading
    programs directly from the internet.
  - Load large programs quickly as `.qlf` files.
  - Support for a basic debugger using the common `?- trace, mygoal.`
    command.   Support for spy- and break-points.
  - Demonstrates _async_ support using Prolog _engines_.
  - Preload programs from the page URL (see below), which allows for
    linking to a ready-to-run program.
  - Edit and reload programs that are loaded from a URL, including
    programs that load their dependencies from the same site.

## Loading programs from the page URL

Adding a _query string_ to the Tinker  URL loads one or more programs
into the editor and consults them.  The accepted forms are

  - `?<url>` <br>
    The entire query string is a URL.  It is not split at `&`, so the
    URL may have a query string of its own.  For example

        https://wasm.swi-prolog.org/wasm/tinker?https://example.org/hello.pl

  - `?url=<url>` <br>
    URL to load.  May be repeated to load several programs.
  - `?code=<text>` <br>
    Prolog text to load.  May be repeated.  The text is loaded into a
    file named `code.pl`.
  - `?name=<file>` <br>
    Name for the file created by the preceding `code=`.

Both  relative and  absolute  URLs are  accepted.   Values of  `url=`
and `code=`  must be encoded  using JavaScript's `encodeURIComponent()`.
Note  that  this does  __not__  encode  `+`,  which we  therefore  do
__not__ interpret as a space.  Files  are added as normal user files,
overwriting a file  with the same name if that  exists.  Loading from
another  site requires  the server  to  allow this  using the  header
`Access-Control-Allow-Origin`.  Sites  such as GitHub (using  the raw
content URL) do so.  A `.qlf` file  is consulted from its URL and, as
it is not source, not shown in the editor.

Note that  loading a program also  runs its _directives_ and  thus an
untrusted link can run arbitrary Prolog code in your browser.

### Editing programs that came from a URL

A file that was loaded  from a URL remembers where it came  from.  The
file menu shows this  URL as a tooltip.  Such a file  is consulted
_using its URL_, e.g.

    ?- consult('https://example.org/dir/run.pl').

As a result,  `:- include(...)`, `:- ensure_loaded(...)`  and `library`
aliases inside the file are resolved   against `https://example.org/dir/`,
while  the text  that is  compiled is  the text  you see  in the  editor.
In other words,  you can edit a program that  was loaded from the web
and  reload it,  including programs  that load  their dependencies  from
the same site.

Clicking an  error location,  `edit/1` and the  debugger download  and
add a local copy of files  that were loaded indirectly, so you  can fix
and reload those too.  Your changes are __not__ written back to the
server.

The local copies are stored in  a directory structure that reflects the
URL, e.g.,   `https://example.org/dir/run.pl`   is  stored   in  the
browser's file  system as  `/prolog/web/example.org/dir/run.pl`.   This
way, a program and the files  it loads keep their relation and programs
from different sites cannot collide.

Programs that  read _data_ files  (e.g., using  `open/3`) rather  than
loading Prolog source do not work: the WASM version cannot open a URL
as a stream.

## Limitations and alternatives

SWI-Tinker is about two times slower than  native SWI-Prolog on the same
hardware. SWI-Tinker lacks  many  libraries   bundled  with  the  native
version, either for reducing the size or because required primitives are
lacking. It also lacks important features   of SWI-Prolog such as _multi
threading_ and access to a lot of  system   resources.  To get a list of
available and not-available libraries, run

    ?- check_installation.

Some alternatives for running Prolog in your browser are:

  - [SWISH](https://swish.swi-prolog.org) provides a server-based
    alternative, i.e., your queries are executed on a server. The
	SWISH environment is much more evolved, providing _notebooks_,
	file storage including version control, file sharing, etc.
	SWISH supports a different set of features.  Queries on SWISH
	are executed _stateless_ and are limited by a _sandbox_.
  - [Ciao playground](https://ciao-lang.org/playground/) provides
    a WASM based version of Ciao Prolog.
  - [Tau Prolog](http://tau-prolog.org/) provides a Prolog version
    completely written in JavaScript.

## Acknowledgements

Raivo Laanemets did most of the  ground work getting SWI-Prolog to run
using    WASM.     Jesse    Wright   provides    the    npm    package
[swipl-wasm](https://www.npmjs.com/package/swipl-wasm).       Torbjörn
Lager created the first version of SWISH.
