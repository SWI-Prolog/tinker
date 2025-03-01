# SWI-Tinker: SWI-Prolog in your browser

Public demo at https://wasm.swi-prolog.org/wasm/tinker

This repository implements SWI-Tinker, a SWI-Prolog playground running
in    your    browser    based   on    SWI-Prolog    compiled    using
[Emscripten](https://emscripten.org/)                               to
[WASM](https://webassembly.org/).

The  current   system  is  primarily  a   proof-of-concept.   You  are
encouraged to help improving it.  The  TODO.md file in this repo gives
a list of possible improvements.

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

## Limitations and alternatives

SWI-Tinker is over 10 times slower  than native SWI-Prolog on the same
hardware.   SWI-Tinker lacks  many libraries  bundled with  the native
version, either for  reducing the size or  because required primitives
are lacking.  It  also lacks important features of  SWI-Prolog such as
_multi threading_ and  access to a lot of system  resources.  To get a
list of available and not-available libraries, run

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
