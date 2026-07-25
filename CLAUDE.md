# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

SWI-Tinker: a SWI-Prolog playground that runs entirely in the browser on top of
SWI-Prolog compiled to WASM (Emscripten).  There is no server-side execution and
no build step for this repo — it is plain ES modules, CSS and Prolog files that
are served statically.  Public demo: https://wasm.swi-prolog.org/wasm/tinker

The code is also intended to be reusable: `tinker.js` exports classes
(`Console`, `Query`, `Source`, `Editor`, `Input`, `Persist`, `Tinker`) for
embedding a Prolog REPL in any page; `tinker.html` is one example composition of
them.

## Running it

The files must be served by a web server that also serves the WASM build of
SWI-Prolog under `/wasm/` (`swipl-bundle.js`, `swipl-web.js`, the library
`/wasm/swipl/...`).  The usual setup is the demo server from swipl-devel, which
picks up this checkout through the symlink
`swipl-devel/src/wasm/demos/tinker -> this repo`:

    cd ~/src/swipl-devel/build.wasm
    swipl ../src/wasm/server.pl        # --port=N, default 8080
    # browse to http://localhost:8080/wasm/tinker

`server.pl` serves `tinker.html` with the COOP/COEP headers Emscripten needs.
Reloading the page picks up edits to `.js`, `.css`, `.pl` and the examples —
no build or restart is needed.  Prolog sources of Tinker itself (`tinker.pl`,
`highlight.pl`) are `Prolog.consult()`ed over HTTP from the same directory.

There is **no test suite** (`npm test` is a stub).  Changes are verified by
loading the page and exercising the affected interaction.

JSDoc for `tinker.js` (untracked output in `doc/`):

    make jsdoc          # jsdoc -c jsdoc.json -t ./node_modules/ink-docstrap/template
    make watch          # browser-sync on doc/ + rebuild on save (needs inotifywait)

## Architecture

Two halves talk to each other continuously; changes usually touch both.

**JavaScript side (`tinker.js`, ~2700 lines, one module, class per component).**
Each instance owns one HTML element, reachable as `inst.elem`; the reverse
mapping is `elem.data.instance`.  Module-global `Module` (the SWIPL WASM module)
and `Prolog` (`Module.prolog`) are initialised once by the `Tinker` constructor.

- `Tinker` — wires the components to the DOM in `div.content`, consults
  `tinker.pl` into module `system` and calls `tinker:tinker_init/1`.  Prolog
  startup and editor startup are two independent async chains (the editor comes
  from a CDN); `Tinker.prologReady` (static) and `Source.ready` are joined into
  `Tinker.ready`.  With the `preload` option, `Tinker.preload()` then loads the
  files named in the page's query string (`?<url>`, `?url=`, `?code=`) and
  consults them.
- `Console` — owns the stack of `Query` elements, output printing (`print(line,
  cls, sgr)` from the WASM `on_output` hook), history, tty size, tty hyperlinks.
- `Query` — the heart of the system: one Prolog query's whole life cycle.  It
  calls `Prolog.call(tinker_run(this, user:Line), {async:true, debugger:true})`
  and then loops in `next()`/`__next()` over the *yield* codes: asking for more
  answers, debugger interaction, `read/1`, `get_char/1`, line input.  Its
  display state lives as a CSS class on the element (`state_classes`:
  run/more/trace/read/prompt/query/term/line), which is what `tinker.css` keys
  off to show the right controls.  Queries may run in a `Prolog.Engine`
  (`run(line, true)` = temporary engine), which is how concurrent/async queries
  work.
- `Source` + `Editor` — right pane: file `<select>`, (re)consult, up/download,
  and a CodeMirror 5 instance loaded at runtime via require.js from CDN
  (mode/theme `prolog` come from swi-prolog.org).  Files live in the WASM
  virtual FS under `/prolog` (`user_dir`), default `/prolog/scratch.pl`.
  `Source.addFile()` (and `fetchFile`/`addFileFromURL` on top of it) is the one
  path for getting a file into Tinker — demos, uploads and preloads all use it;
  `consultFiles()` injects the `consult/1` query.  `Source.ready` resolves after
  `afterEditor()` has restored the files and added the demos.
  A file that came from a URL keeps that URL in `files.origins` (path → URL,
  persisted alongside `files`); such a file is a *mirror*, lives under
  `mirror_dir` (`/prolog/web/<host>/<url path>`) and is consulted **under its
  URL**, not its local path.  `setOrigin`/`fileOrigin`/`mirrorFile`/`mirrorPath`/
  `addMirror`/`syncFile` manage this (the last three are called from
  `tinker.pl`); `displayName()` is what the file menu shows.
  Demo entries in the file menu come from `examples/index.json` and load from
  `/wasm/examples/<name>`.
- `Input` — the `<input>` embedded in a `Query`; behaviour depends on the query
  state (query / term / line / single char), plus tab completion.
- `Persist` — localStorage under prefix `/tinker/`: `history`, `files`,
  `file/<name>`.  Saved on `visibilitychange` to hidden.

**Prolog side.**

- `tinker.pl` (module `tinker`, consulted into `system`) — the toplevel glue.
  `tinker_run/2` binds the JS `Query` object to a global so any predicate can
  reach its own query via `tinker_query/1` (used by `cls/0`, `html/1`,
  `dump_var/2`).  It also: implements `prolog_edit:edit_source/1` so `edit/1`
  opens the editor at the right line, decodes tty hyperlinks (`tty_link/1`),
  drives the debugger UI (`trace_action/2`, source location display),
  `wrap_predicate/4`s the tty input predicates (`read/1`, `get_char/1`, ...) so
  they yield to the browser, wraps `absolute_file_name/3` to accept URLs, and
  enables HTML rendering of answer terms (`html_term` flag,
  `prolog:message_line_element/3`).  It also `asserta`s a
  `user:prolog_load_file/2` clause (`tinker_load_file/2`) that loads a mirrored
  file from the editor buffer but **under the identity of its origin URL**, so
  relative loads inside it resolve against the remote directory.  The `asserta`
  is essential: `library(wasm)`'s own clause is loaded first and would otherwise
  download the URL and skip the load as `already_loaded`.
- `highlight.pl` (module `highlight`) — semantic highlighting.  `Editor` calls
  `highlight:refresh_clause/2` for the clause around the caret on every change
  and `highlight:highlight_all/1` for the whole buffer on load and after ~2s
  idle (mirrors PceEmacs).  It runs `prolog_colourise_term/4` /
  `prolog_colourise_stream/3` + `prolog_xref` and calls back *into* JS
  (`Source.mark/3`, `Source.clearMarks/2`) with CodeMirror `cm-*` classes.
  `class_css/3` maps colour categories to those classes; `source.css` styles
  them.  Adding a highlight category means editing both.

**JS ↔ Prolog bridge** (from `library(wasm)`, see swipl-devel `src/wasm/`):
Prolog code manipulates the UI directly with `Var := Obj.method(Args)`, `#Value`
to force a JS value, and `#{k:V}` dicts for JS objects; JS calls Prolog with
`Prolog.call/query/forEach/consult`, `new Prolog.Compound(...)`,
`new Prolog.String(...)`.  Long-running or nested work uses `{engine:true}`.

**CSS split:** `tinker.css` = layout and query-state visibility; `term.css` =
interactive answer terms rendered by `term.js` (folding, menus, `pl-*` classes);
`source.css` = CodeMirror highlight classes produced by `highlight.pl`.

## Conventions

- Prolog predicates called only from JavaScript must be declared `:- public`
  (see the list at the top of `tinker.pl`) or they are flagged as unreferenced.
- Everything is loaded over HTTP relative to the page, so new Prolog or JS files
  must also be reachable from the served directory.
- `doc/`, `node_modules/` and `*~` are untracked build/editor artifacts.
- Commit message style follows SWI-Prolog: `ADDED:`, `FIXED:`, `ENHANCED:`,
  `COMPAT:` prefixes for user-visible changes.
- `TODO.md` tracks the intended direction; completed items are marked `[done]`.
