/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
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

/**
 * @file Run SWI-Prolog interactively in the browser
 * @author Jan Wielemaker
 *
 * @description
 * This  file  provides a  number  of  JavaScript classes  that  allow
 * building interactive Prolog applications in a web page.  Below is a
 * short introduction of the exported classes.
 *
 * ## Classes
 *
 *   - `Query` <br>
 *     A query manages a Prolog query.  It allows for entering and
 *     running a query.  It manages interaction (`read/1`, the
 *     Prolog debugger, asking for more answers, etc.) and collects
 *     both the answers and console output produced by running the
 *     query.
 *   - `Console`<br>
 *     A console manages a number of `Query` instances.
 *   - `Source`<br>
 *     The source instance manages files, and editing files.  It uses
 *     an instance of `Editor` to edit sources and an instance of
 *     `Persist` to persist files from the virtual WASM file system
 *     to the browser's localStorage.
 *   - `Editor`<br>
 *     Encapsulates an instance of [CodeMirror](https://codemirror.net/).
 *   - `Input`<br>
 *     Encapsulates an HTML `<input>` element and a prompt to read
 *     queries, terms and lines.  It is embedded in a `Query` instance
 *     and is sensitive to the query state.
 *   - `Persist`<br>
 *     Deals with saving files and the query history to the browser's
 *     localStorage.
 *   - `Tinker`<br>
 *     Simple small class that binds the various components together
 *     to establish [SWI-Tinker](https://wasm.swi-prolog.org/wasm/tinker)
 *
 * ## Design notes
 *
 * Except for class `Persist`, each instance controls an HTML element.
 * This element  can be  accessed using the  `.elem` property  and the
 * controlling instance  can be  found from `.data.instance`  from the
 * HTML element.
 */

		 /*******************************
		 *            GLOBALS           *
		 *******************************/

/**
 * WASM Module.  Instantiated by the `Tinker` constructor from
 * the `SWIPL` instantiation.
 * @type {SWIPL}
 */

let Module;			// The WASM module

/**
 * The one and only instance of class `Prolog` that is made available
 * from the `SWIPL` `Module` instance as `Module.prolog`.
 * @type {Prolog}
 */

let Prolog;			// The one and only Prolog instance

		 /*******************************
		 *            SOURCE            *
		 *******************************/

/**
 * Encapsulate the  right pane of  Tinker, providing the  editor, file
 * selector, (re)consult button and up/download buttons.
 */

export class Source {
  files;			// Current and available
  default_file;			// Use scratch file
  user_dir;			// Directory for user files
  select_file;			// File selector
  editor;			// Editor instance
  elem;				// The <form>
  persist;			// Persist instance
  con;				// Console instance

  /**
   * Create the Tinker source file  manager from an DOM structure that
   * contains the  various components.  Components are  found by name.
   * Most  components are  optional, not  providing the  functionality
   * when missing.
   *
   * @param {HTMLFormElement} elem Toplevel element used.
   * @param {object} [options] Options processed.
   * @param {string} [options.user_dir] Working directory where user
   * files are placed.  Defaults to `"/prolog"`.
   * @param {string} [options.default_file_name] Default (scratch)
   * file.  Defaults to `"scratch.pl"`
   * @param {Persist} [options.persist] Persistency manager.  Defaults
   * to `new Persist()`.
   * @param {Console} [options.console] Console into which to inject
   * queries.
   */
  constructor(elem, opts) {
    const self = this;
    this.elem = elem;
    elem.data = {instance: this};

    opts = opts||{};

    this.persist = opts.persist||new Persist();
    this.persist.source = this;
    this.con = opts.console;
    this.user_dir = opts.user_dir||"/prolog";
    this.default_file =
      `${this.user_dir}/${opts.default_file_name||"scratch.pl"}`
    this.files = { current: this.default_file,
		   list: [this.default_file]
		 };
    this.select_file = this.byname("select-file");

    Module.FS.mkdir(this.user_dir);

    this.editor = new Editor(this.byname("editor"),
				   () => self.afterEditor());
    // Register event handling
    this.armFileSelect();
    this.armNewFileButton();
    this.armFileCreateButton();
    this.armDeleteFile();
    this.armDownloadButton();
    this.armUploadButton();
    this.armConsult();
  }

  afterEditor() {
    this.persist.restore();
    this.addExamples();
  }

  set value(source)   { this.editor.value = source; }
  get value()         { return this.editor.value; }
  goto(line, options) { this.editor.goto(line, options); }


  /**
   * Add the examples to the file selector.  This is not ideal as
   * the standard HTML `<select>` does not allow for styling.
   */
  async addExamples() {
    const json = await fetch("examples/index.json").then((r) => {
      return r.json();
    });

    if ( Array.isArray(json) && json.length > 0 ) {
      const select = this.select_file;
      const sep = document.createElement("option");
      sep.textContent = "Demos";
      sep.disabled = true;
      select.appendChild(sep);

      json.forEach((ex) => {
	if ( !this.hasFileOption(select, this.userFile(ex.name)) ) {
	  const opt = document.createElement("option");
	  opt.className = "url";
	  opt.value = "/wasm/examples/"+ex.name;
	  opt.textContent = (ex.comment||ex.name) + " (demo)";
	  select.appendChild(opt);
	}
      });
    }
  }

  /**
   * Add name to the file menu.
   * @param {string} name is the absolute name of the file that
   * is stored in the `value` attribute of the `<option>` element.
   * @return {HTMLElement} holding the `<option>` element.
   */
  addFileOption(name) {
    const select = this.select_file;
    let node = this.hasFileOption(name);

    if ( !node )
    { node = document.createElement('option');
      node.textContent = this.baseName(name);
      node.value = name;
      node.selected = true;
      const sep = this.demoOptionSep();
      if ( sep )
	select.insertBefore(node, sep);
      else
	select.appendChild(node);
    }

    return node;
  }

  /**
   * Switch the source view to a specific file.  This updates the file
   * selector, saves the old file and loads the new file.
   * @param {string} name is the full path of the file to switch to.
   */
  switchToFile(name) {
    const options = Array.from(this.select_file.childNodes);

    options.forEach((e) => {
      e.selected = e.value == name;
    });

    if ( this.files.current != name ) {
      this.ensureSavedCurrentFile();
      this.files.current = name;
      if ( !this.files.list.includes(name) )
	this.files.list.push(name);
      this.persist.loadFile(name);
      this.updateDownload(name);
    }
  }

  /**
   * Delete a  file from  the menu,  the file  system and  the browser
   * localStorage.  The source view switches  to the next file, unless
   * this is the last.  In that case it switches to the previous.
   *
   * @param {string} file is the file to be deleted.
   */

  deleteFile(file) {
    const select = this.select_file;
    const opt = this.hasFileOption(file);
    let to = opt.nextElementSibling;
    const sep = this.demoOptionSep();
    if ( !to || to == sep )
      to = opt.previousElementSibling;
    if ( !to )
      to = default_file;
    this.switchToFile(to.value);
    opt.parentNode.removeChild(opt);
    this.files.list = this.files.list.filter((n) => (n != file));
    this.persist.removeFile(file);
    Module.FS.unlink(file);
  }

  currentFileOption() {
    return this.select_file.options[this.select_file.selectedIndex];
  }

  hasFileOption(name) {
    return Array.from(this.select_file.childNodes)
                .find((n) => n.value == name );
  }

  demoOptionSep() {
    return Array.from(this.select_file.childNodes)
                .find((n) => n.textContent == "Demos" && n.disabled);
  }

  armFileSelect() {
    this.select_file.addEventListener("change", (e) => {
      const opt = this.currentFileOption();

      if ( opt.className == "url" ) {
	fetch(opt.value)
	  .then((res) => res.text())
	  .then((s) => {
	    const name = this.baseName(opt.value);
	    opt.className = "local";
	    opt.value = this.userFile(name);
	    opt.textContent = name;
	    Module.FS.writeFile(opt.value, s);
	    this.switchToFile(opt.value);
	  });
      } else
      { this.switchToFile(opt.value);
      }
    });
  }

  armNewFileButton() {
    const btn = this.byname("new-file");
    btn.addEventListener("click", (e) => {
      const fname = this.byname("file-name");
      e.preventDefault();
      this.elem.classList.add("create-file");
      e.target.disabled = true;
      fname.value = "";
      fname.focus();
    });
  }

  armFileCreateButton() {
    const btn = this.byname("create-button");
    const input = this.byname("file-name");

    input.addEventListener("keydown", (e) => {
      if ( e.key === "Enter" )
	btn.click();
    });

    btn.addEventListener("click", (e) => {
      e.preventDefault();
      let name  = input.value.trim();

      if ( /^[a-zA-Z 0-9.-_]+$/.test(name) )
      { if ( ! /\.pl$/.test(name) )
	name += ".pl";

	name = this.userFile(name);
	this.addFileOption(name);
	this.switchToFile(name);
	this.elem.classList.remove("create-file");
	this.byname("new-file").disabled = false;
      } else
      { alert("No or invalid file name!");
      }
    });
  }

  armDeleteFile() {
    const btn = this.byname("delete-file");
    if ( btn ) {
      btn.addEventListener("click", (e) => {
	e.preventDefault();
	const del = this.currentFileOption().value;

	if ( del == this.default_file )
	{ alert("Cannot delete the default file");
	  return;
	}
	if ( !this.isUserFile(del) )
	{ alert("Cannot delete system files");
	  return;
	}
	this.deleteFile(del);
      });
    }
  }

  /**
   * Update the  title and  download location  of the  download button
   * after switching files.
   */
  updateDownload(file) {
    const btn = this.elem.querySelector("a.btn.download");
    if ( btn ) {
      file = this.baseName(file);
      btn.download = file;
      btn.title = `Download ${file}`;
      btn.href = "download";
    }
  }

  armDownloadButton() {
    const btn = this.elem.querySelector("a.btn.download");
    if ( btn ) {
      btn.addEventListener("click", (ev) => {
	const text = this.value;
	const data = new Blob([text]);
	const btn = ev.target;
	btn.href = URL.createObjectURL(data);
      })
    }
  }

  async download_files(files) {
    for(let i=0; i<files.length; i++) {
      const file = files[i];
      const content = await this.readAsText(file);
      const name = this.userFile(this.baseName(file.name));
      this.addFileOption(name);
      this.switchToFile(name);
      this.value = content;
      this.ensureSavedCurrentFile();
    }
  }

  armUploadButton() {
    const btn = this.elem.querySelector("a.btn.upload");
    if ( btn ) {
      btn.addEventListener("click", (ev) => {
	const exch = ev.target.closest("span.exch-files");
	if ( exch.classList.contains("upload-armed") ) {
	  const files = exch.querySelector('input.upload-file').files;
	  this.download_files(files).then(() => {
	    exch.classList.remove("upload-armed");
	  });
	} else {
	  exch.classList.add("upload-armed")
	}
      });
    }
  }

  ensureSavedCurrentFile() {
    const file = this.files.current;
    if ( file ) {
      if ( this.isUserFile(file) )
	Module.FS.writeFile(file, this.value);
      return file;
    }
  }

  /**
   * Arm the form submit button.
   */

  armConsult() {
    const btn = this.byname("consult");
    if ( btn )
      btn.addEventListener('click', (e) => {
	e.preventDefault();
	const file = this.ensureSavedCurrentFile();
	this.con.injectQuery(`consult('${file}').`); // TODO: quote
      }, false);
  }

  /**
   * @return {Promise} for handling the content of an uploaded file.
   */
  readAsText(file) {
    return new Promise((resolve, reject) => {
        const fr = new FileReader();
        fr.onerror = reject;
        fr.onload = () => {
            resolve(fr.result);
        }
        fr.readAsText(file);
    });
  }

  /**
   * @return {string}  full path  name of  a user  file from  the base
   * name.
   */
  userFile(base) {
    return `${this.user_dir}/${base}`;
  }

  /**
   * @return {bool} `true` when `file` is a _user file_.
   */
  isUserFile(file) {
    return file.startsWith(`${this.user_dir}/`);
  }

  baseName(path) {
    return path.split("/").pop();
  }

  /**
   * Find one of my components by its name.
   * @param {string} name Name of the component to find.
   * @return {HTMLElement} Element with that name;
   */
  byname(name) {
    return this.elem.querySelector(`[name=${name}]`);
  }
} // end class Source


		 /*******************************
		 *      THE SOURCE EDITOR       *
		 *******************************/

/**
 * Encapsulate  the  editor.   In  this  case  the  actual  editor  is
 * [CodeMirror](https://codemirror.net/).  Defines methods to
 *
 *   - Initialise the editor
 *   - Set and get its value {@link Editor#value}
 *   - Go to a line/column {@link Editor#goto}
 *
 * By default, the editor instance is created by {@link Source} on the
 * element named `editor` that must be part of the DOM used to create
 * the {@link Source} instance.
 *
 * The CodeMirror code is downloaded from cdnjs.cloudflare.com and the
 * Prolog mode from www.swi-prolog.org.
 */

export class Editor {
  static cm_url = "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.9";
  static cm_swi = "https://www.swi-prolog.org/download/codemirror";
  CodeMirror;

  /**
   * @param {HTMLDivElement} container Element in which to create the editor
   * @param {function} cont Call-back called when the editor is completed.
   */
  constructor(container, cont) {
    const instance = this;

    function cm_url(sub) {
      return Editor.cm_url + sub;
    }
    function cm_swi(sub) {
      return Editor.cm_swi + sub;
    }

    require.config({ paths: {
      "cm/lib/codemirror":           cm_url("/codemirror.min"),
      "cm/addon/edit/matchbrackets": cm_url("/addon/edit/matchbrackets.min"),
      "cm/mode/prolog":              cm_swi("/mode/prolog")
    }});

    require(["cm/lib/codemirror",
	     "cm/addon/edit/matchbrackets",
	     "cm/mode/prolog/prolog",
	     "cm/mode/prolog/prolog_keys",
	    ], (cm) => {
	      this.CodeMirror = cm;
	      this.createCM(container);
	      cont.call(this.cm);
	    });

    loadCSS(cm_swi("/theme/prolog.css"));
  }

  createCM(container) {
    this.cm = this.CodeMirror(container,
			      { lineNumbers: true,
				matchBrackets: true,
				mode: "prolog",
				theme: "prolog",
				prologKeys: true
			      });
  }

  /**
   * Content of the editor
   * @type {string}
   */
  get value() {
    return this.cm.getValue();
  }
  set value(content) {
    this.cm.setValue(content);
  }

  /**
   * Go to a given 1-based line number.  The target line is styled
   * in CSS using the `.CodeMirror-search-match` selector.
   *
   * @param {number} line
   * @param {Object} [options]
   * @param {number} [options.linepos] Go to a specific column
   */

  goto(line, options) {
    options  = options||{};
    const ch = options.linepos||0;

    function clearSearchMarkers(cm)
    { if ( cm._searchMarkers !== undefined )
      { for(let i=0; i<cm._searchMarkers.length; i++)
	cm._searchMarkers[i].clear();
	cm.off("cursorActivity", clearSearchMarkers);
      }
      cm._searchMarkers = [];
    }

    clearSearchMarkers(this.cm);
    line = line-1;

    this.cm.setCursor({line:line,ch:ch});
    this.cm._searchMarkers.push(
      this.cm.markText({line:line, ch:0},
		  {line:line, ch:this.cm.getLine(line).length},
		  { className:"CodeMirror-search-match",
		    clearOnEnter: true,
		    clearWhenEmpty: true,
		    title: "Target line"
		  }));
    this.cm.on("cursorActivity", clearSearchMarkers);
  }
} // End class Editor

function getPromiseFromEvent(item, event) {
  return new Prolog.Promise((resolve) => {
    const listener = (ev) => {
      item.removeEventListener(event, listener);
      resolve(ev);
    }
    item.addEventListener(event, listener);
  })
}

		 /*******************************
		 *       CREATE ELEMENTS        *
		 *******************************/

function el(sel, ...content) {
  const ar   = sel.split(".");
  const elem = document.createElement(ar.shift());
  if ( ar.length > 0 )
    elem.className = ar.join(" ");
  for(let e of content) {
    if ( typeof(e) === "string" )
      e = document.createTextNode(e);
    elem.appendChild(e);
  }
  return elem;
}

function loadCSS(url)
{ const link = document.createElement("link");
  link.type = "text/css";
  link.rel = "stylesheet";
  link.href = url;
  document.head.appendChild(link);
}

		 /*******************************
		 *       OUTPUT STRUCTURE       *
		 *******************************/

/**
 * A Tinker `Console` is scrollable area that manages a list of {@link
 * Query} instances.  It provides the following functionality:
 *
 *   - Capture Prolog's console output using {@link Console#print}
 *   - Manage queries using {@link Console#addQuery} and {@link
 *     Console#injectQuery}
 *   - Manage and persist the query history as used by {@link Input},
 *     the input field of {@link Query}
 *   - Misc methods such as {@link Console#tty_size}, {@link Console#clear},
 *     etc.
 */

export class Console {
  /** Element for output and {@link Query} instances
   * @type {HTMLDivElement} */
  output;			// element to write in
  history;			// Query history
  persist;			// (History) persistency

  /**
   * Create a Tinker console.  The console is created from an `<div>`
   * element.  The constructor realizes terminal-style automatic
   * scrolling and adds a `<div.tinker-console-output>` that is
   * accessible through {@link Console#output} to which console output
   * (Prolog output to `current_output` and `current_error`) is
   * appended and to which instances of {@link Query} are added.
   *
   * @param {HTMLDivElement} elem Element that is wrapped to
   * provide a scrollable area and prepared for adding
   * instances of `Query`.
   * @param {object} [options]
   * @param {Persist} [options.persist] `Persist` instance used to load
   * the command line history.   When omitted, the history is not saved.
   */
  constructor(elem, options) {
    this.elem = elem;
    elem.classList.add("tinker-console");
    elem.data = {instance: this};

    options = options||{};
    this.persist = options.persist;

    this.output = el("div.tinker-console-output");
    const wrapper = el("div.tinker-console-wrapper",
		       el("span.tinker-scroll-start-at-top"));
    elem.parentNode.insertBefore(wrapper, elem);
    wrapper.appendChild(elem);
    elem.appendChild(this.output);
    this.#initHistory();
  }

  #initHistory() {
    this.history = {
      stack: [],
      current: null
    };
    if ( this.persist )
      this.persist.load("history", this.history, ["stack"]);
  }

  /**
   * Add a  new {@link Query}.  This  presents a  prompt.  After the  query is
   * entered, it  will be executed in  the context of this  query.  If
   * there is already a query with read state, select that.
   * @param {object} [options]
   * @param {bool} [options.focus] If `false`, do not focus the new query
   * @param {string} [options.placeholder] Placeholder for the query.
   */

  addQuery(options) {
    options = options||{};
    const open = this.elem.querySelector("div.tinker-query.read.query");
    if ( open ) {
      if ( options.focus !== false )
	open.data.instance.input.focus("query");
    } else {
      const q = new Query();
      this.output.appendChild(q.elem);
      q.read(options);
    }
  }

  /**
   * Inject a  query.  This is used  by e.g., the consult  button.  If
   * the last query  is open and empty, use that.   Otherwise inject a
   * query before the last.
   *
   * @param {string} query query to inject.
   */
  injectQuery(query) {
    const open = this.lastQuery();

    if ( open && open.input.target == "query" ) {
      if ( !open.input.value.trim() ) {
	open.run(query);
      } else {
	const q = new Query();
	open.elem.parentNode.insertBefore(q.elem, open.elem);
	q.run(query);
      }
    } else {
      Prolog.call(query);
    }
  }

  /**
   * {@link Query} instance associated with the currently running
   * query.  This can be accessed inside Tinker using the Prolog
   * predicate `tinker_query/1`.  It is also used by {@link
   * Console#print} through {@link Console#currentAnswer} to determine
   * the query into which to insert the output.
   * @return {Query|undefined}
   */
  currentQuery() {
    const e = Prolog.current_engine();
    if ( e )
      return e.current_query;
  }

  /**
   * Bottom-most {@link Query} instance displayed on this console.
   * @return {Query|undefined}
   */
  lastQuery() {
    const q = this.output.lastChild;
    while(q) {
      if ( q.classList.contains("tinker-query") && q.data )
	return q.data.instance;
      q = q.previousElementSibling;
    }
  }

  /**
   * `<div>` into which the current answer must be written.  This is
   * the last `<div.query-answer>` of the {@link Console#currentQuery}.
   * New answer `<div>` elements are added by {@link Query#nextAnswer}.
   * @return {HTMLDivElement|undefined}
   */
  currentAnswer() {
    let q;
    if ( (q=this.currentQuery()) )
      return q.answer;
  }

  #getCharSize(element) {
    if ( !element.data )
      element.data = {};
    if ( !element.data.char_size ) {
      let temp = document.createElement("span");
      temp.className = "stdout";
      temp.textContent = "test";
      element.appendChild(temp);
      const rect = temp.getBoundingClientRect();
      element.data.char_size = { h: rect.height,
				 w: rect.width/4
			       };
      element.removeChild(temp);
    }
    return element.data.char_size;
  }

  /**
   * Determine the size in (character) rows and columns of the
   * console.  This is used by the Prolog predicate `tty_size/2`.
   * @return {Array|undefined} holding [rows, columns]
   */
  tty_size() {
    if ( this.output ) {
      const charsz = this.#getCharSize(this.output);
      const wrapper = this.elem.closest("div.tinker-console-wrapper");
      if ( charsz && wrapper )
	return [ Math.floor(wrapper.clientHeight/charsz.h),
		 Math.floor(wrapper.clientWidth/charsz.w)
	       ];
    }
  }

  /**
   * Clear the console.  This removes all output and all completed
   * queries from the console.  Uncompleted queries not touched.
   */
  clear() {
    const rem = [];
    for(let node of this.output.childNodes) {
      if (  node.classList.contains("tinker-query") &&
	   !node.classList.contains("complete") )
	continue;
      rem.push(node);
    }
    for(let node of rem)
      node.remove();
  }

  /**
   * Print a string to the console.  This method can be called from
   * a function registered using the `on_output` method of the `SWIPL`
   * WASM module.  The methods creates a `<span>` element who's class
   * is the `cls` parameter.  Element style is attached to deal with
   * color, bold and underlining.  Finally, if the print is associated
   * to a {@link Query}, the output is added to the current answer of
   * the query.  Otherwise it is added to the {@link Console#output}
   * `<div>`
   *
   * @param {string} line content that must be printed
   * @param {string} cls class (stream), one of `"stdout"` or `"stderr"`
   * @param {object} [sgr] parsed ANSI sequence.  Currently provides
   * `color`, `background_color`, `bold`, `underline` or `link`.
   */
  print(line, cls, sgr, query) {
    query = query||this.currentQuery();
    if ( line.trim() == "" && query &&
	 query.answer_ignore_nl && query.answer ) {
      query.answer_ignore_nl = false;
      return;
    }

    let node;
    if ( sgr && sgr.link ) {
      node = document.createElement('a');
      node.href = sgr.link;
      node.target = "_blank";
      node.addEventListener("click", this.tty_link);
    } else {
      node = document.createElement('span');
      if ( sgr ) {
	if ( sgr.color )
	  node.style.color = sgr.color;
	if ( sgr.background_color )
	  node.background_color.color = sgr.background_color;
	if ( sgr.bold )
	  node.classList.add("bold");
	if ( sgr.underline )
	  node.classList.add("underline");
      }
    }
    node.classList.add(cls);
    node.textContent = line;

    let out;
    if ( query && query.answer )
      out = query.answer;
    else
      out = this.output;

    out.appendChild(node);
  }

  /**
   * Handle a click on a terminal hyperlink.  If we can trace the link
   * to one  of our  files, call `tinker:tty_link/1`.   Otherwise open
   * the link in a new tab.
   * @param {Event} ev is the click event on the `<a>` element
   */
  async tty_link(ev) {
    const a = ev.target;
    const to = a.href;
    if ( to.startsWith("file://") ||
	 to.match("https?://.*\\.pl\(#\d+\)?") )
    { ev.preventDefault();
      await Prolog.forEach("tinker:tty_link(Link)", {Link:to});
    }
    // Use default action
  }

  /**
   * Push an input line to the query history.  The line is _not_
   * added if it is identical to the last line.
   * @param {string} line Query to push to the history.
   */
  pushHistory(line) {
    if ( this.history.stack.length == 0 ||
	 this.history.stack[this.history.stack.length-1] != line )
      this.history.stack.push(line);
    this.history.current = null;
  }

  /**
   * Go up to older events in the history.
   * @param {string} line is the current content of the input line.
   * This is preserved if we move to previous history events, such
   * that it can be retrieved if we use {@link Console#downHistory}
   * to go back to the very start.
   * @return {string|undefined} Text to insert into the input
   */

  upHistory(line) {
    if ( this.history.current == null ) {
      this.history.saved = line;
      this.history.current = this.history.stack.length;
    }
    if ( --this.history.current >= 0 ) {
      return this.history.stack[this.history.current];
    }
  }

  /**
   * Go down to newer events in the history.
   * @return {string|undefined} Text to insert into the input
   */
  downHistory() {
    if ( this.history.current != null ) {
      if ( this.history.current+1 < this.history.stack.length ) {
	this.history.current++;
	return this.history.stack[this.history.current];
      } else if ( this.history.saved !== undefined ) {
	const val = this.history.saved;
	this.history.saved = undefined;
	return val;
      }
    }
  }

  /**
   * Find the console instance from a nested element
   * @return {Console|undefined}
   */
  static findConsole(from) {
    const elem = from.closest(".tinker-console");
    if ( elem && elem.data && elem.data.instance )
      return elem.data.instance;
  }
}

		 /*******************************
		 *         TINKER QUERY         *
		 *******************************/

// All possible states for a query.  Used to remove all states.
const state_classes = [
  "run", "more", "trace", "read", "prompt", "query", "term", "line"
];

/**
 * Enter and control a Prolog query life cycle.  The Tinker toplevel
 * adds an instance of this class to {@link Console}.  The query shows
 * itself as an input field using the `?-` prompt.  After the user
 * enters a Prolog query and hits _Enter_, the Query instance enters
 * the `run` state by calling {@link Query#run}.
 *
 * The  `Query` instance  manages the  Prolog execution  based on  the
 * return  state   from  `Prolog.call()`  using   {@link Query#next}.   If
 * executing the query _yields_, asking  for user input, class `Query`
 * handles  the interaction  and resumes  the Prolog  execution.  This
 * process continues  until a  final state  is reached  (final answer,
 * failure or error), after which {@link Query#completed} is called.
 */

export class Query {
  /**
   * Element we control
   * @type {HTMLDivElement}
   */
  elem;				// div.query-container
  /**
   * Element that receives the output for the next answer generated
   * by Prolog.
   * @type {HTMLDivElement|undefined}
   */
  answer;			// div.query-answer
  /**
   * Input field used to enter queries, terms and lines.
   * @type {Input}
   */
  input;			// Input
  /**
   * The Prolog _engine_ that runs this query.  It can be set using the
   * `options` of the constructor.  Running the query using {@link Query#run}
   * sets this member to the concrete Prolog engine associated with this
   * query.
   * @type {Prolog.Engine|boolean|undefined}
   */
  engine;			// Prolog engine
  #state;			// "run", "more", "trace",
				// "prompt query", "prompt term", "prompt line"
  /**
   * Create a `<div>` to interact with a new Prolog query.
   * The structure is
   *
   * ```
   * <div class="tinker-query">
   *   <div class="query-header">
   *     <span class="query-prompt">?-</span>
   *     <span class="query-goal">between(1,3,X).</span>
   *     <span class="query-buttons">...</span>
   *   </div
   *   <div class="query-answers">
   *     <div class="query-answer">
   *       <span class="stdout">X = 1</span>
   *     </div>
   *     <div class="query-answer">
   *       <span class="stdout">X = 2</span>
   *     </div>
   *     ... // more answers
   *   </div>
   *   <div> // controls (abort, next/stop, trace, ...)
   *     <div class="tinker-abort">...</div>
   *     ...
   *   </div>
   * </div>
   * ```
   * @param {string} [query] is the Prolog query to run.
   * @param {object} [options]
   * @param {Prolog.Engine|bool} [options.engine] Prolog engine on which
   * to run the query.  If `true`, a temporary engine is used that is
   * destroyed when the query is completed.
   */
  constructor(query, options) {
    options = options||{};
    const hdr  = el("div.query-header",
		    el("span.query-prompt", "?-"),
		    el("span.query-goal"));
    const ans  = el("div.query-answer");
    const ansl = el("div.query-answers", ans);
    const ctrl = el("div");

    this.elem = el("div.tinker-query",
		   hdr, ansl, ctrl);
    this.elem.data = { instance: this };

    this.#fillHeader(hdr);
    this.#fillControl(ctrl);

    this.engine = options.engine;
    this.query  = query;
    this.answer = ans;
  }

  /**
   * The query as it appears in the title
   * @type {string}
   */
  set query(query) {
    const span = this.elem.querySelector("span.query-goal");
    if ( query )
      span.textContent = query;
    else
      span.textContent = "";
  }
  get query() {
    const span = this.elem.querySelector("span.query-goal");
    return span.textContent;
  }

  /**
   * Console this query belongs to.
   * @type {Console}
   */
  get console() {
    return Console.findConsole(this.elem);
  }

  #fillHeader(hdr) {
    const self = this;
    const edit  = el("span", "\u270E");
    const close = el("span", "\u2715");
    const icon  = el("span.query-collapse");
    edit.title  = "Copy query to input";
    icon.title  = "Collapse/expand answer";
    close.title = "Close query";
    const btns  = el("span.query-buttons",
		     edit, icon, close);
    hdr.appendChild(btns);

    close.addEventListener("click", () => self.close(), false);
    edit.addEventListener("click", () => {
      const con = this.console;
      const open = con.lastQuery();
      if ( open && open.input.target == "query" ) {
	open.input.value = self.query;
	open.input.focus("query");
      }
    });
    icon.addEventListener("click", () => self.collapsed());
  }

  #fillControl(ctrl) {
    this.input = new Input();
    ctrl.appendChild(this.#createAbort());
    ctrl.appendChild(this.#createMore());
    ctrl.appendChild(this.#createTrace());
    ctrl.appendChild(this.#createKbd());
    ctrl.appendChild(this.input.elem);
  }

  #createAbort() {
    const btn = el("button", "Abort");
    const abort = el("div.tinker-abort", btn);

    btn.addEventListener("click", (e) => {
      e.preventDefault();
      if ( this.waitfor && this.waitfor.abort )
      { console.log("aborting", this.waitfor);
	this.waitfor.abort();
      } else
      { console.log("Requesting abort");
	this.abort_request = true;
      }
    });

    return abort;
  }

  /**
   * Fill the  input elements that  control user interaction  after an
   * answer has been found.
   */
  #createMore() {
    const self = this;
    const next = el("button.more-next", "Next");
    const stop = el("button", "Stop");
    const help = el("button", "?");
    const elem = el("div.tinker-more", next, stop, help);

    next.addEventListener("click", (ev) => {
      ev.preventDefault();
      self.replyMore("redo");
    });
    stop.addEventListener("click", (ev) => {
      ev.preventDefault();
      self.replyMore("continue");
    });
    help.addEventListener("click", (ev) => {
      ev.preventDefault();
      self.replyMore("help");
    });
    elem.addEventListener("keyup", (ev) => {
      if ( ev.defaultPrevented ) return;
      const action = more_shortcuts[ev.key];
      if ( action )
      { ev.preventDefault();
	ev.stopPropagation();
	self.replyMore(action);
      }
    });

    return elem;
  }

  /**
   * Activated after the query completes non-deterministically.  This
   * shows `<div.tinker-more>` control and focusses
   * `<button.more-next>` therein.  Activating one of these controls
   * calls {@link Query#replyMore}.
   */
  promptMore() {
    // this.elem.classList.add("more");
    this.state = "more";
    const btn = this.elem.querySelector("button.more-next");
    btn.focus();
  }

  #createTrace() {
    const self = this;
    function button(action, title, label) {
      const btn = el(`button.${action}`, label);
      btn.title = title;
      btn.addEventListener("click", () => {
	self.replyTrace(action);
      });
      return btn;
    }

    const trace = el("div.tinker-trace",
		     button("creep",   "Creep (c,Space,Enter)", "↳"),
		     button("skip",    "Skip (s)",              "⏭"),
		     button("retry",   "Retry (r)",             "↻"),
		     button("nodebug", "Nodebug (n)",           "▶"),
		     button("abort",   "Abort (a)",             "⏹"));

    trace.addEventListener("keyup", (ev) => {
      if ( ev.defaultPrevented ) return;
      const action = trace_shortcuts[ev.key];
      if ( action )
      { ev.preventDefault();
	ev.stopPropagation();
	self.replyTrace(action);
      }
    });

    return trace;
  }

  promptTrace() {
    this.state = "trace";
    const btn = this.elem.querySelector("button.creep");
    btn.focus();
  }

  #createKbd() {
    const div = el("div.tinker-keyboard",
		   "⌨️",
		   el("span", "waiting for a key"));
    div.tabIndex = 0;
    return div;
  }

  /**
   * Implements `get_single_char/1` by showing a keyboard icon and
   * waiting for the user to type a character.
   * @return {number} code of the key pressed.
   */
  async get_single_char() {
    const kbd = this.elem.querySelector("div.tinker-keyboard");
    if ( kbd ) {
      this.elem.classList.add("key");
      kbd.focus();
      const ev = await getPromiseFromEvent(kbd, "keyup");
      this.elem.classList.remove("key");
      return ev.keyCode;
    } else
      return -1;
  }

  promptLine(target) {
    this.state = "prompt "+target;
    this.input.focus(target);
  }

  /**
   * Set/clear.toggle the collapsed state of the query.
   * @param {bool} [how] If provided, set the collapsed state
   * accordingly.  If omitted, toggle the collapsed state.
   */
  collapsed(how) {
    if ( how === true )
       this.elem.classList.add("collapsed");
    else if ( how === false )
      this.elem.classList.remove("collapsed");
    else
      this.elem.classList.toggle("collapsed");
  }

  set state(state) {
    this.#state = state;
    this.elem.classList.remove(...state_classes);
    this.elem.classList.add(...state.split(" "));
  }

  get state() {
    return this.#state;
  }

  hasState(state) {
    return this.elem.classList.contains(state);
  }

  close() {			// TODO: What if not completed?
    console.log(this, this.elem);
    if ( this.hasState("complete") )
      this.elem.remove();
    else
      alert("Cannot close open query");
  }

  /**
   * Find the  query before this  one.  Currently, we do  not consider
   * the state of the query.  This is used to collapse the query before
   * the latest when the latest starts executing.
   * @return {Query|undefined}
   */
  previous() {
    let node = this.elem.previousElementSibling;
    while(node) {
      if ( node.classList.contains("tinker-query") )
	return node.data.instance;
      node = node.previousElementSibling;
    }
  }

  /**
   * Put the query in `"read query"` state.  In this mode it only
   * shows its {@link Input} element.  After the user submits a
   * Prolog query the Input element activates {@link Query#handleUserInput},
   * which in turn calls {@Query#run} to start Prolog.
   * @param {object} [options]
   * @param {bool}   [options.focus] If `false`, do not focus the
   * {@link Input} element.
   * @param {string} [options.placeholder] Set the placeholder for
   * the {@link Input} element.
   */

  read(options) {
    options = options||{};
    this.state = "read query";
    if ( options.focus !== false )
      this.input.focus("query");
    if ( options.placeholder )
      this.input.placeholder = options.placeholder;
  }

  /**
   * Run the query.
   * @param {string} line Prolog source for the query to execute
   * @param {Prolog.Engine|true} [engine] If provided run the query in
   * an engine.  If `true`, a temporary engine is used that is closed
   * after the query completes.
   */
  run(line, engine) {
    engine = engine||this.engine;

    if ( engine === true ) {
      const e = new Prolog.Engine({auto_close:true});
      this.engine = e;
      this.elem.classList.add("engine");
      e.with(() => this.__run(line));
      this.addNextQuery({focus:false,
			 placeholder:"Enter and run concurrent query"});
    } else if ( engine instanceof Prolog.Engine ) {
      this.engine = engine;
      this.elem.classList.add("engine");
      engine.with(() => this.__run(line));
    } else {
      this.__run(line);
    }
  }

  __run(line) {
    Prolog.current_engine().current_query = this;
    this.query = line;
    const jqline = new Prolog.Compound(":", "user", line);
    const jgoal  = new Prolog.Compound("tinker_run", this, jqline);
    const rc = Prolog.call(jgoal, { async:true, debugger:true });
    this.state = "run";
    const prev = this.previous();
    if ( prev && prev.hasState("complete") )
      prev.collapsed(true);
    this.next(rc);
  }

  /**
   * Handle the return of Prolog.call().  This is a success, failure,
   * error or a yield code.
   * @param {object} rc Result from {@link Prolog#call} using `async:true`.
   */

  next(rc) {
    const self = this;

    if ( rc.yield && this.engine instanceof Prolog.Engine ) {
      return this.engine.with(() => self.__next(rc));
    } else {
      return this.__next(rc);
    }
  }

  __next(rc)
  { this.waitfor = null;

    if ( rc.yield !== undefined ) {
      this.waitfor = rc;

      Prolog.flush_output();

      if ( this.abort_request ) {
	this.abort_request = false;
	return this.__next(rc.resume("wasm_abort"));
      }

      switch(rc.yield)
      { case "beat":
          return setTimeout(() => this.__next(rc.resume("true")), 0);
	case "query":
          this.answer = undefined;
          /*FALLTHROUGH*/
	case "term":
	case "line":
          this.promptLine(rc.yield);
          break;
	case "more":
          this.promptMore();
          break;
	case "trace":
	{ this.trace_action("print", rc.trace_event);
	  this.promptTrace();
          break;
	}
	case "builtin":
	{ rc.resume((rc) => this.__next(rc));
          break;
	}
      }
    } else {
      if ( rc.error ) {
	if ( rc.message == "Execution Aborted" ) {
	  if ( this.engine && !this.engine.open ) {
	    this.print("% Execution Aborted", "stderr", {color: "green"});
	  } else {
	    this.call("print_message(informational, unwind(abort))");
	  }
	} else
	{ console.log("Unhandled exception; restarting", rc);
	}
      }
      this.completed();
    }
  }

  /**
   * Called with the embedded {@link Input} element is submitted.
   * That action depends on {@link Query#state}.  Notably when in
   * `"read query"` mode, it uses {@link Query#run} to start the query
   * and when used as prompt for a Prolog call to `read/1`,
   * `get_code/1`, etc. it resumes the Prolog engine with the entered
   * line.
   *
   * @param {string} line Line entered by the user.
   * @param {Event} event is the event that trigged the submit.  This
   * can be used to check modifiers such as `event.shiftKey`.  Notably
   * a query entered with SHIFT is executed on a new engine.
   */
  handleUserInput(line, event) {
    switch(this.state)
    { case "read query":
      { this.run(line, event && event.shiftKey);
	break;
      }
      case "prompt term":
      case "prompt line":
      { this.state = "run";
	this.next(this.waitfor.resume(line));
	break;
      }
    }
  }

  /**
   * Add a `div.query-answer` element to capture the output and
   * solution of the next answer.
   */
  nextAnswer() {
    if ( this.answer ) {
      const ans = document.createElement("div");
      ans.className = "query-answer";
      this.answer.after(ans);
      this.answer = ans;
      this.answer_ignore_nl = true; // suppress the first newline
    }
  }

  /**
   * Handle the __Next/Stop__ buttons that are displayed after the
   * current query finishes non-deterministically.
   * @param {string} action Tells Prolog how to continue.  Currently
   * supports `"redo"` (Next), and `"continue"` (Stop).
   */
  replyMore(action) {
    if ( this.waitfor && this.waitfor.yield == "more" ) {
      const res = Prolog.query(
	"'$toplevel':more_command(Action, _Keys, Msg, Style)",
	{Action:action}, {nodebug:true, string:"atom"}).once();

      switch(res.Style)
      { case "bold":
	  this.print(res.Msg, "stdout", {bold:true});
	  break;
	case "comment":
	  this.print(res.Msg, "stdout", {color:"#0f0"});
	  break;
	case "warning":
	  this.print(res.Msg, "stdout", {color:"#800"});
	  break;
      }

      switch(action)
      { case "redo":
	{ this.nextAnswer();
	  break;
	}
	case "continue":
	{ this.answer_ignore_nl = true;
	  break;
	}
      }
      this.next(this.waitfor.resume(action));
    }
  }

  call(query, options) {
    if ( this.engine )
      return this.engine.with(() => Prolog.call(query, options));
    else
      return Prolog.call(query, options);
  }

  replyTrace(action) {
    if ( this.waitfor && this.waitfor.yield == "trace" ) {
      this.print(` [${action}]`, "stderr", {color: "#888"});
      this.call("nl(user_error)", {nodebug:true});

      switch(action)
      { case "goals":
	case "listing":
	case "help":
	{ this.trace_action(action, this.waitfor.trace_event);
	  break;
	}
	default:
	{ this.state = "run";
	  this.next(this.waitfor.resume(action));
	}
      }
    }
  }

  /**
   * Call tinker:trace_action(action, msg)
   * @param {string} action is the trace action to perform
   * @param {number} msg is the Prolog `term_t` handle holding the
   * trace event context.
   */
  trace_action(action, msg) {
    const self = this;
    if ( this.engine )
      return this.engine.with(() => self.__trace_action(action, msg));
    else
      return self.__trace_action(action, msg);
  }

  __trace_action(action, msg) {
    const prolog = Prolog;

    return prolog.with_frame(() => {
      const av = prolog.new_term_ref(2);

      prolog.put_chars(av+0, action, prolog.PL_ATOM);
      prolog.bindings.PL_put_term(av+1, msg);
      const flags = prolog.PL_Q_NODEBUG;
      const pred  = prolog.predicate("tinker:trace_action/2");
      const qid   = prolog.bindings.PL_open_query(0, flags, pred, av);
      const rc    = prolog.bindings.PL_next_solution(qid);
      prolog.bindings.PL_close_query(qid);
      return rc;
    });
  }

  /**
   * The query we are running has been completed.  This sets the
   * state to `"complete"` and calls {@link Query#addNextQuery}
   * to add a new query to the {@link Console} this query belongs to.
   */
  completed() {
    this.state = "complete";
    this.answer = undefined;
    this.addNextQuery();
  }

  /**
   * Calls {@link Console#addQuery} on the console to which this query
   * belongs.
   */
  addNextQuery(options) {
    this.console.addQuery(options);
  }

  /**
   * Calls {@link Console#print} on the console to which this query
   * belongs.
   */
  print(line, cls, sgr) {
    this.console.print(line, cls, sgr, this);
  }

  /**
   * Calls {@link Console#tty_size} on the console to which
   * this query belongs.
   * @return {Array|undefined}
   */
  tty_size() {
    const con = Console.findConsole(this.elem);
    if ( con )
      return con.tty_size();
  }
} // end class Query


		 /*******************************
		 *        ENTER A QUERY         *
		 *******************************/

/**
 * Handle term and line input for a {@link Query}.  This deals with
 * entering a query, read/1 and friends and reading a line of input.
 */

export class Input {
  /** Element controlled by this instance
   * @type {HTMLDivElement}
   */
  elem;
  /**
   * "Thing" we are requested to read.  One of `"query"`, `"term"`, or
   * `"line"`
   * @type {string}
   */
  target;

  /**
   * Create the HTML DOM and register the event handlers.  The
   * structure is
   * ```
   * <div class="tinker-input">
   *   <span class="prompt">?- </span>
   *   <input>
   * </div>
   * ```
   *
   * The constructor calls {@link Input#armInput} and {@link
   * Input#armCompletion} to register event handling.
   */
  constructor() {
    const input = el("input");
    const run = el("button.tinker-query-run",
		   el("span", "▶"));
    this.elem = el("div.tinker-input",
		   el("span.prompt", "?- "),
		   input, run);
    this.elem.data = { instance: this };
    input.type = "text";
    input.name = "tinker-input";
    input.autocapitalize = "none";
    input.autocomplete = "off"
    run.title = "Run query";
    this.armInput();
    this.armCompletion();
  }

  /**
   * Value (text) of the `<input>` element.
   * @type {string}
   */
  get value() {
    const input = this.elem.querySelector("input");
    return input.value;
  }

  set value(val) {
    const input = this.elem.querySelector("input");
    return input.value = val;
  }

  /**
   * Placeholder of the `<input>` element.
   * @type {string}
   */
  set placeholder(val) {
    const input = this.elem.querySelector("input");
    return input.placeholder = val;
  }

  /**
   * Read-only property returning the {@link Query} instance
   * this input belongs to.
   * @type {Query}
   */
  get query() {
    return this.elem.closest(".tinker-query").data.instance;
  }

  /**
   * Submit the input line.  If the `target` is `"query"` or `"term"`,
   * add a full stop if the input is not terminated by a full stop.
   * If the `target` is `"query"` and there is an associated {@link
   * Console}, push the line to the _history_.  Finally, call {@link
   * Query#handleUserInput} to make the {@link Query} object act on
   * the input.
   * @param {Event} event Event that triggered the submit.
   */
  submit(event) {
    let query = this.value;
    this.value = '';

    if ( this.target == "query" ||
	 this.target == "term" ) {
      if ( query.trim() == "" ) {
	return false;
      } else {
	if ( ! /\.\s*/.test(query) )
          query += ".";
      }

      if ( this.target == "query" ) {
	const con = Console.findConsole(this.elem);
	if ( con )
	  con.pushHistory(query);
      }
    }

    this.query.handleUserInput(query, event);
  }

  /**
   * focus the input element and set the prompt and placeholder based
   * in the requested target.  The prompt is requested from Prolog and
   * can be set using `prompt/2` or `prompt1/1`.
   * @param {string} target is one of `"query", `"term"` or `"line"`
   */
  focus(target) {
    const input  = this.elem.querySelector("input");
    const prompt = this.elem.querySelector("span.prompt");
    switch(target)
    { case "query":
      { prompt.textContent = "?- ";
	input.placeholder = "Please enter a query.  (use Shift-Enter to execute in new engine)";
	break;
      }
      default:
      { const s = Prolog.prompt_string(0)||"|: ";
	prompt.textContent = s;
	input.placeholder = `Please enter a ${target}`;
      }
    }
    input.focus();
    this.target = target;
  }

  /**
   * Handle allow  keys for  history and Enter  to submit  the current
   * input.  This method is called by the constructor.
   */
  armInput() {
    const input = this.elem.querySelector("input");
    input.addEventListener("keyup", (event) => {
      if ( event.defaultPrevented ) return;

      switch(event.key)
      { case "ArrowUp":
	case "ArrowDown":
	{ const con = Console.findConsole(this.elem);
	  let val;

	  if( event.key === "ArrowUp" )
	    val = con.upHistory(input.value);
	  else
	    val = con.downHistory(input.value);

	  if ( val !== undefined )
	    input.value = val;
	  break;
	}
	case "Enter":
	{ this.submit(event);
	  break;
	}
	default:
	return;
      }

      event.preventDefault();
    }, true);

    const run = this.elem.querySelector("button");
    run.addEventListener("click", (ev) => {
      this.submit(ev);
    });
  }

  /**
   * Enable Tab-based completion on the element.  This method is
   * called by the constructor.
   * @todo show possible completions in case there are multiple.
   */
  armCompletion() {
    const input = this.elem.querySelector("input");
    input.addEventListener("keydown", (event) => {
      if ( event.key == "Tab" ) {
	event.preventDefault();
	const caret  = input.selectionStart;
	const all    = input.value;
	const before = all.slice(0,caret);
	const after  = caret == all.length ? "" : all.slice(caret-all.length);

	function commonPrefix(words)
	{ let i = 0;

	  while(words[0][i] && words.every(w => w[i] === words[0][i]))
	    i++;
	  return words[0].slice(0, i);
	}

	function setCompletion(to, del)
	{ input.value = ( before.slice(0, before.length-del.length) +
			  to +
			  after );
	}

	const res = Prolog.query(
	  "tinker:complete_input(Before,After,Delete,Completions)",
	  {Before:before, After:after}).once();

	if ( res.Completions.length == 1 ) {
	  setCompletion(res.Completions[0], res.Delete.v);
	} else if ( res.Completions.length > 1 ) {
	  const common = commonPrefix(res.Completions);
	  if ( common.length > 0 )
	    setCompletion(common, res.Delete.v);
	}
      }
    });
  }
} // end class Input

		 /*******************************
		 *            TRACER            *
		 *******************************/

/**
 * Mapping from keys  to Prolog trace events.  This  is exported, such
 * that Prolog can use this to print the trace help commands.
 * @type {object}
 */

export const trace_shortcuts = {
  " ":     "creep",
  "Enter": "creep",
  "a":	   "abort",
  "c":     "creep",
  "g":	   "goals",
  "l":	   "leap",
  "L":	   "listing",
  "r":	   "retry",
  "s":	   "skip",
  "n":     "nodebug",
  "u":	   "up",
  "?":	   "help"
};

export const more_shortcuts = {
  "h": "help",
  "?": "help",
  ";": "redo",
  " ": "redo",
  "n": "redo",
  "r": "redo",
  ".": "continue",
  "a": "continue",
  "c": "continue",
  "*": "choicepoint",
  "w": "write",
  "p": "print",
  "+": "depth_inc",
  "-": "depth_dec"
};


		 /*******************************
		 *        PERSIST FILES         *
		 *******************************/

/**
 * Provide persistency of files and  query history using the browser's
 * `localStorage` facility.  localStorage keys are:
 *
 *  - `<prefix>/history`
 *  - `<prefix>/files`
 *  - `<prefix>/file/<name>`
 *
 *  File storage in  the WASM virtual file system  and its persistency
 *  in the localStorage is based  on communication with the associated
 *  {@link Source} instance.
 */

export class Persist {
  /**
   * The {@link Source} instance I'm associated with.  This is
   * set by the constructor of the {@link Source} class.
   * @type {Source}
   */
  source;			// Associated Source instance
  autosave;			// Save on exit
  map;				// key -> object
  prefix;			// Key prefix

  /**
   * @param {object} [options]
   * @param {bool} [options.autosave] If `true` (default), save state on
   * `visibilitychange` to `hidden` event.
   * @param {string} [options.prefix] Prefix for all keys.  Default is
   * `"/tinker/"`
   */
  constructor(options) {
    const self = this;

    options = options|{};
    this.autosave = options.autosave !== undefined ? options.autosave : true;
    this.prefix = options.prefix !== undefined ? options.prefix : "/tinker/";
    this.map = {};

    window.addEventListener("visibilitychange", () => {
      if ( document.hidden && this.autosave )
	self.persist();
    });
  }

  itemKey(name) {
    return `${this.prefix}${name}`;
  }

  fileKey(name) {
    return `${this.prefix}file${name}`;
  }

  /**
   * Load  `name` from  localStorage and  use its  attributes to  fill
   * `into`.  The object is registered such that it is saved back into
   * the localStorage when the page is left.
   *
   * @param {string} name the localStorage key
   * @param {object} into JavaScript object filled
   * @param {string[]} [keys] Keys of `into` that must be saved.
   * Defaults to the keys of `into`.
   */
  load(name, into, keys) {
    this.map[name] = { data: into,
		       keys: keys||Object.keys(into)
		     };
    const item = localStorage.getItem(this.itemKey(name));
    if ( item ) {
      const obj = JSON.parse(item);
      for(let k of keys) {
	if ( obj[k] !== undefined )
	  into[k] = obj[k];
      }
    }
  }

  /**
   * Save all objects registered with `load()` to the localStorage.
   */
  persistRegistered() {
    for(let k of Object.keys(this.map)) {
      const data = this.map[k].data;
      const save = {};
      for( let key of this.map[k].keys ) {
	save[key] = data[key];
      }
      localStorage.setItem(this.itemKey(k), JSON.stringify(save));
    }
  }

  removeFile(name) {
    localStorage.removeItem(this.fileKey(name));
  }

  restoreFile(name)
  { const content = localStorage.getItem(this.fileKey(name))||"";

    if ( content || name == this.source.default_file )
    { Module.FS.writeFile(name, content);
      this.source.addFileOption(name);
    } else
    { this.source.files.list = this.source.files.list
				.filter((n) => (n != name));
    }
  }

  restoreFiles()
  { const self = this;
    let f = localStorage.getItem(this.itemKey("files"));
    if ( f ) this.source.files = JSON.parse(f);

    this.source.files.list.forEach((f) => self.restoreFile(f));
    if ( !this.source.files.list.includes(this.source.default_file) )
      this.source.files.list.unshift(this.source.default_file);

    let current = this.source.files.current;
    this.source.files.current = null;
    this.source.switchToFile(current || this.source.default_file);
  }

  loadFile(name)
  { name = name || this.source.files.current;

    try
    { let content = Module.FS.readFile(name, { encoding: 'utf8' });
      this.source.value = content;
    } catch(e)
    { this.source.value = "";
    }
  }

  persistsFile(name)
  { if ( this.source.userFile(name) )
    { try
      { let content = Module.FS.readFile(name, { encoding: 'utf8' });
	localStorage.setItem(this.fileKey(name), content);
      } catch(e)
      { localStorage.removeItem(this.fileKey(name));
      }
    }
  }

  persistFiles() {
    this.source.ensureSavedCurrentFile();

    const l = this.source.files.list.filter((n) => this.source.isUserFile(n));
    const save =
	  { list: l,
	    current: l.includes(this.source.files.current)
		? this.source.files.current
		: this.source.default_file
	  };

    localStorage.setItem(this.itemKey("files"), JSON.stringify(save));

    save.list.forEach((f) => this.persistsFile(f));
  }

  persist() {
    this.persistRegistered();
    if ( this.source )
      this.persistFiles();
  }

  restore() {
    if ( this.source )
      this.restoreFiles();
  }
}

		 /*******************************
		 *         START PROLOG         *
		 *******************************/

/**
 * The `Tinker` class puts it all together and can be considered an
 * example on how the other classes can be used.
 */

export class Tinker {
  static prepared = false;	// Only do this once
  /**
   * @type {Persist}
   */
  persist;			// Persist instance
  /**
   * @type {Console}
   */
  console;			// Console instance
  /**
   * @type {Source}
   */
  source;			// Source instance

  /**
   * Instantiate `Tinker` from an HTML DOM tree.   See `tinker.html`
   * for the structure of this DOM.  The {@link Source} is created
   * in a `<div>` with class `tinker-source`.  This `<div>` decides
   * in the layout and supported components.
   *
   * On first call,  the `SWIPL` WASM Module must be  passed in.  This
   * is used to initialise the  global variables `Module` and `Prolog`
   * in this module. as  well as to load the Prolog  part of tinker as
   * `tinker.pl`
   *
   * @param {object} [options]
   * @param {HTMLElement} [options.root] Root element below which to
   * find and instantiate the components
   * @param {SWIPL} [options.module] WASM module holding SWI-Prolog.
   * __must__ be provided on first invocation.
   * @param {bool} [options.banner] If `true`, print welcome banner.
   */
  constructor(options) {
    options = options||{};
    const root = options.root||document;
    if ( !Module ) {
      if ( !options.module )
	throw new TypeError(`Module expected; found ${options.module}`);
      Module = options.module;
      Prolog = Module.prolog;
    }

    // Create the components
    this.persist = new Persist();
    this.console = new Console(root.querySelector("div.tinker-console"),
			       { persist: this.persist
			       });
    this.source  = new Source(root.querySelector("div.tinker-source"),
			      { persist: this.persist,
				console: this.console
			      });

    // Prepare SWI-Prolog
    if ( !Tinker.prepared ) {
      Tinker.prepared = true;
      Prolog.consult("tinker.pl", {module:"system"}).then(() => {
	Prolog.query("tinker:tinker_init(Dir)",
		     {Dir:this.source.user_dir}).once();
	if ( options.banner )
	  Prolog.call("version");
	this.console.addQuery();
      });
    } else {
      if ( options.banner )
	Prolog.call("version");
      this.console.addQuery();
    }
  }
}
