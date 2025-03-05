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

		 /*******************************
		 *   CONSTANTS AND COMPONENTS   *
		 *******************************/

const user_dir      = "/prolog"
const default_file  = `${user_dir}/scratch.pl`;
let   files	    = { current: default_file,
			list: [default_file]
		      };
let   source;			// right pane (editor + file actions)
let   cm;			// The editor (TODO: remove)

const terminal	    = document.getElementById('console');
const output	    = document.getElementById('output');
let   answer;
let   answer_ignore_nl = false;
const input	    = document.getElementById('input');
const more	    = document.getElementById('more');
const trace	    = document.getElementById('trace');
const abort	    = document.getElementById('abort');
const keyboard	    = document.getElementById('keyboard');
let   waitfor	    = null;
let   abort_request = false;
let   history       = { stack: [], current: null };

function user_file(file)
{ return `${user_dir}/${file}`;
}

function is_user_file(file)
{ return file.startsWith(`${user_dir}/`);
}

		 /*******************************
		 *            SOURCE            *
		 *******************************/

/**
 * Encapsulate the  right pane of  Tinker, providing the  editor, file
 * selector, (re)consult button and up/download buttons.
 */

class TinkerSource {
  select_file;			// File selector
  editor;			// TinkerEditor instance
  elem;				// The <form>

  constructor(elem) {
    const self = this;
    this.elem = elem;
    this.select_file = this.byname("select-file");
    this.editor = new TinkerEditor(this.byname("editor"),
				   () => self.afterEditor());
    window.cm = cm = this.editor;	// TODO hack

    this.armFileSelect();
    this.armNewFileButton();
    this.armFileCreateButton();
    this.armDeleteFile();
    this.armDownloadButton();
    this.armUploadButton();
    this.armConsult();
  }

  afterEditor() {
    this.addExamples();
    toplevel();
  }

  setValue(source)    { this.editor.setValue(source); }
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

    if ( files.current != name ) {
      if ( files.current )
	Persist.saveFile(files.current);
      files.current = name;
      if ( !files.list.includes(name) )
	files.list.push(name);
      Persist.loadFile(name);
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
    files.list = files.list.filter((n) => (n != file));
    localStorage.removeItem(file);
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

	if ( del == default_file )
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
	const text = this.getValue();
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
      this.setValue(content);
      Persist.saveFile(name);
    }
  }

  armUploadButton() {
    const btn = this.elem.querySelector("a.btn.upload");
    if ( btn ) {
      btn.addEventListener("click", (ev) => {
	const exch = ev.target.closest("span.exch-files");
	if ( exch.classList.contains("upload-armed") )
	{ const files = exch.querySelector('input.upload-file').files;
	  download_files(files).then(() => {
	    exch.classList.remove("upload-armed");
	  });
	} else
	{ exch.classList.add("upload-armed")
	}
      });
    }
  }

  /**
   * Arm the form submit button.
   */

  armConsult() {
    this.elem.addEventListener('submit', (e) => {
      e.preventDefault();
      Persist.saveFile(files.current);
      query(`consult('${files.current}').`);
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

  userFile(file) {
    return `${user_dir}/${file}`;
  }

  isUserFile(file) {
    return file.startsWith(`${user_dir}/`);
  }

  baseName(path) {
    return path.split("/").pop();
  }

  byname(name) {
    return this.elem.querySelector(`[name=${name}]`);
  }
} // end class TinkerSource


		 /*******************************
		 *      THE SOURCE EDITOR       *
		 *******************************/

/**
 * Encapsulate  the  editor.   In  this  case  the  actual  editor  is
 * CodeMirror.  Defines methods to
 *   - Initialise the editor
 *   - Set and get its value
 *   - Go to a line/column
 */

class TinkerEditor {
  static cm_url = "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.65.9";
  static cm_swi = "https://www.swi-prolog.org/download/codemirror";
  CodeMirror;

  constructor(container, cont) {
    const instance = this;

    function loadCss(url)
    { const link = document.createElement("link");
      link.type = "text/css";
      link.rel = "stylesheet";
      link.href = url;
      document.getElementsByTagName("head")[0].appendChild(link);
    }

    function cm_url(sub) {
      return TinkerEditor.cm_url + sub;
    }
    function cm_swi(sub) {
      return TinkerEditor.cm_swi + sub;
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
	      Persist.restore();
	      cont.call(this.cm);
	    });

    loadCss(cm_swi("/theme/prolog.css"));
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
   * @param {string} new content for the editor
   */
  setValue(content) {
    return this.cm.setValue(content);
  }

  /**
   * @return {string} current content of the editor
   */
  getValue() {
    return this.cm.getValue();
  }

  /**
   * Go to a given 1-based line number
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
} // End class TinkerEditor


		 /*******************************
		 *    PROLOG OUTPUT STREAMS     *
		 *******************************/

function print_output(line, cls, sgr) {
  if ( line.trim() == "" && answer && answer_ignore_nl )
  { answer_ignore_nl = false;
    return;
  }

  let node;
  if ( sgr && sgr.link )
  { node = document.createElement('a');
    node.href = sgr.link;
    node.target = "_blank";
    node.addEventListener("click", tty_link);
  } else
  { node = document.createElement('span');
    if ( sgr )
    { if ( sgr.color )
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
  (answer||output).appendChild(node);
};


async function tty_link(ev)
{ const a = ev.target;
  const to = a.href;
  if ( to.startsWith("file://") ||
       to.match("https?://.*\\.pl\(#\d+\)?") )
  { ev.preventDefault();
    await Prolog.forEach("tinker:tty_link(Link)", {Link:to});
  }
  // Use default action
}


function getPromiseFromEvent(item, event) {
  return new Prolog.Promise((resolve) => {
    const listener = (ev) => {
      item.removeEventListener(event, listener);
      resolve(ev);
    }
    item.addEventListener(event, listener);
  })
}

async function get_single_char()
{ terminal.classList.add("key");
  keyboard.focus();
  const ev = await getPromiseFromEvent(keyboard, "keyup");
  terminal.classList.remove("key");
  return ev.keyCode;
}

function getCharSize(element)
{ if ( !element.char_size )
  { let temp = document.createElement("span");
    temp.className = "stdout";
    temp.textContent = "test";
    element.appendChild(temp);
    const rect = temp.getBoundingClientRect();
    element.char_size = { h: rect.height,
			  w: rect.width/4
			};
    element.removeChild(temp);
  }
  return element.char_size;
}

function tty_size()
{ const tty = document.querySelector("div.console");
  const wrapper = tty.closest("div.scroll-wrapper");
  const charsz = getCharSize(output);
  return [ Math.floor(wrapper.clientHeight/charsz.h),
	   Math.floor(wrapper.clientWidth/charsz.w)
	 ];
}

		 /*******************************
		 *       OUTPUT STRUCTURE       *
		 *******************************/

/** @return {HTMLDivElement} in which the current query should dump
 * its answer.
 */

window.current_answer = () => answer;
window.tty_size = tty_size;

/** Add a structure for a query.  The structure is
 *
 * ```
 * <div class="query-container">
 *   <div class="query-header">?- between(1,3,X).</div
 *   <div class="query-answers">
 *     <div class="query-answer">
 *       <span class="stdout">X = 1;</span>
 *     </div>
 *     <div class="query-answer">
 *       <span class="stdout">X = 2;</span>
 *     </div>
 *     ...
 *   </div>
 * </div>
 * ```
 */

class TinkerQuery {
  elem;				// div.query-container
  answer;			// div.query-answer
  /**
   * Create a `<div>` to interact with a new Prolog query
   *
   * @param {string} query is the Prolog query to run
   * @param {HTMLElement} Element to which to add the new query
   */
  constructor(query, output) {
    const div1 = document.createElement("div");
    const div2 = document.createElement("div");
    const div3 = document.createElement("div");
    const div4 = document.createElement("div");
    const more = document.createElement("div");
    const btns = document.createElement("span");
    const edit = document.createElement("span");
    const close = document.createElement("span");
    const icon = document.createElement("span");
    btns.className = "query-buttons";
    edit.textContent = "\u270E";
    edit.title = "Copy query to input";
    close.textContent = "\u2715";
    icon.className = "query-collapse";
    icon.title = "Collapse/expand answer";
    btns.appendChild(edit);
    btns.appendChild(icon);
    btns.appendChild(close);

    this.fillMore(more);

    const prev = last_query();
    if ( prev )
      prev.collapsed(true);

    div1.className = "query-container";
    div2.className = "query-header";
    div3.className = "query-answers";
    div4.className = "query-answer";
    div1.appendChild(btns);
    div1.appendChild(div2);
    div1.appendChild(div3);
    div1.appendChild(more);
    div3.appendChild(div4);
    div2.textContent = `?- ${query}`;
    edit.addEventListener("click", () => {
      const queryElem = input.querySelector("input");
      queryElem.value = query;
      queryElem.focus();
    });
    close.addEventListener("click", () => div1.remove(), false);
    icon.addEventListener(
      "click",
      (e) => e.target.closest("div.query-container").collapsed());

    this.elem = div1;
    div1.data = { query: this };
    answer = this.answer = div4;

    output.appendChild(div1);
  }

  /**
   * Fill the  input elements that  control user interaction  after an
   * answer has been found.
   */
  fillMore(more) {
    const self = this;
    const next = document.createElement("button");
    const stop = document.createElement("button");

    more.className = "tinker-more";
    next.className = "more-next";
    stop.className = "more-cont";
    next.textContent = "Next";
    stop.textContent = "Stop";
    more.appendChild(next);
    more.appendChild(stop);
    next.addEventListener("click", (ev) => {
      ev.preventDefault();
      self.reply_more("redo");
    });
    stop.addEventListener("click", (ev) => {
      ev.preventDefault();
      self.reply_more("continue");
    });
  }

  promptMore() {
    // this.elem.classList.add("more");
    const btn = this.elem.querySelector("button.more-next");
    btn.focus();
  }

  /**
   * Set/clear.toggle the collapsed state of the query
   */
  collapsed(how) {
    if ( how === true )
       this.elem.classList.add("collapsed");
    else if ( how === false )
      this.elem.classList.remove("collapsed");
    else
      this.elem.classList.toggle("collapsed");
  }

  /**
   * Add a `div.query-answer` element to capture the output and
   * solution of the next answer.
   */
  next_answer() {
    if ( this.answer ) {
      const div4 = document.createElement("div");
      div4.className = "query-answer";
      answer.after(div4);
      answer = this.answer = div4;
      answer_ignore_nl = true; // suppress the first newline
    }
  }

  /**
   * Handle the "Next"/"Stop" buttons
   */
  reply_more(action) {
    if ( waitfor && waitfor.yield == "more" )
    { switch(action)
      { case "redo":     print_output(";", "stdout"); this.next_answer(); break;
	case "continue": print_output(".", "stdout"); answer_ignore_nl = true; break;
      }
      next(waitfor.resume(action), this);
    }
  }
} // end class TinkerQuery

function last_query()
{ const q = output.lastChild;
  if ( q && q.classList.contains("query-container") )
    return q.data.query;
  return undefined;
}

/** Run a query.  Used for e.g., consulting the current file.
 * @param {String} query is the query to run.
 */
function query(query)
{ const q = new TinkerQuery(query, output);

  if ( waitfor && waitfor.yield == "query" )
  { set_state("run");
    next(waitfor.resume(query), q);
  } else
  { Prolog.call(query);
  }
}

		 /*******************************
		 *        ENTER A QUERY         *
		 *******************************/

function submitQuery(queryElem)
{ const input = queryElem.querySelector("input");
  let query = input.value;
  input.value = '';
  let q;

  if ( queryElem.ex_target == "query" ||
       queryElem.ex_target == "term" )
  { if ( query.trim() == "" )
    { return false;
    } else
    { if ( ! /\.\s*/.test(query) )
        query += ".";
    }
  }

  if ( queryElem.ex_target == "query" )
  { history.stack.push(query);
    history.current = null;
    q = new TinkerQuery(query, output);
  }

  set_state("run");
  next(waitfor.resume(query), q);
  return true;
}

function focusInput(queryElem, why)
{ const input  = queryElem.querySelector("input");
  const prompt = queryElem.querySelector("span.prompt");
  switch(why)
  { case "query":
    { prompt.textContent = "?-";
      break;
    }
    default:
    { const s = Prolog.prompt_string(0)||"|: ";
      prompt.textContent = s;
    }
  }
  input.placeholder = `Please enter a ${why}`;
  input.focus();
  queryElem.ex_target = why;
}

input.addEventListener("keydown", (event) =>
{ if ( event.key == "Tab" )
  { const elem   = input.querySelector("input");
    const caret  = elem.selectionStart;
    const all    = elem.value;
    const before = all.slice(0,caret);
    const after  = caret == all.length ? "" : all.slice(caret-all.length);

    function commonPrefix(words)
    { let i = 0;

      while(words[0][i] && words.every(w => w[i] === words[0][i]))
	i++;
      return words[0].slice(0, i);
    }

    function setCompletion(to, del)
    { elem.value = ( before.slice(0, before.length-del.length) +
		     to +
		     after );
    }

    const res = Prolog.query(
      "tinker:complete_input(Before,After,Delete,Completions)",
      {Before:before, After:after}).once();

    if ( res.Completions.length == 1 )
    { setCompletion(res.Completions[0], res.Delete.v);
    } else if ( res.Completions.length > 1 )
    { const common = commonPrefix(res.Completions);
      if ( common.length > 0 )
	setCompletion(common, res.Delete.v);
    }

    event.preventDefault();
  }
});

input.addEventListener("keyup", (event) =>
{ if ( event.defaultPrevented ) return;

  switch(event.key)
  { case "ArrowUp":
      if ( history.current == null )
      {	history.saved = event.target.value;
	history.current = history.stack.length;
      }
      if ( --history.current >= 0 )
      { event.target.value = history.stack[history.current];
      }
      break;
    case "ArrowDown":
      if ( history.current != null )
      { if ( ++history.current < history.stack.length )
	{ event.target.value = history.stack[history.current];
	} else if ( history.current == history.stack.length )
	{ event.target.value = history.saved;
	}
      }
      break;
    case "Enter":
    { submitQuery(input);
      break;
    }
    default:
      return;
  }

  event.preventDefault();
}, true);


		 /*******************************
		 *     CONTROLLING A QUERY      *
		 *******************************/

abort.addEventListener('submit', (e) => {
  e.preventDefault();
  if ( waitfor && waitfor.abort )
  { console.log("aborting", waitfor);
    waitfor.abort();
  } else
  { console.log("Requesting abort");
    abort_request = true;
  }
}, false);


		 /*******************************
		 *            TRACER            *
		 *******************************/

function reply_trace(action)
{ if ( waitfor && waitfor.yield == "trace" )
  { print_output(` [${action}]`, "stderr", {color: "#888"});
    Prolog.call("nl(user_error)", {nodebug:true});

    switch(action)
    { case "goals":
      case "listing":
      case "help":
      { trace_action(action, waitfor.trace_event);
	break;
      }
      default:
      { set_state("run");
	next(waitfor.resume(action));
      }
    }
  }
}

function trace_action(action, msg)
{ const prolog = Prolog;

  return prolog.with_frame(() =>
  { const av = prolog.new_term_ref(2);

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

const trace_shortcuts = {
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

trace.addEventListener("keyup", (ev) => {
  if ( ev.defaultPrevented ) return;
  const action = trace_shortcuts[ev.key];
  if ( action )
  { ev.preventDefault();
    ev.stopPropagation();
    reply_trace(action);
  }
});

		 /*******************************
		 *       TOPLEVEL STATES        *
		 *******************************/

function set_state(state)
{ terminal.className = "console " + state;
}

function next(rc, query)
{ waitfor = null;

  if ( rc.yield !== undefined )
  { waitfor = rc;

    Prolog.flush_output();

    if ( abort_request )
    { abort_request = false;
      return next(waitfor.resume("wasm_abort"));
    }

    switch(rc.yield)
    { case "beat":
      return setTimeout(() => next(waitfor.resume("true"), query), 0);
      case "query":
        answer = undefined;
        /*FALLTHROUGH*/
      case "term":
      case "line":
        set_state("prompt "+rc.yield);
        focusInput(input, rc.yield);
        break;
      case "more":
        set_state("more");
        query.promptMore();
        break;
      case "trace":
      { trace_action("print", waitfor.trace_event);
        set_state("trace");
        document.getElementById("trace.creep").focus();
        break;
      }
      case "builtin":
        rc.resume((rc)=>next(rc));
        break;
    }
  } else if ( rc.error )
  { if ( rc.message == "Execution Aborted" )
    { Prolog.call("print_message(informational, unwind(abort))");
    } else
    { console.log("Unhandled exception; restarting", rc);
    }
    toplevel();
  }
}

function toplevel()
{ let rc = Prolog.call("wasm_query_loop",
		       { async:true,
			 debugger:true
		       });

  next(rc);
}

		 /*******************************
		 *         START PROLOG         *
		 *******************************/

let Prolog;
let Module;
var options = {
  arguments: ['-g', 'true'],
  locateFile: function(file) { // not needed with swipl-bundle.js
    return '/wasm/' + file;
  },
  on_output: print_output
};

SWIPL(options).then(async (module) =>
    { Module = module;
      Prolog = Module.prolog;
      Module.FS.mkdir(user_dir);
      await Prolog.load_scripts();
      await Prolog.consult("tinker.pl", {module:"system"});
      Prolog.query("tinker:tinker_init(Dir)", {Dir:user_dir}).once();
      Prolog.call("version");
      window.source = source = new TinkerSource(
	document.querySelector("form[name=source]"));
    });


		 /*******************************
		 *        PERSIST FILES         *
		 *******************************/

class Persist
{ static autosave = true;

  static persistsFile(name)
  { if ( is_user_file(name) )
    { try
      { let content = Module.FS.readFile(name, { encoding: 'utf8' });
	localStorage.setItem(name, content);
      } catch(e)
      { localStorage.removeItem(name);
      }
    }
  }

  static restoreFile(name)
  { let content = localStorage.getItem(name)||"";

    if ( content || name == default_file )
    { Module.FS.writeFile(name, content);
      source.addFileOption(name);
    } else
    { files.list = files.list.filter((n) => (n != name));
    }
  }

  static restoreFiles()
  { const self = this;
    let f = localStorage.getItem("files");
    if ( f ) files = JSON.parse(f);

    files.list.forEach((f) => self.restoreFile(f));
    if ( !files.list.includes(default_file) )
      files.list.unshift(default_file);

    let current = files.current;
    files.current = null;
    source.switchToFile(current || default_file);
  }

  static loadFile(name)
  { name = name || files.current;

    try
    { let content = Module.FS.readFile(name, { encoding: 'utf8' });
      cm.setValue(content);
    } catch(e)
    { cm.setValue("");
    }
  }

  static saveFile(name, force)
  { if ( force || is_user_file(name) )
    { Module.FS.writeFile(name, cm.getValue());
    }
  }

  static persist()
  { localStorage.setItem("history", JSON.stringify(history));
    const l = files.list.filter((n) => is_user_file(n)||n == default_file);
    const save =
	  { list: l,
	    current: l.includes(files.current) ? files.current : default_file
	  };

    localStorage.setItem("files",   JSON.stringify(save));

    save.list.forEach((f) => this.persistsFile(f));
  }

  static restoreHistory()
  { const h = localStorage.getItem("history");

    if ( h )
      history = JSON.parse(h);
  }

  static restore()
  { this.restoreFiles();
    this.restoreHistory();
  }
}

window.onunload = (e) => {
  if ( Persist.autosave )
    Persist.persist();
}

		 /*******************************
		 *          DEMO CALLS          *
		 *******************************/

window.add_one = (n) => n+1;

window.promise_any = (data) =>
{ console.log(data);

  return new Promise(function(resolve, reject)
  { resolve(data);
  });
}
