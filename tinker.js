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
  mirror_dir;			// Directory for files loaded from a URL
  /**
   * Resolved with  myself after the  editor is created,  the files
   * are restored  from the  browser's localStorage and  the demos
   * are added to the file menu.
   * @type {Promise<Source>}
   */
  ready;
  #readySignal;			// Resolve `ready`

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
   * @param {string} [options.mirror_dir] Directory below which files
   * that are loaded from a URL are placed, using a directory structure
   * that reflects the URL.  Defaults to `"/prolog/web"`.
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
    if ( this.con )
      this.con.source = this;
    this.user_dir = opts.user_dir||"/prolog";
    this.mirror_dir = opts.mirror_dir||`${this.user_dir}/web`;
    this.default_file =
      `${this.user_dir}/${opts.default_file_name||"scratch.pl"}`
    this.files = { current: this.default_file,
		   list: [this.default_file],
		   origins: {},		// file -> URL it was loaded from
		   pristine: {}		// file -> hash as loaded from origin
		 };
    this.select_file = this.byname("select-file");

    Module.FS.mkdirTree(this.user_dir);

    this.ready = new Promise((resolve) => { this.#readySignal = resolve; });
    this.editor = new Editor(this.byname("editor"),
			     () => self.afterEditor(),
			     { source:this });
    // Register event handling
    this.armFileSelect();
    this.armNewFileButton();
    this.armFileCreateButton();
    this.armDeleteFile();
    this.armDownloadButton();
    this.armUploadButton();
    this.armShareButton();
    this.armOptimise();
    this.armConsult();
  }

  /**
   * Called after the editor  has been created.  Restores the files
   * from the browser's localStorage, adds the demos to the file menu
   * and finally resolves {@link Source#ready}.  Note that the demos
   * must be added before we are ready as {@link Source#addFileOption}
   * inserts before the demos and {@link Source#addExamples} skips
   * demos for which the user has a file.
   */
  async afterEditor() {
    this.persist.restore();
    try
    { await this.addExamples();
    } catch(e)				// no examples/index.json
    { console.error("Tinker: could not add demos:", e);
    }
    this.#readySignal(this);
  }

  set value(source)     { this.editor.value = source; }
  get value()           { return this.editor.value; }
  goto(line,options)    { this.editor.goto(line,options); }
  mark(from,to,options) { this.editor.mark(from,to,options); }
  clearMarks(from,to)   { this.editor.clearMarks(from,to); }

  /**
   * The default  translation to Prolog  gets the content as  an atom.
   * We return an instance of Prolog.String.
   * @return {Prolog.String} representing the current content of the
   * editor.
   */
  getValueAsPrologString() {
    return new Prolog.String(this.value);
  }

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
	const url = `/wasm/examples/${ex.name}`;
	if ( !this.mirrorFile(new URL(url, window.location.href).href) ) {
	  const opt = document.createElement("option");
	  opt.className = "url";
	  opt.value = url;
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
      node.textContent = this.displayName(name);
      node.value = name;
      node.selected = true;
      const sep = this.demoOptionSep();
      if ( sep )
	select.insertBefore(node, sep);
      else
	select.appendChild(node);
    }
    node.title = this.fileOrigin(name);	// show the origin as tooltip

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
      this.editor.file = name;
      if ( !this.files.list.includes(name) )
	this.files.list.push(name);
      this.persist.loadFile(name);
      this.updateDownload(name);
    }
  }

		 /*******************************
		 *      FILES FROM THE WEB      *
		 *******************************/

  /**
   * Set or  clear the  URL a  file was loaded  from.  Files  with an
   * origin are  _mirrors_: they are  consulted under their  URL (see
   * {@link Source#consultFiles} and `tinker:tinker_load_file/2`) such
   * that relative  loads from the  file are resolved  against the
   * remote directory, while the local copy provides the content.
   *
   * @param {string} file Absolute path of a user file.
   * @param {string} [url] URL, may be relative to the page.  If
   * omitted, the origin is removed.
   */
  setOrigin(file, url) {
    if ( url )
      this.files.origins[file] = new URL(url, window.location.href).href;
    else
      delete this.files.origins[file];
  }

  /**
   * @param {string} file Absolute path of a user file.
   * @return {string} the URL `file` was loaded from or `""`.  This is
   * called from Prolog, hence `""` rather than `undefined`.
   */
  fileOrigin(file) {
    return this.files.origins[file]||"";
  }

  /**
   * Remember the content of `file` as it was loaded from its origin,
   * so that we can tell whether the user changed it.  See
   * {@link Source#isPristine}.
   *
   * @param {string} file Absolute path of a user file.
   * @param {string} [content] Content as loaded.  If omitted, the file
   * is no longer considered a copy of its origin.
   */
  setPristine(file, content) {
    if ( content === undefined )
      delete this.files.pristine[file];
    else
      this.files.pristine[file] = hashString(content);
  }

  /**
   * @param {string} file Absolute path of a user file.
   * @return {bool} `true` if `file` still holds the content that was
   * loaded from its origin.
   */
  isPristine(file) {
    const hash = this.files.pristine[file];
    let content;

    if ( hash === undefined )
      return false;

    if ( file == this.files.current ) {
      content = this.value;
    } else {
      try
      { content = Module.FS.readFile(file, {encoding:'utf8'});
      } catch(e)
      { return false;
      }
    }

    return hash === hashString(content);
  }

  /**
   * @param {string} url Absolute URL.
   * @return {string} the local file that mirrors `url` or `""`.
   */
  mirrorFile(url) {
    const origins = this.files.origins;

    for(const file of Object.keys(origins)) {
      if ( origins[file] == url )
	return file;
    }

    return "";
  }

  /**
   * Local file to  use for the mirror of `url`.   We recreate the
   * directory structure  of the URL  below `mirror_dir` such  that a
   * program and  the files it  loads keep their relation  and files
   * from different sites cannot collide.  For example
   *
   * ```
   * https://example.org/dir/run.pl
   *     -> /prolog/web/example.org/dir/run.pl
   * ```
   *
   * @param {string} url Absolute URL.
   * @return {string} absolute path below `mirror_dir`.
   */
  mirrorPath(url) {
    const found = this.mirrorFile(url);
    if ( found )
      return found;

    const u    = new URL(url, window.location.href);
    const dirs = [ safeFileName(u.host.replace(/:/g, "_")),
		   ...u.pathname.split("/").slice(0,-1)
		                .filter((s) => s != "")
		                .map(safeFileName)
		 ];
    const base = this.urlFileName(url);
    const ext  = /^(.*?)(\.[^.]*)?$/.exec(base);
    let file   = [this.mirror_dir, ...dirs, base].join("/");

    // Two URLs can still map onto the same file, e.g., if they only
    // differ in their scheme or in a character we do not allow.
    for(let i=2; this.fileOrigin(file) != ""; i++)
      file = [this.mirror_dir, ...dirs, `${ext[1]}_${i}${ext[2]||""}`].join("/");

    return file;
  }

  /**
   * @param {string} file Absolute path of a user file.
   * @return {bool} `true` if `file` is mirrored from a URL.
   */
  isMirrorFile(file) {
    return file.startsWith(`${this.mirror_dir}/`);
  }

  /**
   * Name  to display  in the  file  menu.  Files  mirrored from  a
   * URL are shown  as `host/dir/file.pl`, eliding  the middle of
   * long paths.  The full URL is available as tooltip.
   *
   * @param {string} file Absolute path of a user file.
   * @return {string} name to display.
   */
  displayName(file) {
    if ( !this.isMirrorFile(file) )
      return this.baseName(file);

    const rel  = file.substring(this.mirror_dir.length+1).split("/");
    const base = rel.pop();
    const host = rel.shift();

    if ( rel.length <= 2 )
      return [host, ...rel, base].join("/");
    return [host, "…", rel[rel.length-1], base].join("/");
  }

  /**
   * Create the directory holding `file` if it does not exist.
   * @param {string} file Absolute path of a file.
   */
  ensureDir(file) {
    const dir = file.replace(/\/[^/]*$/, "");

    if ( dir != "" )
      Module.FS.mkdirTree(dir);
  }

  /**
   * Create or refresh the local  copy of `url`.  Called from Prolog
   * when the user opens  a file that was loaded from  a URL, e.g. by
   * `edit/1` or by clicking an error location.
   *
   * @param {string} url URL the content came from.
   * @param {string} content Content of the file.
   * @param {object} [options] See {@link Source#addFile}.  By default
   * the file is __not__ displayed in the editor.
   * @return {string} the absolute path of the mirror.
   */
  addMirror(url, content, options) {
    const origin = new URL(url, window.location.href).href;

    options = Object.assign({switchTo:false}, options||{}, {origin});
    return this.addFile(this.mirrorPath(origin), content, options);
  }

  /**
   * Make sure  the WASM file system  copy of `file`  reflects what is
   * in the editor.  Only writes if the content changed, such that the
   * modification time keeps its meaning for `ensure_loaded/1`, `make/0`
   * and friends.  Called from Prolog before consulting a mirror.
   *
   * @param {string} file Absolute path of a user file.
   * @return {bool} `true` if the file was written.
   */
  syncFile(file) {
    if ( file == this.files.current && this.isUserFile(file) ) {
      const content = this.value;
      let old;

      try
      { old = Module.FS.readFile(file, {encoding:'utf8'});
      } catch(e)
      { old = undefined;
      }

      if ( old !== content ) {
	Module.FS.writeFile(file, content);
	return true;
      }
    }

    return false;
  }

  /**
   * Add a  file to the  WASM file system  and the file  menu.  This
   * is the  single entry point  for getting  a file into  Tinker: it
   * is used  for the demos,  uploaded files and  files preloaded by
   * {@link Tinker#preload}.  If the file exists it is overwritten.
   *
   * @param {string} name Base name or absolute path in `user_dir`.
   * @param {string} content Content for the file.
   * @param {object} [options]
   * @param {bool} [options.switchTo] If `false`, do not display the
   * file in the editor.
   * @param {string} [options.origin] URL this content came from.  See
   * {@link Source#setOrigin}.  A previous origin is cleared if this
   * option is not provided.
   * @return {string} the absolute path of the file.
   */
  addFile(name, content, options) {
    options = options||{};
    const file = this.isUserFile(name)
	  ? name
	  : this.userFile(this.baseName(name));

    this.ensureDir(file);
    Module.FS.writeFile(file, content);	// before switchToFile(): that
    this.setOrigin(file, options.origin);// loads the file from the FS
    this.setPristine(file, options.origin ? content : undefined);
    this.addFileOption(file);

    if ( this.files.current == file ) {	// switchToFile() is a no-op
      this.value = content;
      this.updateDownload(file);
    } else if ( options.switchTo !== false ) {
      this.switchToFile(file);
    } else if ( !this.files.list.includes(file) ) {
      this.files.list.push(file);
    }

    return file;
  }

  /**
   * Fetch a file over HTTP.
   * @param {string} url URL to fetch.  May be relative to the page.
   * @return {Promise<object>} object holding `name`, the base name
   * derived from the URL, `content` and `url`, the absolute URL that
   * was fetched.  The promise is rejected if the URL is invalid or
   * cannot be fetched.
   */
  async fetchFile(url) {
    const name = this.urlFileName(url);	// throws on an invalid URL
    const abs  = new URL(url, window.location.href).href;
    const res  = await fetch(abs);
    if ( !res.ok )
      throw new Error(`${res.status} ${res.statusText}`);

    return { name, content: await res.text(), url: abs };
  }

  /**
   * Fetch `url` and add its content as a user file.  The file
   * remembers `url` as its origin.
   * @param {string} url URL to fetch.  May be relative to the page.
   * @param {object} [options] See {@link Source#addFile}
   * @return {Promise<string>} the absolute path of the file.
   */
  async addFileFromURL(url, options) {
    const {name, content, url:origin} = await this.fetchFile(url);
    return this.addFile(name, content, Object.assign({origin}, options||{}));
  }

  /**
   * Derive the name  of a user file from a  URL.  Removes the search
   * and fragment  of the URL, replaces  characters we do not  want in
   * a file name and adds `.pl` if the URL carries no extension.
   * @param {string} url URL.  May be relative to the page.
   * @return {string} base name for the file.
   */
  /**
   * File name  for the  `name=` parameter of  a `code=`  preload.  We
   * only accept a plain file name in `user_dir`.
   * @param {string} name Name from the query string
   * @return {string} base name for the file.
   */
  codeFileName(name) {
    let base = safeFileName(this.baseName(name));

    if ( base == "" )
      base = "code.pl";
    if ( !base.includes(".") )
      base += ".pl";

    return base;
  }

  urlFileName(url) {
    const u = new URL(url, window.location.href);
    let base = safeFileName(u.pathname.split("/").pop());

    if ( base == "" )
      base = "unnamed.pl";
    if ( !base.includes(".") )
      base += ".pl";

    return base;
  }

  /**
   * Consult files by injecting a query into the console.  All files
   * are  consulted from  a single  query because  {@link Console#injectQuery}
   * cannot start a  new query while a previous one  is still running.
   *
   * Files that  were loaded from  a URL  are consulted using  the URL
   * they came from.  See {@link Source#setOrigin}.
   *
   * @param {string[]} [files] Files to consult.  These may be files in
   * the WASM file system as well as URLs.  Default is the file that is
   * currently displayed in the editor.
   * @return {bool} `true` if a query was injected.
   */
  consultFiles(files) {
    if ( !this.con )
      return false;

    if ( !files ) {
      const file = this.ensureSavedCurrentFile();
      files = file ? [file] : [];
    }
    if ( files.length == 0 )
      return false;

    const ids  = files.map((f) => this.fileOrigin(f)||f);
    const goal = ids.length == 1
	  ? `consult(${prologAtom(ids[0])}).`
	  : `consult([${ids.map(prologAtom).join(",")}]).`;
    this.con.injectQuery(goal);

    return true;
  }

  /**
   * Print an informational message to the console.
   * @param {string} msg Message to print.  Printed as a Prolog comment.
   */
  printMessage(msg) {
    if ( this.con )
      this.con.printMessage(`% ${msg}\n`, "stderr", {color:"blue"});
    else
      console.log(msg);
  }

  /**
   * Print an error message to the console.
   * @param {string} msg Message to print.
   * @param {Error} [err] Exception that caused this error.  Printed
   * on the browser console for developers.
   */
  printError(msg, err) {
    if ( err )
      console.error(err);
    if ( this.con )
      this.con.printMessage(`% ERROR: ${msg}\n`, "stderr", {color:"red"});
    else
      console.error(msg);
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
    this.setOrigin(file);
    this.setPristine(file);
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
	const url = opt.value;
	this.fetchFile(url)
	  .then(({content, url:origin}) => {
	    // Turn the demo into a local file before addFile() adds it
	    // to the menu, as we would else get a second entry.
	    const file = this.mirrorPath(origin);
	    opt.className = "local";
	    opt.value = file;
	    opt.textContent = this.displayName(file);
	    this.addFile(file, content, {origin});
	  })
	  .catch((e) => {
	    this.printError(`Failed to load ${url}: ${loadErrorMessage(e)}`, e);
	    this.switchToFile(this.files.current);
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
      this.addFile(this.baseName(file.name), await this.readAsText(file));
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

  /**
   * URL that  opens Tinker with the  current file loaded.  This  is the
   * URL of the page without its query string, extended with
   *
   *   - `url=`, if the file was loaded from a URL and the user did not
   *     change it.  This keeps the file small and, unlike `code=`,
   *     preserves files loaded by this file.
   *   - `code=` and `name=` otherwise, which carry the content of the
   *     editor and the name of the file.
   *
   * See {@link parsePreloadSearch}.
   *
   * @return {string} URL that can be shared with others.
   */
  shareURL() {
    const base = `${window.location.origin}${window.location.pathname}`;
    const file = this.files.current;
    const origin = this.fileOrigin(file);

    if ( origin && this.isPristine(file) )
      return `${base}?url=${encodeURIComponent(origin)}`;

    return `${base}?code=${encodeURIComponent(this.value)}` +
	         `&name=${encodeURIComponent(this.baseName(file))}`;
  }

  /**
   * Copy {@link Source#shareURL} to the clipboard.  If we may not use
   * the clipboard (the  page must be served  from a _secure context_)
   * we ask the user to copy the URL.
   *
   * @param {HTMLElement} [btn] Element that is briefly modified to
   * acknowledge the copy.
   * @return {Promise<string>} the URL.
   */
  async share(btn) {
    const url = this.shareURL();

    try
    { await navigator.clipboard.writeText(url);
      if ( btn )
      { const label = btn.textContent;
	btn.textContent = "✔";		// ✔
	setTimeout(() => { btn.textContent = label; }, 1500);
      }
      this.printMessage(
	url.length > 2000
	  ? `Copied a link of ${url.length} characters to the clipboard. \
Note that many sites and mail programs truncate long links.`
	  : `Copied a link of ${url.length} characters to the clipboard`);
    } catch(e)
    { this.printError(`Could not copy the link: ${e.message||e}`, e);
      window.prompt("Copy this link to share the program", url);
    }

    return url;
  }

  armShareButton() {
    const btn = this.elem.querySelector("a.btn.share");
    if ( btn ) {
      btn.addEventListener("click", (ev) => {
	ev.preventDefault();
	this.share(btn);
      });
    }
  }

  ensureSavedCurrentFile() {
    const file = this.files.current;
    if ( file ) {
      this.syncFile(file);
      return file;
    }
  }

  /**
   * Arm  the checkbox  that controls  the Prolog  flag `optimise`.
   * Note  that the  flag only  affects  programs that  are loaded
   * after it is changed.
   */

  armOptimise() {
    const cb = this.byname("optimise");

    if ( cb ) {
      cb.addEventListener("change", (ev) => {
	Prolog.call(`set_prolog_flag(optimise, ${cb.checked})`);
	this.printMessage(
	  `Compiling ${cb.checked ? "with" : "without"} optimisation. \
This applies to programs that are loaded from now on.`);
      });
    }
  }

  /**
   * Update the optimise  checkbox from the Prolog  flag.  Called after
   * Prolog is initialised and after {@link Tinker#preload}, which may
   * change the flag from the query string of the page.
   */

  showOptimise() {
    const cb = this.byname("optimise");

    if ( cb ) {
      try
      { const answer = Prolog.query("current_prolog_flag(optimise, V)").once();
	cb.checked = answer.V == true || answer.V == "true";
      } catch(e)
      { console.error(e);
      }
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
	this.consultFiles();
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
  CodeMirror;			// Our CodeMirror instance
  timeout;			// Timeout to detect typing pause (highlight)
  source;			// Related Source instance
  file;				// Current file
  current_var_clause;		// Clause marked as current var

  /**
   * @param {HTMLDivElement} container Element in which to create the editor
   * @param {function} cont Call-back called when the editor is completed.
   * @param {object} options
   * @param {object} options.source {@link Source} instance embedding me.
   */
  constructor(container, cont, options) {
    const instance = this;
    this.source = options.source;

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
	      Prolog.consult("highlight.pl", {engine:true}).then(() => {
		cont.call(this.cm);
	      });
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
    this.cm.on("change", (cm, change) => {
      this.change(change);
    });
    this.cm.on("cursorActivity", (cm) => {
      this.cursorActivity();
    });
    this.cm.on("viewportChange", (cm, from, to) => {
      this.viewport(from, to);
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
   * Called  on  changes  to  the  editor.   This  is  used  to  drive
   * highlighting updates.  To sync with PceEmacs, the scheme is to
   *   - Do a full xref and highlight pass on changing the value
   *     and after 2 seconds idle time.
   *   - Do an incremental update for the current clause on each
   *     change.  This gets the clause around the caret.
   */
  change(change) {
    const gen = this.cm.changeGeneration();

    if ( this.timeout ) {
      clearTimeout(this.timeout);
      this.timeout = null;
    }

    if ( change.origin == "setValue" ) {
      console.log("New value");
      this.current_var_clause = null;
      this.refreshHighlight("init");
    } else {
      this.refreshTermHighlight(this.getClause());
      this.timeout = setTimeout(() => {
	this.refreshHighlight("timed");
      }, 2000);
    }
  }

  /**
   * Trap  cursor activity  to highlight  the variable  at the  cursor
   * position.
   */
  cursorActivity() {
    const cm = this.cm;
    const cursor = cm.getCursor();
    let word;

    function getWord(from) {
      const a = cm.findWordAt(from);
      const w = cm.getRange(a.anchor, a.head);
      if ( w.match(/\w+/) )
	return w;
    }

    if ( !(word=getWord(cursor)) &&
	 cursor.ch > 0 ) {
      const prev = {...cursor};
      prev.ch--;
      word=getWord(prev);
    }

    this.unmarkVar();
    if ( word && /^(_|\p{Lu})/u.test(word) )
      this.markVar(word);
  }

  /**
   * Trap changes  to the viewport.   Currently unused.  This  must be
   * used to avoid highlighting large files as a whole.
   */
  viewport(from, to) {
    //console.log("Showing lines", from, to);
  }

  /**
   * Refresh the highlight of the clause around the cursor using
   * the last cross-reference data.
   */
  async refreshTermHighlight(clause) {
    const source = this.source;
    await Prolog.forEach("highlight:refresh_clause(Source, Clause)",
			 {Source:source, Clause:clause}, {engine:true});
  }

  /**
   * Mark the variable named `varname` in the current clause.
   */
  markVar(varname) {
    const clause = this.getClause();
    clause.current_variable = varname;
    this.refreshTermHighlight(clause);
    this.current_var_clause = clause;
  }

  unmarkVar() {
    const clause = this.current_var_clause;
    if ( clause ) {
      this.current_var_clause = null;
      delete clause.current_variable;
      this.refreshTermHighlight(clause);
    }
  }

  /**
   * Refresh all highlighting over the entire file.
   */
  async refreshHighlight(why) {
    const source = this.source;

    this.saveMarks();
    await Prolog.forEach("highlight:highlight_all(Source)",
			 {Source:source}, {engine:true});
    this.finalizeSavedMarks();
  }

  /**
   * GetQ the  editor text  that holds the  clause around  the cursor.
   * @return {object} An  object holding the text as  well as position
   * information.
   */
  getClause() {
    const cm = this.cm;
    const cursor = cm.getCursor();
    const cline = cursor.line;
    const first = cm.firstLine();
    const last  = cm.lastLine();

    function lineEndState(ln) {
      const info = cm.lineInfo(ln);
      return info.handle.stateAfter;
    }

    function hasFullStop(ln) {
      const info   = cm.lineInfo(ln);
      const handle = info.handle;
      if ( handle.styles && handle.styles.includes("fullstop") )
	return "token";
      if ( /[^#$&*+-./:<=>?@\\^~]\.\s/.test(info.text) )
	return "regex";
    }

    function bodyLine(ln) {
      const info = lineEndState(ln);
      return info && info.inBody;
    }

    let sline = cline;
    let eline = cline;
    let end;
    while( sline > first && bodyLine(sline-1) )
      sline--;
    while( eline < last && !(end=hasFullStop(eline)) )
      eline++;
    //console.log(`Clause in lines ${sline}..${eline}`);

    let lines = [];
    for(let i = sline; i<=eline; i++)
      lines.push(cm.lineInfo(i).text);

    return ({ text:       new Prolog.String(lines.join("\n")),
	      file:	  this.file,
	      start_line: sline,
	      end_line:   eline,
	      end_reason: end,
	      start_char: this.startIndexOfLine(sline),
	      change_gen: cm.changeGeneration()
	    });
  }

  /**
   * @param {number} 0-based line number
   * @return {number} character index of the first character
   * of the line.
   */
  startIndexOfLine(ln) {
    const cm = this.cm;
    let index = 0;
    for(let i=cm.firstLine(); i<ln; i++)
      index += cm.lineInfo(i).text.length+1;
    return index;
  }

  /**
   * Go to a given 1-based line number.  The target line is styled
   * in CSS using the `.CodeMirror-search-match` selector.
   *
   * @param {number} line
   * @param {Object} [options]
   * @param {number} [options.linepos] Go to a specific column
   * @param {string} [options.className] CSS class.  Defaults to
   * `CodeMirror-search-match`
   * @param {string} [options.title] Element `title` attribute.
   * Default to `"Target line"`
   * @param {number} [options.margin] Vertical space visible above
   * and below the target line.  Defaults to 100 (pixels).
   */

  goto(line, options) {
    options      = options||{};
    const ch     = options.linepos||0;
    const cname  = options.className||"CodeMirror-search-match";
    const title  = options.title||"Target line";
    const margin = options.margin == undefined ? 100 : options.margin;

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
    if ( margin )
      this.cm.scrollIntoView({line:line,ch:ch}, margin);
    this.cm._searchMarkers.push(
      this.cm.markText({line:line, ch:0},
		       {line:line, ch:this.cm.getLine(line).length},
		       { className: cname,
			 clearOnEnter: true,
			 clearWhenEmpty: true,
			 title: title
		       }));
    this.cm.on("cursorActivity", clearSearchMarkers);
  }

  indexCache;

  indexToPos(index) {
    const cm = this.cm;
    let last = cm.lastLine();
    let line;
    let ls;

    if ( this.indexCache &&
	 cm.changeGeneration() == this.indexCache.gen &&
	 index >= this.indexCache.start ) {
      line = this.indexCache.line;
      ls   = this.indexCache.start;
    } else {
      line = cm.firstLine();
      ls = 0;
    }

    for( ; line <= last ; line++ ) {
      const info = cm.lineInfo(line);
      const len  = info.text.length;
      if ( index <= ls+len ) {
	this.indexCache = {
	  gen:   cm.changeGeneration(),
	  line:  line,
	  start: ls
	};
	return {line:line, ch:index-ls};
      }
      ls += len+1;
    }
    return {line:last, ch:0};
  }

  /**
   * Add a CodeMirror text mark for the range (from,to).
   * @param {number|object} from Start location as 0-based offset or CodeMirror
   * {line:Line, ch:LinePos} object.
   * @param {number|object} to End location as 0-based offset or CodeMirror
   * {line:Line, ch:LinePos} object.
   * @param {object} options Options passed to {@link
   * CodeMirror#markText}.  Typically contains `className`.  May also
   * contain `attributes for additional attributed on the `<span>` that
   * is created.  This often involves `title`.
   */

  markers;
  savedMarkers;

  saveMarks() {
    this.savedMarkers = this.markers;
    this.markers = [];
  }

  finalizeSavedMarks() {
    if ( this.savedMarkers ) {
      for(let m of this.savedMarkers)
	m.clear();
      this.savedMarkers = null;
    }
  }

  keepMark(from, to, options) {
    if ( this.savedMarkers ) {
      if ( typeof(from) !== "number" ||
	   typeof(to)   !== "number" )
	return false;

      function equalMark(m, from, to, options) {
	function equalAttrs(a1, a2) {
	  if ( !a1 && !a2 )
	    return true;
	  for(let a of ["title"]) {
	    if ( a1[a] || a2[a] && a1[a] != a2[a] )
	      return false;
	  }
	  return true;
	}

	return ( m._from == from && m._to == to &&
		 m.className == options.className &&
		 equalAttrs(options.attributes, m.attributes) );
      }

      for(let i=0; i<this.savedMarkers.length; i++) {
	const m = this.savedMarkers[0];
	if ( equalMark(m, from, to, options) ) {
	  this.markers.push(m);
	  this.savedMarkers.shift();
	  return true;
	} else {
	  if ( m._from < to ) {
	    m.clear();
	    this.savedMarkers.shift();
	    return false;
	  }
	}
      }
    }

    return false;
  }

  mark(from, to, options) {
    const cm = this.cm;

    if ( this.keepMark(from, to, options) )
      return;

    const toPos = (x) => typeof(x) === "number" ? this.indexToPos(x) : x;

    if ( !this.markers )
      this.markers = [];

    delete options["$tag"];
    if ( options.attributes ) {
      delete options.attributes["$tag"];
    }

    const f = toPos(from);
    const t = toPos(to);
    const m = cm.markText(f, t, options);
    m._from = from;
    m._to   = to;
    this.markers.push(m);
  }

  clearMarks(from, to) {
    const cm = this.cm;

    if ( this.markers ) {
      if ( from === undefined ) from = 0;
      if ( to   === undefined ) to   = 100000000;

      let del = 0;
      for(let i=0; i<this.markers.length; i++) {
	const m = this.markers[i];
	if ( (m._from >= from && m._from <= to) ||
	     (m._to   >= from && m._to   <= to) ) {
	  del++;
	  m.clear();
	  this.markers[i] = undefined;
	}
      }
      if ( del )
	this.markers = this.markers.filter((el) => el !== undefined);
    }
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
		 *         QUERY STRING         *
		 *******************************/

/**
 * Parse the query string of the page into a list of files that must
 * be preloaded by {@link Tinker#preload}.  Accepted forms are
 *
 *   - `?<url>` <br>
 *     The entire query string is a single URL.  It is __not__ split
 *     at `&`, so the URL may have a query string of its own.
 *   - `?url=<url>` <br>
 *     URL to load.  May be repeated to load multiple files.
 *   - `?code=<text>` <br>
 *     Prolog text to load.  May be repeated.
 *   - `?name=<file>` <br>
 *     Name for the file created from the preceding `code=`.  Default
 *     is `code.pl`.
 *   - `?optimise=true` or `?optimise=false` <br>
 *     Set the Prolog flag `optimise` before loading the files.  See
 *     {@link Tinker#preload}.
 *
 * Values  of `url`  and `code`  must be  encoded using  JavaScript's
 * `encodeURIComponent()`.  Note that  this does not encode  `+`.  We
 * deliberately  do not  use `URLSearchParams`,  which maps  `+` onto
 * a space and thus breaks e.g., `?code=X is 1+2.`
 *
 * A query string that starts with a parameter we do not know is
 * ignored.  This allows embedding Tinker in a page that uses the
 * query string for its own purposes.
 *
 * @param {string} search Query string, e.g., `window.location.search`
 * @return {object[]} List of objects holding `type` (`"url"`, `"code"`
 * or `"optimise"`) and `value`, in the order they appear in `search`.
 * Objects of type `"code"` may have a `name`.
 */

export function parsePreloadSearch(search) {
  const s = (search||"").replace(/^\?/, "");

  if ( s == "" )
    return [];

  const m = /^([a-zA-Z_][a-zA-Z0-9_]*)(=|&|$)/.exec(s);
  if ( !m )				// bare ?<url>
    return [ {type:"url", value:decodeURL(s)} ];
  if ( !preload_params.includes(m[1]) ) {
    console.warn(`Tinker: ignoring query string "${s}"`);
    return [];
  }

  const items = [];
  for(let part of s.split("&")) {
    const i   = part.indexOf("=");
    const key = i < 0 ? part : part.substring(0,i);
    const val = i < 0 ? ""   : decodeParam(part.substring(i+1));
    const last = items[items.length-1];

    switch(key)
    { case "url":
      case "code":
	items.push({type:key, value:val});
        break;
      case "name":
	if ( last && last.type == "code" )
	  last.name = val;
	else
	  console.warn(`Tinker: "name" does not follow a "code" parameter`);
        break;
      case "optimise":
	if ( /^(|true|yes|on|1)$/i.test(val) )
	  items.push({type:key, value:true});
	else if ( /^(false|no|off|0)$/i.test(val) )
	  items.push({type:key, value:false});
	else
	  console.warn(`Tinker: ignoring "optimise=${val}"`);
        break;
      default:
	console.warn(`Tinker: ignoring query string parameter "${key}"`);
    }
  }

  return items;
}

const preload_params = ["url", "code", "name", "optimise"];

/**
 * `decodeURIComponent()` that returns its input on invalid escapes.
 */
function decodeParam(s) {
  try
  { return decodeURIComponent(s);
  } catch(e)				// URIError on a stray `%`
  { return s;
  }
}

/**
 * Turn a  path segment of  a URL into  a name we  accept in the  WASM
 * file system.  Notably removes the `..` and `.` special names.
 * @param {string} s Segment of the path of a URL
 * @return {string} name for a file or directory
 */

function safeFileName(s) {
  return decodeParam(s)
	     .replace(/[^a-zA-Z0-9._-]/g, "_")
	     .replace(/^\.+/, "");
}

/**
 * Hash a  string.  Used to  tell whether the  user changed a  file we
 * loaded from a URL.  This is FNV-1a, which is short and good enough
 * to detect a change.
 * @param {string} s
 * @return {string} hash as a hexadecimal string
 */

function hashString(s) {
  let h = 0x811c9dc5;

  for(let i=0; i<s.length; i++) {
    h ^= s.charCodeAt(i);
    h = Math.imul(h, 0x01000193);
  }

  return (h>>>0).toString(16);
}

/**
 * Decode a bare `?<url>`.  As the URL is not required to be encoded,
 * we only decode if it does not start with a scheme and contains an
 * escape, i.e., we accept both `?https://x/a.pl` and `?https%3A//x/a.pl`.
 */
function decodeURL(s) {
  if ( !/^[a-zA-Z][a-zA-Z0-9+.-]*:/.test(s) && s.includes("%") )
    return decodeParam(s);
  return s;
}

/**
 * Quote a string as a Prolog atom.  We always quote, which is both
 * simpler and always correct.
 * @param {string} s Text to represent as a Prolog atom
 * @return {string} quoted atom, e.g. `'/prolog/my file.pl'`
 */

export function prologAtom(s) {
  const esc = String(s)
	.replace(/\\/g, "\\\\")
	.replace(/'/g, "\\'")
	.replace(/[\x00-\x1f]/g,
		 (c) => `\\x${c.charCodeAt(0).toString(16)}\\`);

  return `'${esc}'`;
}

/**
 * Turn an exception from `fetch()` into a message.  A failure to
 * connect or a CORS violation only provides `TypeError: Failed to
 * fetch`, which is not of much help to the user.
 * @param {Error} e Exception raised
 * @return {string} message to display
 */

function loadErrorMessage(e) {
  if ( e instanceof TypeError )
    return `${e.message} (server unreachable or it does not send \
Access-Control-Allow-Origin)`;
  return e.message||String(e);
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
  source;			// Source instance I'm related to

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
    let q = this.output.lastChild;
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
   * @return {HTMLElement} the element that was added.
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

    return node;
  }

  /**
   * Print a message that is not  the output of a query, e.g., a file
   * that  could not  be loaded.   Unlike {@link Console#print},  this
   * inserts  above the  prompt of  the open  query rather  than below
   * it.
   *
   * @param {string} line content that must be printed
   * @param {string} cls class (stream), one of `"stdout"` or `"stderr"`
   * @param {object} [sgr] see {@link Console#print}
   */
  printMessage(line, cls, sgr) {
    const node  = this.print(line, cls, sgr);
    const query = this.lastQuery();

    if ( query && query.elem != node && node.parentNode == this.output &&
	 query.elem.parentNode == this.output )
      this.output.insertBefore(node, query.elem);
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
      const btn = el(`button.${action}`, el("span", label));
      btn.title = title;
      btn.addEventListener("click", () => {
	self.replyTrace(action);
      });
      return btn;
    }

    const trace = el("div.tinker-trace",
		     button("creep",   "Creep (c,Space,Enter)", "↴"),
		     button("skip",    "Skip (s)",              "↷"),
		     button("retry",   "Retry (r)",             "↻"),
		     button("nodebug", "Nodebug (n)",           "▷"),
		     button("abort",   "Abort (a)",             "🛑"),
		     button("help",    "Help (?)",              "?"));

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
	{ this.traceAction("print", rc.trace_event);
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
	{ this.traceAction(action, this.waitfor.trace_event);
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
  traceAction(action, msg) {
    const self = this;
    if ( this.engine )
      return this.engine.with(() => self.__traceAction(action, msg));
    else
      return self.__traceAction(action, msg);
  }

  __traceAction(action, msg) {
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

  /**
   * Get  access to  the shortcut  mapping.  This  both allows  giving
   * correct help and modifying the shortcuts from Prolog.
   * @return {object} Mapping from Event `key` to an action.
   */
  shortcuts(which) {
    switch(which)
    { case "trace":
        return trace_shortcuts;
      case "more":
      return more_shortcuts;
    }
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
 * Mapping from  keys to  Prolog trace events.   Can be  accessed from
 * {@link Query#shortcuts}.
 * @type {object}
 */

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

const more_shortcuts = {
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

  /**
   * Remove all data  Tinker keeps in the  browser's localStorage.  As
   * the current state would otherwise be  saved again when the page is
   * left, this also clears {@link Persist#autosave}.  Used by the
   * Prolog predicate `tinker_reset/0`, which reloads the page.
   *
   * @return {object} number of `files` and `history` entries removed.
   */
  reset() {
    const removed = {files: 0, history: 0};

    this.autosave = false;

    for(const key of Object.keys(localStorage)) {
      if ( !key.startsWith(this.prefix) )
	continue;

      const name = key.substring(this.prefix.length);
      if ( name.startsWith("file/") ) {
	removed.files++;
      } else if ( name == "history" ) {
	try
	{ removed.history = JSON.parse(localStorage.getItem(key)).stack.length;
	} catch(e)
	{ removed.history = 0;
	}
      }
      localStorage.removeItem(key);
    }

    return removed;
  }

  restoreFile(name)
  { const content = localStorage.getItem(this.fileKey(name))||"";

    if ( content || name == this.source.default_file )
    { this.source.ensureDir(name);
      Module.FS.writeFile(name, content);
      this.source.addFileOption(name);
    } else
    { this.source.files.list = this.source.files.list
				.filter((n) => (n != name));
      this.source.setOrigin(name);
    }
  }

  restoreFiles()
  { const self  = this;
    const files = this.source.files;
    const f = localStorage.getItem(this.itemKey("files"));

    if ( f )				// may predate `origins`
    { const saved = JSON.parse(f);
      if ( Array.isArray(saved.list) ) files.list    = saved.list;
      if ( saved.current )             files.current = saved.current;
      files.origins  = saved.origins||{};
      files.pristine = saved.pristine||{};
    }

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
    const origins = {};
    const pristine = {};
    l.forEach((f) => { const url = this.source.fileOrigin(f);
		       if ( url )
		       { origins[f] = url;
			 const hash = this.source.files.pristine[f];
			 if ( hash !== undefined )
			   pristine[f] = hash;
		       }
		     });
    const save =
	  { list: l,
	    current: l.includes(this.source.files.current)
		? this.source.files.current
		: this.source.default_file,
	    origins,
	    pristine
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
  /**
   * Resolved after `tinker.pl` is loaded and initialised.  Only the
   * first instance loads Prolog, so this is a static property.
   * @type {Promise}
   */
  static prologReady = null;
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
   * Resolved  with myself  after Prolog  is initialised  __and__ the
   * editor and file menu are ready.  Note that the Prolog and editor
   * startup are independent: the editor is loaded from a CDN.
   * @type {Promise<Tinker>}
   */
  ready;

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
   * @param {bool|string} [options.preload] If `true`, preload the files
   * from the query string of the page.  See {@link Tinker#preload}.  If
   * a string, this is used as the query string.
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
    if ( !Tinker.prologReady ) {
      Tinker.prologReady =
	Prolog.consult("tinker.pl", {module:"system"}).then(() => {
	  Prolog.query("tinker:tinker_init(Dir)",
		       {Dir:this.source.user_dir}).once();
	});
    }
    Tinker.prologReady.then(() => {
      if ( options.banner )
	Prolog.call("version");
      this.console.addQuery();	// must be done before we can inject
    });				// a query into the console.

    this.ready = Promise.all([ Tinker.prologReady, this.source.ready ])
	                .then(() => this);

    this.ready
	.then(() => { if ( options.preload )		// may set `optimise`
		        return this.preload(options.preload === true
					    ? window.location.search
					    : options.preload);
		    })
	.then(() => this.source.showOptimise())
	.catch((e) => console.error(e));
  }

  /**
   * Preload files from  the query string of the page.   This allows a
   * link  to  open Tinker  with  one  or  more programs  loaded,  for
   * example
   *
   * ```
   * https://wasm.swi-prolog.org/wasm/tinker?https://example.org/hello.pl
   * ```
   *
   * See {@link parsePreloadSearch}  for the accepted forms.   Each of
   * the files  is added as a  user file and displayed  in the editor,
   * after  which  all  files  are consulted.   Files  that  cannot
   * be loaded are reported on the console and skipped.  URLs that do
   * not refer to Prolog _source_ (e.g., `.qlf` files) are consulted
   * from their URL and are not added to the editor.
   *
   * @param {string} search Query string, e.g., `window.location.search`
   * @return {Promise<string[]>} the files and URLs that are consulted.
   */
  async preload(search) {
    const source = this.source;
    const jobs = [];
    let codes = 0;

    // Establish the target file names before fetching such that the
    // numbering of `code=` parameters does not depend on the order in
    // which the fetches complete.
    for(let item of parsePreloadSearch(search)) {
      if ( item.type == "code" ) {
	const name = item.name
	      ? source.codeFileName(item.name)
	      : ( ++codes == 1 ? "code.pl" : `code_${codes}.pl` );
	jobs.push({ file: source.userFile(name), content: item.value });
      } else if ( item.type == "optimise" ) {
	Prolog.call(`set_prolog_flag(optimise, ${item.value})`);
      } else {
	jobs.push({ url: item.value });
      }
    }

    const loaded = (await Promise.all(jobs.map((j) => this.#preload(j))))
	           .filter((j) => j != null);
    // Only display the last file to avoid needless editor updates.
    const show = loaded.reduce((last, j, i) =>
			       j.content !== undefined ? i : last, -1);
    const consult = [];

    loaded.forEach((job, i) => {
      if ( job.content !== undefined ) {
	if ( source.files.list.includes(job.file) )
	  source.printMessage(`Replaced ${job.file}`);
	source.addFile(job.file, job.content,
		       { switchTo: i == show,
			 origin: job.origin	// undefined for `code=`
		       });
	consult.push(job.file);
      } else {
	source.printMessage(`${job.url} is consulted from its URL`);
	consult.push(job.url);
      }
    });

    if ( consult.length > 0 )
      source.consultFiles(consult);

    return consult;
  }

  /**
   * Get the content for a single file of {@link Tinker#preload}.
   * @param {object} job Holds either `content` or `url`
   * @return {Promise<object>} `job` with `file` and `content` filled or
   * `null` if the file could not be loaded.  If the URL does not refer
   * to Prolog source it is returned unmodified for consulting the URL.
   */
  async #preload(job) {
    const source = this.source;

    if ( job.content !== undefined )
      return job;

    try
    { const url = new URL(job.url, window.location.href);
      if ( /\.qlf$/i.test(url.pathname) )
	return { url: url.href };	// cannot show this in the editor.
					// Must be global for consult/1 to
					// see it as a URL.

      const {content, url:origin} = await source.fetchFile(job.url);
      return { file: source.mirrorPath(origin), content, origin };
    } catch(e)
    { source.printError(`Failed to load ${job.url}: ${loadErrorMessage(e)}`, e);
      return null;
    }
  }
}
