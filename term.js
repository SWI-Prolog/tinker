/*  Part of SWISH

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2025, VU University Amsterdam
			      CWI Amsterdam
			      SWI-Prolog Solutions b.v.
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
 * @fileOverview
 * Make Prolog terms responsive
 *
 * @version 0.5.0
 * @author Jan Wielemaker, jan@swi-prolog.org
 */

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

function getOffset(element) {
  if ( !element.getClientRects().length ) {
    return { top: 0, left: 0 };
  }

  const rect = element.getBoundingClientRect();
  return ({ top: rect.top + window.scrollY,
	    left: rect.left + window.scrollX
	  });
}

		 /*******************************
		 *          CLASS TERM          *
		 *******************************/

let themenu;

export class Term {
  elem;

  constructor(el) {
    this.elem = el;
    this.prepare(el);
  }

  /**
   * Fold a  single node or  all elements  of a NodeList.   Folding is
   * achieved by  adding the class `fold`  to the compound node  and a
   * `span.pl-ellipsis` right after the head (functor name)
   *
   * @param {HTMLElement|NodeList} set Element or set of elements to fold.
   */
  fold(set) {
    function _fold(elm) {
      elm.classList.remove("vertical");
      elm.classList.add("fold");

      if ( !elm.querySelector(":scope > .pl-ellipsis") ) {
	if ( elm.classList.contains("pl-compound") )
	  elm.querySelector(".pl-functor")
	     .after(el("span.pl-ellipsis", "...)"));
	else if ( elm.classList.contains("pl-list") )
	  elm.querySelector(".pl-list-open")
	     .after(el("span.pl-ellipsis", "..."));
	else if ( elm.classList.contains("pl-dict") )
	  elm.querySelector(".pl-dict-open")
	     .after(el("span.pl-ellipsis", "...}"));
      }
    }

    if ( set instanceof NodeList )
      set.forEach((e) => _fold(e));
    else
      _fold(set)
  }

  unfold(set) {
    function _unfold(el) {
      if ( el.classList.contains("fold") ) {
	el.classList.remove("fold");
	const ell = el.querySelector(":scope > span.pl-ellipsis");
	if ( ell )
	  ell.remove();
      }
    }

    if ( set instanceof NodeList )
      set.forEach((e) => _unfold(e));
    else
      _unfold(set)
  }

  getLayout(el) {
    if ( el.classList.contains("fold") )
      return "ellipsis";
    if ( el.classList.contains("vertical") )
      return "vertical";
    return "horizontal";
  }

  layout(set, options) {
    if ( typeof(options) == "string" )
      options = { layout: options };
    else
      options = options||{};

    if ( options.propagate == undefined &&
	 options.layout == "horizontal" )
      options.propagate = true;

    function _layout(el, options) {
      if ( options.propagated && !options.no_ellipsis &&
	   this.getLayout(el) == "ellipsis" )
	return;

      if ( options.layout == "vertical" ) {
	this.unfold(el);
	el.classList.add("vertical");
      } else if ( options.layout == "horizontal" ) {
	this.unfold(el);
	el.classList.remove("vertical");
      } else if ( options.layout == "ellipsis" ) {
	this.fold(el);
      } else if ( options.layout == "auto" ) {
	this.layout(el, "horizontal");
	if ( autoLayout.call(this, el, options) )
	  el.classList.add("vertical");
      }

      if ( options.propagate ) {
	const opts = {...options, propagate:false, propagated:true};

	this.layout(el.querySelectorAll(".pl-adaptive"), opts);
      }
    }

    function autoLayout(el, options) {
      let rc = false;

      el.childNodes.forEach((e) => {
	rc = rc && autoLayout(e, options);
      });

      if ( el.classList.contains("pl-adaptive") ) {
	let layout;

	if ( el.data && (layout=el.data.layout) ) {
	  this.layout(layout);
	} else {
	  if ( el.offsetWidth > (options.width||400) ) {
	    this.layout(el, "vertical");
	    layout = "vertical";
	  }
	}

	return layout == "vertical";
      }

      return false;
    }

    if ( set instanceof NodeList )
      set.forEach((e) => _layout.call(this, e, options));
    else
      _layout.call(this, set, options);
  }

  fit(el) {
    const adapt = el.closest(".pl-adaptive.pl-level-0");
    if ( adapt ) {
      const paren = adapt.closest(".pl-embrace");

      if ( paren )
	this.fit_parenthesis(paren);
    }
  }

  /**
   * Called on `.pl-embrace` to adjust the size of the parenthesis
   */
  fit_parenthesis(set) {
    function _fit_parenthesis(el) {
      const paren = el.querySelectorAll(":scope > .pl-parenthesis");
      const em    = el.querySelectorAll(":scope > .pl-embraced");
      let has_vertical_sibling = false;

      for(let n of em) {
	if ( n.classList.contains("vertical") ) {
	  has_vertical_sibling = true;
	  break;
	}
      }

      if ( has_vertical_sibling ) {
	el.classList.add("vertical");
      } else {
	el.classList.remove("vertical");
      }
    }

    if ( set instanceof NodeList )
      set.forEach((e) => _fit_parenthesis(e));
    else
      _fit_parenthesis(set)
  }

  getTitle(elm) {
    elm = elm.closest(".pl-adaptive");
    if ( elm ) {
      let title;
      if ( elm.classList.contains("pl-compound") ) {
	title = elm.dataset.name+"/"+elm.dataset.arity;
      } else if ( elm.classList.contains("pl-list") ) {
	title = elm.dataset.length + " elements";
	if ( elm.dataset.partial )
	  title += " (partial)";
      } else if ( elm.classList.contains("pl-dict") ) {
	title = "Dict";
      } else if ( elm.classList.contains("pl-binding") ) {
	title = "Binding";
      }
      return title;
    }
  }

  /**
   * Show a menu for adjusting the layout of `el` that has the functor
   * `funct`.  When  the user clicks  the menu, remove it  and perform
   * the selected action.
   */
  menu(funct, options) {
    const elm = funct.closest(".pl-adaptive");
    const title = this.getTitle(elm);

    function item(icon, action, title) {
      const i = el("li",
		   el("span", el("span.icon", icon), title));
      i.dataset.action = action;
      return i;
    }

    const menu = el("ul.pl-compound-menu",
		    el("li.header", title),
		    item("🖤", "auto", "Smart"),
		    item("➡", "horizontal", "Horizontal"),
		    item("⬇", "vertical", "Vertical"),
		    item("⋯", "ellipsis", "Ellipsis"),
		    item("⎘", "copy", "Copy"));

    function clickHandler(ev) {
      ev.stopPropagation();
      if ( ev.target.closest(".pl-compound-menu") ) {
	const action = ev.target.closest("li").dataset.action;
	let on = elm;
	if ( on.classList.contains("pl-binding") ) {
	  on.children.forEach((o) => {
	    if ( action == "ellipsis" )
	      o.classList.add("fold");
	    else
	      o.classList.remove("fold");
	  });

	  if ( action != "copy" )
	    on = on.querySelector(":scope > .pl-adaptive.pl-level-0");
	}

	const self = menu.data.instance;
	if ( action == "copy" ) {
	  // Avoid ellipsis and possible change in vertical layout
	  const clone = on.cloneNode(true);
	  self.layout(clone,
		      { layout: 'horizontal',
			no_ellipsis:true,
			propagate:true
		      });
	  const text = clone.textContent;
	  navigator.clipboard.writeText(text);
	} else {
	  self.layout(on, action);
	  self.fit(on);
	}
      }

      menu.remove();
      themenu = null;
      document.removeEventListener("click", clickHandler);
    }
    document.addEventListener("click", clickHandler);

    const offset = getOffset(funct);
    menu.style.top = (offset.top+funct.offsetHeight)+"px";
    menu.style.left = offset.left+"px";
    menu.data = { instance: this };
    document.body.appendChild(menu);
    const mrect = menu.getBoundingClientRect();
    const win = menu.ownerDocument.defaultView;
    if ( mrect.bottom+10 > win.innerHeight )
      menu.style.top = (offset.top-mrect.height)+"px";
    themenu = menu;
  }

  activate(set, act) {
    function _activate(el, act) {
      if ( act ) {
	el.classList.add("pl-active");
	el.title = this.getTitle(el);
      } else
	el.classList.remove("pl-active");
    }

    if ( set instanceof NodeList )
      set.forEach((e) => _activate.call(this, e, act));
    else
      _activate.call(this, set, act)
  }

  prepare(el) {
    el.addEventListener("click", (ev) => {
      if ( ev.target.matches("span.pl-trigger") && !themenu ) {
	ev.stopPropagation();
	ev.preventDefault();
	this.menu(ev.target);
      }
    });
    el.addEventListener("mouseover", (ev) => {
      if ( ev.target.matches("span.pl-trigger") ) {
	this.activate(ev.target.parentNode, true);
      }
    });
    el.addEventListener("mouseout", (ev) => {
      if ( ev.target.matches("span.pl-trigger") ) {
	this.activate(ev.target.parentNode, false);
      }
    });
  }
} // end class Term
