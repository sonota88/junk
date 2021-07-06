class CodeEditor {
  static render(state) {
    return TreeBuilder.build(h =>
      h("div", {
          style: {
            position: "relative"
          , zIndex: 1000
          , background: "#fff"
          , margin: "0.5rem"
          , padding: "0.5rem"
          , "border-radius": "0.2rem"
          }
        }
      , h("button", {
            onclick: ()=>{ __p.onclick_codeEditor_cancel(); }
          }
        , "cancel"
        )
      , h("button", {
            onclick: ()=>{ __p.onclick_codeEditor_update(); }
          }
        , "update"
        )
      , h("br")
      , h("textarea", {
            id: "code_editor_code"
          , style: {
              width: "90%"
            , height: "60%"
            }
          }
        , state.codeEditor.code
        )
      )
    );
  }

  static _nextBegOfLine(src, posOrig) {
    let pos = posOrig;
    while (pos < src.length - 3) {
      if (src.charAt(pos) === "\n") {
        return pos + 1;
      }
      pos++;
    }
    return null;
  }

  static getPosBegin(src, cursorPos) {
    let pos = cursorPos;
    let posBegin = null;
    while (3 <= pos) {
      if (pos === 3) { // テキストの先頭
        if (
          src.charAt(pos - 3) === "`"
            && src.charAt(pos - 2) === "`"
            && src.charAt(pos - 1) === "`"
        ) {
          posBegin = pos;
          break;
        }
      } else {
        if (
          src.charAt(pos - 4) === "\n"
            && src.charAt(pos - 3) === "`"
            && src.charAt(pos - 2) === "`"
            && src.charAt(pos - 1) === "`"
          // 末尾の LF を見ていないが、たぶん問題ない
        ) {
          posBegin = pos;
          break;
        }
      }
      pos--;
    }
    // puts(279, posBegin);

    if (posBegin == null) {
      return null;
    }

    return this._nextBegOfLine(src, pos);
  }

  static getPosEnd(src, cursorPos) {
    let pos = cursorPos;
    let posEnd = null;
    const max = src.length;
    while (pos <= max - 4) {
      if (
           src.charAt(pos + -1) === "\n"
        && src.charAt(pos +  0) === "`"
        && src.charAt(pos +  1) === "`"
        && src.charAt(pos +  2) === "`"
        && src.charAt(pos +  3) === "\n"
      ) {
        posEnd = pos;
        break;
      }
      pos++;
    }
    return posEnd;
  }
}

class View {
  static render(state){
    return TreeBuilder.build(h =>
      h("div", {}
      , h("h1", {}, `「${ state.title }」の編集`)
      , h("a", { href: `/page/${__p.getPageId()}` }, "戻る")
      , h("hr")
      , h("input", {
            id: "input_title"
          , value: state.title
          , style: { width: "90%" }
          , onchange: ()=>{ __p.onchange_title(); }
          }
        )
      , h("br")
      , h("button", {
            onclick: ()=>{ __p.onclick_indentMinus(); }
          }, "indent-"
        )
      , h("button", {
            onclick: ()=>{ __p.onclick_indentPlus(); }
          }, "indent+"
        )
      , h("button", {
            onclick: ()=>{ __p.onclick_insertDateTime(); }
          }, "date time"
        )
      , h("button", {
            onclick: ()=>{ __p.onclick_codeEditor_open(); }
          }, "edit code"
        )

      , state.codeEditor
        ? CodeEditor.render(state)
        : null

      , h("br")
      , h("textarea", {
            id: "edit_box"
          , onchange: ()=>{ __p.onchange_src(); }
          , style: {
              width: "90%"
            , height: "60%"
            }
          }
        , state.src
        )
      , h("br")
      , h("button", { onclick: ()=>{ __p.onclick_preview(); } }, "プレビュー")
      , h("button", { onclick: ()=>{ __p.onclick_submit(); } }, "更新")
      , h("div", {
            id: "preview_box"
          , style: {
              border: "solid 0.5rem #eee"
            }
          }
        , h("pre", { id: "preview_body" })
        )
      )
    );
  }
}

class Page {
  constructor(){
    this.state = {
      title: '{title}'
    , src: '{src}'
    , range: null // "11,22"
    , codeEditor: null
    };
  }

  /**
   * render したら editor を取得しなおす必要がある。
   */
  editor(){
    const $els = $("#edit_box");
    if ($els.length === 0) {
      return null;
    }

    return new Editor($els.get(0));
  }

  getTitle(){
    return `「${ this.state.title }」の編集`;
  }

  getPageId(){
    location.href.match(/page\/(\d+)/);
    return _parseInt(RegExp.$1);
  }

  getRange(){
    const url = new URL(location.href);
    const sp = url.searchParams;
    if (sp.has("range")) {
      return sp.get("range");
    } else {
      return null;
    }
  }

  init(){
    puts("init");

    const params = {};
    const range = this.getRange();
    this.state.range = range;
    if (range != null) {
      params.range = range;
    }

    __g.api_v2(
      "get"
    , `/api/page/${ this.getPageId() }/edit`
    , params
    , (result)=>{
        __g.unguard();
        puts(result);
        Object.assign(this.state, result);

        this.render();
        __g.updatePageTitle(this.getTitle());

      }
    , (errors)=>{
        __g.unguard();
        __g.printApiErrors(errors);
        alert("Check console.");
      }
    );
  }

  render(){
    $("#tree_builder_container")
      .empty()
      .append(View.render(this.state));
    // $("#edit_box").val(this.state.src);
  }

  validSrc_p(src) {
    try {
      let _ = wikiproc.toHTML(
        this.getPageId()
        ,src
        ,{} // this.state.pageIdTitleMap // TODO
        ,{}
      );
      return true;
    } catch(e) {
      console.log(e);
      return false;
    }
  }

  onclick_preview(){
    try {
      const ret = wikiproc.toHTML(
        this.getPageId()
        ,this.state.src
        ,{} // this.state.pageIdTitleMap // TODO
        ,{}
      );
      $("#preview_body").html(ret.mainContent);
    } catch(e) {
      console.log(e);
      $("#preview_body").text("wikiproc failed: show console");
    }
  }

  onclick_submit(){
    puts("submit");
    __g.guard();

    if (! this.validSrc_p(this.state.src)) {
      alert("invalid src");
      __g.unguard();
      return;
    }

    const params = {
      title: this.state.title
      ,src: this.state.src
      ,range: this.state.range
    };

    __g.api_v2(
      "patch"
    , `/api/page/${ __p.getPageId() }`
    , params
    , (result)=>{
        puts("OK");
        sessionStorage.setItem("changedPageId", this.getPageId());
        location.href = `/page/${ __p.getPageId() }`;
      }
    , (errors)=>{
        __g.unguard();
        __g.printApiErrors(errors);
        alert("Check console.");
      }
    );
  }

  getInputTitle(){
    return $("#input_title").val();
  }

  getSrc(){
    return $("#edit_box").val();
  }

  onchange_title(){
    puts("-->> onchange_title");
    const title = this.getInputTitle();
    puts(title);
    this.state.title = title;
    // 再レンダリングしない
  }

  onchange_src(){
    puts("-->> onchange_src");
    const src = this.getSrc();
    puts(src);
    this.state.src = src;
    // 再レンダリングしない
  }

  onclick_indentMinus(){
    this.editor().modifySelection((sel)=>{
      return Editor.unindentSpace(sel);
    });
  }

  onclick_indentPlus() {
    this.editor().modifySelection((sel)=>{
      return Editor.indent(sel, " ");
    });
  }

  onclick_insertDateTime() {
    const pad2 = (n)=>{
      if (n < 10) {
        return "0" + n;
      } else {
        return "" + n;
      }
    }

    const d = new Date();
    const dtstr =
        `${ d.getFullYear() }`
      + `-${ pad2(d.getMonth() + 1) }`
      + `-${ pad2(d.getDate()     ) }`
      + ` ${ pad2(d.getHours()    ) }`
      + `:${ pad2(d.getMinutes()  ) }`
      + `:${ pad2(d.getSeconds()  ) }`
      ;
    
    this.editor().insertText(dtstr);
  }

  // --------------------------------
  // code block editor

  _codeEditor_getTextArea() {
    return $("#code_editor_code").get(0);
  }

  onclick_codeEditor_open() {
    const ed = () => { return this.editor(); };

    puts("-->> onclick_codeEditor_open");
    const src = ed().val();
    const cursorPos = ed().getCursorPos();
    // puts("cursorPos", cursorPos);

    const posBegin = CodeEditor.getPosBegin(src, cursorPos);
    if (posBegin == null) {
      alert("not in code block");
      return;
    }
    // puts(293, posBegin);

    const posEnd = CodeEditor.getPosEnd(src, cursorPos);
    if (posEnd == null) {
      alert("not in code block");
      return;
    }
    // puts(308, posEnd);

    const pre = src.substring(0, posBegin);
    const code = src.substring(posBegin, posEnd);
    const post = src.substring(posEnd, src.length);

    puts("pre", JSON.stringify(pre));
    puts("target", JSON.stringify(code));
    puts("post", JSON.stringify(post));

    this.state.codeEditor = {
      pre, code, post, cursorPos
    };
    this.render();
    __g.guard();
    $(this._codeEditor_getTextArea()).focus();
  }

  onclick_codeEditor_update() {
    const ed = () => { return this.editor(); };

    puts("-->> onclick_codeEditor_update");

    let edited = this._codeEditor_getTextArea().value;
    if (! edited.endsWith("\n")) {
      edited = edited + "\n";
    }
    // puts(JSON.stringify(edited));

    const newSrc = [
      this.state.codeEditor.pre,
      edited,
      this.state.codeEditor.post
    ].join("");
    // puts(344, newSrc);
    this.state.src = newSrc;

    let newCursorPos = this.state.codeEditor.cursorPos;
    if (
      this.state.codeEditor.pre.length + edited.length < newCursorPos
    ) {
      // 編集前より短くなった場合
      newCursorPos = this.state.codeEditor.pre.length;
    }

    this.state.codeEditor = null;

    this.render();
    ed().setCursorPos(newCursorPos);
    ed().focus();
    __g.unguard();
  }

  onclick_codeEditor_cancel() {
    const ed = () => { return this.editor(); };

    puts("-->> onclick_codeEditor_cancel");

    let newCursorPos = this.state.codeEditor.cursorPos;
    this.state.codeEditor = null;

    this.render();
    ed().setCursorPos(newCursorPos);
    ed().focus();
    __g.unguard();
  }
}

__g.ready(new Page());
