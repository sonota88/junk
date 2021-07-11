class Editor {

  constructor(el){
    this.textarea = el;
    this.$textarea = $(el);

    this.keyCodeMap = {
      32: "SPC"
      ,65: "a"
      ,68: "d"
      ,69: "e"
      ,72: "h"
      ,75: "k"
    };
    this.keyBind = {
      // "C-SPC": set_mark_command
      "C-a"      : this._move_beginning_of_line
      ,"C-d"     : this._deleteChar
      ,"C-e"     : this._move_end_of_line
      ,"C-h"     : this._backwardDeleteChar
      ,"C-k"     : this._selectLineTail
      ,"C-M-SPC" : this._selectCuurentToken
    };

    var cmd = "";
    const me = this;

    function _execCommand(ev, key){
      var fn = me.keyBind[key];
      if(fn){
        ev.preventDefault();
        fn();
      }
      cmd = "";
    }

    this.$textarea.on("keydown", (ev)=>{
      // puts(ev);
      if(ev.ctrlKey){
        cmd += "C-";
      }
      if(ev.altKey){
        cmd += "M-";
      }

      if([
        32 // SPC
        ,65 // a
        ,68 // d
        ,69 // e
        ,72 // h
        ,75 // k
      ].indexOf(ev.keyCode) >= 0)
      {
        cmd += me.keyCodeMap[ev.keyCode];
      }

      _execCommand(ev, cmd);
    });
  }

  val(){
    return this.$textarea.val();
  }

  focus(){
    this.textarea.focus();
  }

  setSelectionRange(from, to){
    this.textarea.setSelectionRange(from, to);
  }
  _setCursorPos(textarea, pos){
    textarea.setSelectionRange(pos, pos);
  }
  setCursorPos(pos) {
    this._setCursorPos(this.textarea, pos);
  }
  _getCursorPos(textarea) {
    return textarea.selectionStart;
  }
  getCursorPos() {
    return this._getCursorPos(this.textarea);
  }
  _goto_char(pos){
    this.textarea.setSelectionRange(pos, pos);
  }

  insertText(text){
    var origText = this.$textarea.val();
    var pos = this._getCursorPos(this.textarea);
    var pre = origText.substr(0, pos);
    var post = origText.substr(pos);
    this.$textarea.val(pre + text + post);

    this._setCursorPos(this.textarea, pos + text.length);
    this.textarea.focus();
  }

  /**
   * 選択範囲を加工する
   */
  modifySelection(proc){
    const posStart = this.textarea.selectionStart;
    const posEnd = this.textarea.selectionEnd;
    if(posStart === posEnd){
      return;
    }

    const orig = this.textarea.value;
    const pre = orig.substr(0, posStart);
    const sel = orig.substring(posStart, posEnd);
    const post = orig.substr(posEnd);
    const modified = proc(sel);
    this.textarea.value = pre + modified + post;
    this.textarea.setSelectionRange(posStart, posStart + modified.length);
    this.textarea.focus();
  }

  _isBeginningOfLine(point){
    return (point === 0 || this.textarea.value.charAt(point - 1) === "\n");
  }

  /**
   * @return 最も近い前方の改行の次、またはテキストの最初
   */
  _getBeginningOfLine(){
    var text = this.textarea.value;
    var point = this.textarea.selectionStart;
    var to = null;
    for(var i=point-1; i>=0; i--){
      if(text.charAt(i) === "\n"){
        to = i + 1;
        break;
      }
    }
    if(to === null){
      to = 0;
    }
    return to;
  }

  /**
   * @return スペース・タブを無視した行頭の位置
   */
  _getBeginningOfLineIgnoreSpace(){
    var text = this.textarea.value;
    var point = this.textarea.selectionStart;
    var to = null;
    var ch;
    for(var i=point; i<=text.length; i++){
      ch = text.charAt(i);
      if(ch === "\n"){
        break;
      }
      if(ch === " "
         || ch === "\t"
         || ch === "　"
        )
      {
        ;
      }else{
        to = i;
        break;
      }
    }
    return to;
  }

  _move_beginning_of_line(){
    var point = this.textarea.selectionStart;
    var to = null;

    if(isBeginningOfLine(point)){
      to = getBeginningOfLineIgnoreSpace();
    }else{
      to = getBeginningOfLine();
    }

    goto_char(to);
  }

  // 改行またはテキストの最後
  _getEndOfLine(){
    var point = this.textarea.selectionStart;
    var text = this.textarea.value;
    var to = null;
    for(var i=point; i<=text.length; i++){
      if(text.charAt(i) === "\n"){
        to = i;
        break;
      }
    }
    if(to === null){
      to = text.length;
    }
    return to;
  }

  _move_end_of_line(){
    var to = getEndOfLine();
    goto_char(to);
  }

  _deleteChar(){
    var text = this.textarea.value;
    var pos = this.textarea.selectionStart;
    var pre = text.substring(0, pos);
    var post = text.substring(pos + 1);
    this.textarea.value = pre + post;

    goto_char(pos);
  }

  _backwardDeleteChar(){
    var text = this.textarea.value;
    var pos = this.textarea.selectionStart;
    var pre = text.substring(0, pos - 1);
    var post = text.substring(pos);
    this.textarea.value = pre + post;
    goto_char(pos - 1);
  }

  _selectLineTail(){
    var from = this.textarea.selectionStart;
    var to = getEndOfLine();
    this.textarea.setSelectionRange(from, to);
  }

  _isTokenElem(ch){
    return /^[a-zA-Z0-9_]$/.test(ch);
  }

  _getBeginningOfToken(){
    var text = this.textarea.value;
    var point = this.textarea.selectionStart;
    var to = null;
    var ch;
    for(var i=point-1; i>=0; i--){
      if( ! isTokenElem(text.charAt(i))){
        to = i+1;
        break;
      }
    }
    if(to === null){
      to = 0;
    }
    return to;
  }

  _getEndOfToken(){
    var point = this.textarea.selectionStart;
    var text = this.textarea.value;
    var to = null;
    for(var i=point; i<=text.length; i++){
      if( ! isTokenElem(text.charAt(i))){
        to = i;
        break;
      }
    }
    if(to === null){
      to = text.length;
    }
    return to;
  }

  _selectCuurentToken(){
    puts("selectCuurentToken");
    var from = getBeginningOfToken();
    var to = getEndOfToken();
    this.textarea.setSelectionRange(from, to);
  }

  static indent(text, indentStr){
    const lines = text.split("\n");

    return lines
      .map((line, i)=>{
        if( i === lines.length - 1 && line === "" ){
          return "";
        }else{
          return indentStr + line;
        }
      })
      .join("\n");
  }

  static unindentSpace(text) {
    const lines = text.split("\n");

    return lines
        .map((line)=>{
          if(line.match(/^\t/)){
            const m = line.match(/^(\t+)(.*)$/);
            const tabs = m[1];
            const rightContext = m[2];
            return tabs.replace(/\t/g, "        ") + rightContext;
          }else{
            return line;
          }
        })
        .map((line)=>{
          return line.replace(/^ /, "");
        })
        .join("\n");
  }

}
