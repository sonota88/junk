function puts(){
  // console.log.apply(console, arguments);
}

var Textaremacs = (function(){

  // ----------------
  // Emacs commands

  function keyboard_quit(me){
    me.cmd = "";
  }

  function move_end_of_line(me){
    var to = me.getEndOfLine();
    me.goto_char(to);
  }

  function backward_delete_char(me){
    var text = me.val();
    var pos = me.getPoint();
    var pre = text.substring(0, pos - 1);
    var post = text.substring(pos);
    me.val(pre + post);
    me.goto_char(pos - 1);
  }

  function kill_line(me){
    var from = me.getPoint();
    var to = me.getEndOfLine();
    var val = me.val();
    // me.killRing.push(val.substring(from, to));
    me.val(
      val.substring(0, from) + val.substring(to)
    );
    me.goto_char(from);
  }

  // ----------------
  // My commands

  function kyMoveBeginningOfLine(me){
    var point = me.getPoint();
    var to = null;

    if(me.isBeginningOfLine(point)){
      to = me.getBeginningOfLineIgnoreSpace();
    }else{
      to = me.getBeginningOfLine();
    }

    me.goto_char(to);
  }

  function selectCurrentToken(me){
    var from = me.getBeginningOfToken();
    var to = me.getEndOfToken();
    me.el.setSelectionRange(from, to);
  }

  function kyDelete(me){
    if(me.region_active_p()){
      me.deleteRegion();
    }else{
      me.delete_char();
    }
  }

  function kySpace(me){
    if(me.region_active_p()){
      me.modifyRegion(function(sel){
        return me.indent(sel, " ");
      });
    }else{
      me.insert(" ");
    }
  }

  function unindentRegionBySpace(me){
    if( ! me.region_active_p()){
      return;
    }
    me.modifyRegion(function(sel){
      return me.unindentSpace(sel);
    });
  }

  function upcaseRegion(me){
    if( ! me.region_active_p()){
      return;
    }
    me.modifyRegion(function(sel){
      return sel.toUpperCase();
    });
  }

  function downcaseRegion(me){
    if( ! me.region_active_p()){
      return;
    }
    me.modifyRegion(function(sel){
      return sel.toLowerCase();
    });
  }

  var kyRotateCase_orig = "";
  var kyRotateCase_status = "original";
  function kyRotateCase(me){

    function capitalize(str){
      return str.substring(0, 1).toUpperCase()
          + str.substring(1).toLowerCase()
      ;
    }

    if( ! me.region_active_p()){
      selectCurrentToken(me);
    }
    me.modifyRegion(function(sel){
      if(kyRotateCase_orig.toLowerCase() !== sel.toLowerCase()){
        kyRotateCase_orig = sel;
        kyRotateCase_status = "original";
      }

      if(kyRotateCase_status === "original"){
        kyRotateCase_status = "up";
        return sel.toUpperCase();
      }else if(kyRotateCase_status === "up"){
        kyRotateCase_status = "down";
        return sel.toLowerCase();
      }else if(kyRotateCase_status === "down"){
        kyRotateCase_status = "capitalized";
        return capitalize(kyRotateCase_orig);
      }else if(kyRotateCase_status === "capitalized"){
        kyRotateCase_status = "original";
        return kyRotateCase_orig;
      }else{
        throw new Error("invalid status");
      }
    });
  }

  // ----------------

  function THIS($el){
    this.$el = $el;
    this.el = this.$el.get(0);
    
    this.cmd = "";
    // this.killRing = [];
    
    this.$el.on("keydown", this.dispatch.bind(this));
  }
  var _proto_ = THIS.prototype;

  THIS.keyCodeMap = {
    9: "TAB"
    ,13: "RET"
    ,32: "SPC"
    ,65: "a"
    ,66: "b"
    ,68: "d"
    ,69: "e"
    ,70: "f"
    ,71: "g"
    ,72: "h"
    ,75: "k"
    ,76: "l"
    ,80: "p"
    ,87: "w"
    ,88: "x"
    ,89: "y"
  };

  _proto_.keyBind = {
    "C-a": kyMoveBeginningOfLine
    ,"C-d": kyDelete
    ,"C-e": move_end_of_line
    ,"C-g": keyboard_quit
    ,"C-h": backward_delete_char
    ,"C-k": kill_line
    ,"M-l": kyRotateCase
    ,"SPC": kySpace
    ,"S-SPC": unindentRegionBySpace
    ,"C-M-SPC": selectCurrentToken
    ,"C-S-x": upcaseRegion
    ,"C-S-y": downcaseRegion
  };

  THIS.prototype.dispatch = function(ev){
    var me = this;
    // puts(this, ev);

    if(me.cmd.length > 0){ me.cmd += " "; }
    if(ev.ctrlKey ){ me.cmd += "C-"; }
    if(ev.altKey  ){ me.cmd += "M-"; }
    if(ev.shiftKey){ me.cmd += "S-"; }

    if(ev.keyCode in THIS.keyCodeMap){
      me.cmd += THIS.keyCodeMap[ev.keyCode];
    }else{
      me.cmd = "";
    }
    // puts(me.cmd);

    var fn = me.keyBind[me.cmd];
    if(fn){
      ev.preventDefault();
      fn.apply(me, [me, ev]);
      me.cmd = "";
    }
  };

  // ----------------

  /**
   * 選択範囲を加工する
   */
  THIS.prototype.modifyRegion = function(fn){
    var me = this;
    if( ! me.region_active_p()){
      return;
    }
    var posStart = me.el.selectionStart;
    var posEnd = me.el.selectionEnd;

    var orig = me.val();
    var pre = orig.substr(0, posStart);
    var sel = orig.substring(posStart, posEnd);
    var post = orig.substr(posEnd);
    var modified = fn(sel);
    me.val(pre + modified + post);
    me.el.setSelectionRange(posStart, posStart + modified.length);
    me.focus();
  };

  THIS.prototype.delete_char = function(){
    var me = this;
    var text = me.val();
    var pos = me.getPoint();
    var pre = text.substring(0, pos);
    var post = text.substring(pos + 1);
    me.val(pre + post);

    me.goto_char(pos);
  };

  THIS.prototype.goto_char = function(point){
    this.el.setSelectionRange(point, point);
  };

  _proto_.region_active_p = function(){
    return this.el.selectionStart != this.el.selectionEnd;
  };

  // ----------------

  THIS.prototype.getPoint = function(){
    return this.el.selectionEnd;
  };

  THIS.prototype.focus = function(){
    this.el.focus();
  };

  THIS.prototype.val = function(){
    if(arguments.length > 0){
      this.el.value = arguments[0];
    }
    return this.el.value;
  };

  THIS.prototype.isBeginningOfLine = function(point){
    return (point === 0 || this.val().charAt(point - 1) === "\n");
  };

  /**
   * @return 最も近い前方の改行の次、またはテキストの最初
   */
  THIS.prototype.getBeginningOfLine = function(){
    var me = this;
    var text = me.val();
    var point = me.getPoint();
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
  };

  /**
   * @return スペース・タブを無視した行頭の位置
   */
  THIS.prototype.getBeginningOfLineIgnoreSpace = function(){
    var me = this;
    var text = me.val();
    var point = me.getPoint();
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
  };

  // 改行またはテキストの最後
  THIS.prototype.getEndOfLine = function(){
    var me = this;
    var point = me.getPoint();
    var text = me.val();
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
  };

  THIS.prototype.isTokenElem = function(ch){
    return /^[a-zA-Z0-9_]$/.test(ch);
  };

  THIS.prototype.getBeginningOfToken = function(){
    var me = this;
    var text = me.val();
    var point = me.getPoint();
    var to = null;
    var ch;
    for(var i=point-1; i>=0; i--){
      if( ! me.isTokenElem(text.charAt(i))){
        to = i+1;
        break;
      }
    }
    if(to === null){
      to = 0;
    }
    return to;
  };

  THIS.prototype.getEndOfToken = function(){
    var me = this;
    var text = me.val();
    var point = me.getPoint();
    var to = null;
    for(var i=point; i<=text.length; i++){
      if( ! me.isTokenElem(text.charAt(i))){
        to = i;
        break;
      }
    }
    if(to === null){
      to = text.length;
    }
    return to;
  };

  THIS.prototype.deleteRegion = function(){
    var me = this;

    var beg = me.region_beginning();

    me.modifyRegion(function(sel){
      return "";
    });

    me.goto_char(beg);
  };

  THIS.prototype.insert = function(str){
    var me = this;
    var text = me.val();
    var pos = me.getPoint();
    var pre = text.substring(0, pos);
    var post = text.substring(pos);
    me.val(pre + str + post);

    me.goto_char(pos + str.length);
  };

  THIS.prototype.indent = function(text, indentStr){
    var lines = text.split("\n");
    var len = lines.length;

    return lines.map(function(line, i){
      if( i === lines.length - 1 && line === "" ){
        return "";
      }else{
        return indentStr + line;
      }
    }).join("\n");
  };

  THIS.prototype.unindentSpace = function(text){
    var lines = text.split("\n");

    return lines.map(function(line){
      if(line.match(/^\t/)){
        line.match(/^(\t+)(.*)$/);
        var tabs = RegExp.$1;
        var rightContext = RegExp.$2;
        return tabs.replace(/\t/g, "        ") + rightContext;
      }else{
        return line;
      }
    }).map(function(line){
      return line.replace(/^ /, "");
    }).join("\n");
  };

  _proto_.region_beginning = function(){
    return Math.min(this.el.selectionStart, this.el.selectionEnd);
  };

  _proto_.region_end = function(){
    return Math.max(this.el.selectionStart, this.el.selectionEnd);
  };

  return THIS;
})();
