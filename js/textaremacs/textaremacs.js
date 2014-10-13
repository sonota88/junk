// function puts(){
//   console.log.apply(console, arguments);
// }

var Textaremacs = (function(){

  // ----------------
  // Emacs commands

  function move_beginning_of_line(me){
    var point = me.getPoint();
    var to = null;

    if(me.isBeginningOfLine(point)){
      to = me.getBeginningOfLineIgnoreSpace();
    }else{
      to = me.getBeginningOfLine();
    }

    me.goto_char(to);
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

  // ----------------
  // My commands

  function selectRestOfLine(me){
    var from = me.getPoint();
    var to = me.getEndOfLine();
    me.el.setSelectionRange(from, to);
  }

  function selectCurrentToken(me){
    var from = me.getBeginningOfToken();
    var to = me.getEndOfToken();
    me.el.setSelectionRange(from, to);
  }

  function deleteChars(me){
    if(me.isMarkActive()){
      me.deleteRegion();
    }else{
      me.delete_char();
    }
  }

  // ----------------

  function THIS($el){
    this.$el = $el;
    this.el = this.$el.get(0);
    
    this.cmd = "";
    
    this.$el.on("keydown", this.dispatch.bind(this));
  }

  THIS.keyCodeMap = {
    32: "SPC"
    ,65: "a"
    ,68: "d"
    ,69: "e"
    ,72: "h"
    ,75: "k"
  };

  THIS.keyBind = {
    "C-a": move_beginning_of_line
    ,"C-d": deleteChars
    ,"C-e": move_end_of_line
    ,"C-h": backward_delete_char
    ,"C-k": selectRestOfLine
    ,"C-M-SPC": selectCurrentToken
  };

  THIS.prototype.dispatch = function(ev){
    var me = this;
    // puts(this, ev);

    if(ev.ctrlKey){
      me.cmd += "C-";
    }
    if(ev.altKey){
      me.cmd += "M-";
    }

    if(ev.keyCode in THIS.keyCodeMap){
      me.cmd += THIS.keyCodeMap[ev.keyCode];
    }

    me.execCommand(ev);
  };

  THIS.prototype.execCommand = function(ev){
    var me = this;
    var fn = THIS.keyBind[me.cmd];
    if(fn){
      ev.preventDefault();
      fn.apply(me, [me, ev]);
    }
    me.cmd = "";
  };

  // ----------------

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
    var point = me.el.selectionEnd;
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

  THIS.prototype.isMarkActive = function(){
    return this.el.selectionStart != this.el.selectionEnd;
  };

  THIS.prototype.deleteRegion = function(){
    var me = this;

    var pos1 = me.el.selectionStart;
    var pos2 = me.el.selectionEnd;
    var from, to;
    if(pos1 < pos2){
      from = pos1;
      to = pos2;
    }else{
      from = pos2;
      to = pos1;
    }

    var text = me.val();
    var pre = text.substring(0, from);
    var post = text.substring(to);
    me.val(pre + post);

    me.goto_char(from);
  };

  return THIS;
})();
