function puts(){
  // console.log.apply(console, arguments);
}

var Textaremacs = (function(){

  var features = {};

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

  function kyShiftSpace(me){
    if(me.region_active_p()){
      unindentRegionBySpace(me);
    }else{
      me.dabbrev_expand();
    }
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
    this.keyHistory = [];
    this.featureParams = {};
    
    this.$el.on("keydown", this.dispatch.bind(this));
  }
  var _proto_ = THIS.prototype;

  THIS.keyCodeMap = {
    9: "TAB"
    ,13: "RET"
    ,32: "SPC"
    ,38: "<up>"
    ,40: "<down>"
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
    ,"S-SPC": kyShiftSpace
    ,"C-M-SPC": selectCurrentToken
    ,"C-S-<up>": upcaseRegion
    ,"C-S-<down>": downcaseRegion
  };

  THIS.prototype.dispatch = function(ev){
    var me = this;
    puts(this, ev, ev.keyCode);

    if(me.cmd.length > 0){ me.cmd += " "; }
    if(ev.ctrlKey ){ me.cmd += "C-"; }
    if(ev.altKey  ){ me.cmd += "M-"; }
    if(ev.shiftKey){ me.cmd += "S-"; }

    if(ev.keyCode in THIS.keyCodeMap){
      me.cmd += THIS.keyCodeMap[ev.keyCode];
    }else{
      me.cmd = "";
    }

    me.keyHistory.push(me.cmd === "" ? null : me.cmd);
    if(me.keyHistory.length > 4){
      me.keyHistory.shift();
    }

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

  function startsWith(str, pat){
    return str.indexOf(pat) === 0;
  }

  _proto_.dabbrev_expand = function(){
    features.dabbrev_expand.exec(this);
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

  _proto_.getText = function(from, to){
    return this.val().substring(from, to);
  };

  _proto_.setKeybind = function(cmd, fn){
    this.keyBind[cmd] = fn;
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


  ////////////////////////////////
  // Features

  features.dabbrev_expand = {
    extractTokens: function(text, target){
      var ts = [];
      var tail = text;
      var tok;
      while(tail.length > 0){
        if(tail.match(/^([a-zA-Z0-9_]+)/)){
          tok = RegExp.$1;
          tail = RegExp.rightContext;
          if(startsWith(tok, target) && tok !== target){
            ts.push(tok);
          }
        }else{
          tail = tail.substring(1);
        }
      }
      return ts;
    }

    ,prepareCandidateTokens: function(
      searchRangeBefore, searchRangeAfter, curTok
    ){
      // 前方からトークンを抽出
      var ts = this.extractTokens(searchRangeBefore, curTok);

      var cts = [];
      var _tok;

      // 重複排除＋近い方から追加
      for(var i=ts.length-1; i>=0; i--){
        _tok = ts[i];
        if(cts.indexOf(_tok) >= 0){
          continue;
        }
        cts.push(_tok);
      }

      // 後方からトークンを抽出
      ts = this.extractTokens(searchRangeAfter, curTok);

      // 重複排除＋近い方から追加
      var len = ts.length;
      for(i=0; i<len; i++){
        _tok = ts[i];
        if(cts.indexOf(_tok) >= 0){
          continue;
        }
        cts.push(_tok);
      }

      if(cts.length === 0){
        return [];
      }

      cts.push(curTok);

      return cts;
    }

    ,getBeginningOfCurrentToken: function(me){
      var former = me.getText(0, me.getPoint());

      // 現在入力途中の単語の最初
      var begOfCur;
      for(var i=me.getPoint() - 1; i>=0; i--){
        if( ! me.isTokenElem(former.charAt(i))){
          begOfCur = i + 1;
          break;
        }
      }
      if( ! begOfCur){
        // テキスト先頭まで現在のトークン
        return null;
      }
      if(begOfCur === me.getPoint()){
        // 入力途中のトークンなし
        return null;
      }

      return begOfCur;
    }

    ,exec: function(me){
      if( ! me.featureParams.dabbrev_expand){
        me.featureParams.dabbrev_expand = {
          beg: null
          ,cts: [] // candidate tokens
          ,i: 0
        };
      }
      // feature params
      var fp = me.featureParams.dabbrev_expand;

      var begOfCur = this.getBeginningOfCurrentToken(me);
      if(begOfCur === null){
        return;
      }

      // 現在入力途中の単語
      var curTok = me.getText(begOfCur, me.getPoint());

      // 直前のキー入力が S-SPC
      //   → begOfCur, cts をそのまま使う（キャッシュを使う）
      // 直前のキー入力が S-SPC ではない
      //   → begOfCur, cts を作りなおす
      var changed = (me.keyHistory[me.keyHistory.length - 2] !== 'S-SPC');
      if(changed){
        fp.beg = begOfCur;
        var searchRangeBefore = me.getText(0, begOfCur);
        var searchRangeAfter = me.getText(me.getPoint(), me.$el.val().length);
        fp.cts = this.prepareCandidateTokens(
          searchRangeBefore, searchRangeAfter, curTok);
        fp.i = 0;
      }
      if(fp.cts.length === 0){
        return;
      }

      var next_i = fp.i + 1;
      if(next_i >= fp.cts.length){
        next_i = 0;
      }
      // 候補
      var cand = fp.cts[fp.i];
      me.el.setSelectionRange(fp.beg, me.getPoint());
      me.modifyRegion(function(sel){
        return cand;
      });
      me.goto_char(begOfCur + cand.length);
      fp.i = next_i;
    }
  };


  return THIS;
})();
