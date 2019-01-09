const puts = (...args)=>{
  // console.log.apply(console, args);
};

////////////////////////////////
// Features

const features = {};
const featureParams = {};

features.dabbrev_expand = {
  extractTokens: (text, target)=>{
    const ts = [];
    let tail = text;

    while(tail.length > 0){
      if(/^([a-zA-Z0-9_]+)/.test(tail)){
        tail.match(/^([a-zA-Z0-9_]+)/);
        const tok = RegExp.$1;
        tail = RegExp.rightContext;
        if(tok.startsWith(target) && tok !== target){
          ts.push(tok);
        }
      }else{
        tail = tail.substring(1);
      }
    }
    return ts;
  }

  ,prepareCandidateTokens: (
    searchRangeBefore, searchRangeAfter, curTok
  )=>{
    const feat = features.dabbrev_expand;

    // 前方からトークンを抽出
    let ts = feat.extractTokens(searchRangeBefore, curTok);

    const cts = [];
    let _tok;

    // 重複排除＋近い方から追加
    for(let i=ts.length-1; i>=0; i--){
      _tok = ts[i];
      if(cts.indexOf(_tok) >= 0){
        continue;
      }
      cts.push(_tok);
    }

    // 後方からトークンを抽出
    ts = feat.extractTokens(searchRangeAfter, curTok);

    // 重複排除＋近い方から追加
    const len = ts.length;
    for(let i=0; i<len; i++){
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

  ,getBeginningOfCurrentToken: (me)=>{
    const former = me.getText(0, me.getPoint());

    // 現在入力途中の単語の最初
    let begOfCur;
    for(let i=me.getPoint() - 1; i>=0; i--){
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

  ,exec: (cb)=>{
    const feat = features.dabbrev_expand;

    if( ! featureParams.dabbrev_expand){
      featureParams.dabbrev_expand = {
        beg: null
        ,cts: [] // candidate tokens
        ,i: 0
      };
    }
    // feature params
    const fp = featureParams.dabbrev_expand;

    const begOfCur = feat.getBeginningOfCurrentToken(cb);
    if(begOfCur === null){
      return;
    }

    // 現在入力途中の単語
    const curTok = cb.getText(begOfCur, cb.getPoint());

    // 直前のキー入力が S-SPC
    //   → begOfCur, cts をそのまま使う（キャッシュを使う）
    // 直前のキー入力が S-SPC ではない
    //   → begOfCur, cts を作りなおす
    const changed = (cb.keyHistory[cb.keyHistory.length - 2] !== 'S-SPC');
    if(changed){
      fp.beg = begOfCur;
      const searchRangeBefore = cb.getText(0, begOfCur);
      const searchRangeAfter = cb.getText(cb.getPoint(), cb.$el.val().length);
      fp.cts = feat.prepareCandidateTokens(
        searchRangeBefore, searchRangeAfter, curTok);
      fp.i = 0;
    }
    if(fp.cts.length === 0){
      return;
    }

    let next_i = fp.i + 1;
    if(next_i >= fp.cts.length){
      next_i = 0;
    }
    // 候補
    const cand = fp.cts[fp.i];
    cb.el.setSelectionRange(fp.beg, cb.getPoint());
    cb.modifyRegion((sel)=>{
      return cand;
    });
    cb.goto_char(begOfCur + cand.length);
    fp.i = next_i;
  }
};

class ChottoBuffer {

  constructor($el){
    this.$el = $el;
    this.el = this.$el.get(0);
    
    this.cmd = "";
    // this.killRing = [];
    this.keyHistory = [];
    
    this.$el.on("keydown", this.dispatch.bind(this));

    this.kyRotateCase_orig = "";
    this.kyRotateCase_status = "original";

    this.keyBind = {
      "C-a"         : ()=>{ this.kyMoveBeginningOfLine(); }
      ,"C-d"        : ()=>{ this.kyDelete             (); }
      ,"C-e"        : ()=>{ this.move_end_of_line     (); }
      ,"C-g"        : ()=>{ this.keyboard_quit        (); }
      ,"C-h"        : ()=>{ this.backward_delete_char (); }
      ,"C-k"        : ()=>{ this.kill_line            (); }
      ,"M-l"        : ()=>{ this.kyRotateCase         (); }
      ,"SPC"        : ()=>{ this.kySpace              (); }
      ,"S-SPC"      : ()=>{ this.kyShiftSpace         (); }
      ,"C-M-SPC"    : ()=>{ this.selectCurrentToken   (); }
      ,"C-S-<up>"   : ()=>{ this.upcaseRegion         (); }
      ,"C-S-<down>" : ()=>{ this.downcaseRegion       (); }
    };
  }

  // ----------------
  // Emacs commands

  keyboard_quit(){
    const me = this;

    me.cmd = "";
  }

  move_end_of_line(){
    const me = this;

    var to = me.getEndOfLine();
    me.goto_char(to);
  }

  backward_delete_char(){
    const me = this;

    var text = me.val();
    var pos = me.getPoint();
    var pre = text.substring(0, pos - 1);
    var post = text.substring(pos);
    me.val(pre + post);
    me.goto_char(pos - 1);
  }

  kill_line(){
    const me = this;

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

  kyMoveBeginningOfLine(){
    const me = this;

    var point = me.getPoint();
    var to = null;

    if(me.isBeginningOfLine(point)){
      to = me.getBeginningOfLineIgnoreSpace();
    }else{
      to = me.getBeginningOfLine();
    }

    me.goto_char(to);
  }

  selectCurrentToken(){
    const me = this;

    var from = me.getBeginningOfToken();
    var to = me.getEndOfToken();
    me.el.setSelectionRange(from, to);
  }

  kyDelete(){
    const me = this;

    if(me.region_active_p()){
      me.deleteRegion();
    }else{
      me.delete_char();
    }
  }

  kySpace(){
    const me = this;

    if(me.region_active_p()){
      me.modifyRegion((sel)=>{
        return me.indent(sel, " ");
      });
    }else{
      me.insert(" ");
    }
  }

  unindentRegionBySpace(){
    const me = this;

    if( ! me.region_active_p()){
      return;
    }
    me.modifyRegion((sel)=>{
      return me.unindentSpace(sel);
    });
  }

  kyShiftSpace(){
    const me = this;

    if(me.region_active_p()){
      this.unindentRegionBySpace(me);
    }else{
      me.dabbrev_expand();
    }
  }

  upcaseRegion(){
    const me = this;

    if( ! me.region_active_p()){
      return;
    }
    me.modifyRegion((sel)=>{
      return sel.toUpperCase();
    });
  }

  downcaseRegion(){
    const me = this;

    if( ! me.region_active_p()){
      return;
    }
    me.modifyRegion((sel)=>{
      return sel.toLowerCase();
    });
  }

  kyRotateCase(){
    const me = this;

    const capitalize = (str)=>{
      return str.substring(0, 1).toUpperCase()
          + str.substring(1).toLowerCase()
      ;
    }

    if( ! me.region_active_p()){
      this.selectCurrentToken(me);
    }
    me.modifyRegion((sel)=>{
      if(this.kyRotateCase_orig.toLowerCase() !== sel.toLowerCase()){
        this.kyRotateCase_orig = sel;
        this.kyRotateCase_status = "original";
      }

      if(this.kyRotateCase_status === "original"){
        this.kyRotateCase_status = "up";
        return sel.toUpperCase();
      }else if(this.kyRotateCase_status === "up"){
        this.kyRotateCase_status = "down";
        return sel.toLowerCase();
      }else if(this.kyRotateCase_status === "down"){
        this.kyRotateCase_status = "capitalized";
        return capitalize(this.kyRotateCase_orig);
      }else if(this.kyRotateCase_status === "capitalized"){
        this.kyRotateCase_status = "original";
        return this.kyRotateCase_orig;
      }else{
        throw new Error("invalid status");
      }
    });
  }

  // ----------------

  dispatch(ev){
    var me = this;
    puts(this, ev, ev.keyCode);

    if(me.cmd.length > 0){ me.cmd += " "; }
    if(ev.ctrlKey ){ me.cmd += "C-"; }
    if(ev.altKey  ){ me.cmd += "M-"; }
    if(ev.shiftKey){ me.cmd += "S-"; }

    if(ev.keyCode in ChottoBuffer.keyCodeMap){
      me.cmd += ChottoBuffer.keyCodeMap[ev.keyCode];
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
  }

  // ----------------

  /**
   * 選択範囲を加工する
   */
  modifyRegion(fn){
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
  }

  delete_char(){
    var me = this;
    var text = me.val();
    var pos = me.getPoint();
    var pre = text.substring(0, pos);
    var post = text.substring(pos + 1);
    me.val(pre + post);

    me.goto_char(pos);
  }

  goto_char(point){
    this.el.setSelectionRange(point, point);
  }

  region_active_p(){
    return this.el.selectionStart != this.el.selectionEnd;
  }

  dabbrev_expand(){
    features.dabbrev_expand.exec(this);
  }

  // ----------------

  getPoint(){
    return this.el.selectionEnd;
  }

  focus(){
    this.el.focus();
  }

  val(){
    if(arguments.length > 0){
      this.el.value = arguments[0];
    }
    return this.el.value;
  }

  getText(from, to){
    return this.val().substring(from, to);
  }

  setKeybind(cmd, fn){
    this.keyBind[cmd] = fn;
  }

  isBeginningOfLine(point){
    return (point === 0 || this.val().charAt(point - 1) === "\n");
  }

  /**
   * @return 最も近い前方の改行の次、またはテキストの最初
   */
  getBeginningOfLine(){
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
  }

  /**
   * @return スペース・タブを無視した行頭の位置
   */
  getBeginningOfLineIgnoreSpace(){
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
  }

  // 改行またはテキストの最後
  getEndOfLine(){
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
  }

  isTokenElem(ch){
    return /^[a-zA-Z0-9_]$/.test(ch);
  }

  getBeginningOfToken(){
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
  }

  getEndOfToken(){
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
  }

  deleteRegion(){
    var me = this;

    var beg = me.region_beginning();

    me.modifyRegion((sel)=>{
      return "";
    });

    me.goto_char(beg);
  }

  insert(str){
    var me = this;
    var text = me.val();
    var pos = me.getPoint();
    var pre = text.substring(0, pos);
    var post = text.substring(pos);
    me.val(pre + str + post);

    me.goto_char(pos + str.length);
  }

  indent(text, indentStr){
    var lines = text.split("\n");
    var len = lines.length;

    return lines.map((line, i)=>{
      if( i === lines.length - 1 && line === "" ){
        return "";
      }else{
        return indentStr + line;
      }
    }).join("\n");
  }

  unindentSpace(text){
    var lines = text.split("\n");

    return lines.map((line)=>{
      if(line.match(/^\t/)){
        line.match(/^(\t+)(.*)$/);
        var tabs = RegExp.$1;
        var rightContext = RegExp.$2;
        return tabs.replace(/\t/g, "        ") + rightContext;
      }else{
        return line;
      }
    }).map((line)=>{
      return line.replace(/^ /, "");
    }).join("\n");
  }

  region_beginning(){
    return Math.min(this.el.selectionStart, this.el.selectionEnd);
  }

  region_end(){
    return Math.max(this.el.selectionStart, this.el.selectionEnd);
  }


}

ChottoBuffer.keyCodeMap = {
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
