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

    while (tail.length > 0) {
      if (/^([a-zA-Z0-9_]+)/.test(tail)) {
        tail.match(/^([a-zA-Z0-9_]+)/);
        const tok = RegExp.$1;
        tail = RegExp.rightContext;
        if (tok.startsWith(target) && tok !== target) {
          ts.push(tok);
        }
      } else {
        tail = tail.substring(1);
      }
    }

    return ts;
  }

  ,extractTokens_v2: (text, target)=>{
    const ts = [];
    let tail = text;

    while (tail.length > 0) {
      const m = tail.match(/^(.*?)([a-zA-Z0-9_]+)/m)
      if (m == null) { break; }

      const tok = m[2];
      if (tok.startsWith(target) && tok !== target) {
        ts.push(tok);
      }
      tail = tail.substring(m[0].length);
    }

    return ts;
  }

  ,prepareCandidateTokens: (
    searchRangeBefore, searchRangeAfter, curTok
  )=>{
    const feat = features.dabbrev_expand;

    // 前方からトークンを抽出
    let ts = feat.extractTokens_v2(searchRangeBefore, curTok);

    const cts = [];
    let _tok;

    // 重複排除＋近い方から追加
    for (let i=ts.length-1; i>=0; i--) {
      _tok = ts[i];
      if (cts.indexOf(_tok) >= 0) {
        continue;
      }
      cts.push(_tok);
    }

    // 後方からトークンを抽出
    ts = feat.extractTokens_v2(searchRangeAfter, curTok);

    // 重複排除＋近い方から追加
    for (let i=0, len=ts.length; i<len; i++) {
      _tok = ts[i];
      if (cts.indexOf(_tok) >= 0) {
        continue;
      }
      cts.push(_tok);
    }

    if (cts.length === 0) {
      return [];
    }

    cts.push(curTok);

    return cts;
  }

  ,getBeginningOfCurrentToken: (cb)=>{
    const former = cb.getText(0, cb.getPoint());

    // 現在入力途中の単語の最初
    let begOfCur;
    for (let i=cb.getPoint() - 1; i>=0; i--) {
      if (! cb.isTokenElem(former.charAt(i))) {
        begOfCur = i + 1;
        break;
      }
    }
    if (! begOfCur) {
      // テキスト先頭まで現在のトークン
      return null;
    }
    if (begOfCur === cb.getPoint()) {
      // 入力途中のトークンなし
      return null;
    }

    return begOfCur;
  }

  ,exec: (cb)=>{
    const feat = features.dabbrev_expand;

    if (! featureParams.dabbrev_expand) {
      featureParams.dabbrev_expand = {
        beg: null
        ,cts: [] // candidate tokens
        ,i: 0
      };
    }
    // feature params
    const fp = featureParams.dabbrev_expand;

    const begOfCur = feat.getBeginningOfCurrentToken(cb);
    if (begOfCur === null) {
      return;
    }

    // 現在入力途中の単語
    const curTok = cb.getText(begOfCur, cb.getPoint());

    const keyHist = cb.keyHistory.filter((cmd)=>{
      return cmd != null;
    });

    // 直前のキー入力が S-SPC
    //   → begOfCur, cts をそのまま使う（キャッシュを使う）
    // 直前のキー入力が S-SPC ではない
    //   → begOfCur, cts を作りなおす
    const changed = (keyHist[keyHist.length - 2] !== 'S-SPC');
    if (changed) {
      fp.beg = begOfCur;
      const searchRangeBefore = cb.getText(0, begOfCur);
      const searchRangeAfter = cb.getText(cb.getPoint(), cb.$el.val().length);
      fp.cts = feat.prepareCandidateTokens(
        searchRangeBefore, searchRangeAfter, curTok);
      fp.i = 0;
    }
    if (fp.cts.length === 0) {
      return;
    }

    let nextIndex = fp.i + 1;
    if (nextIndex >= fp.cts.length) {
      nextIndex = 0;
    }
    // 候補
    const cand = fp.cts[fp.i];
    cb.setRegion(fp.beg, cb.getPoint());
    cb.modifyRegion((sel)=>{
      return cand;
    });
    cb.goto_char(begOfCur + cand.length);
    fp.i = nextIndex;
  }
};

////////////////////////////////

const keyCodeMap = {
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

class ChottoBuffer {

  constructor(el){
    this.$el = $(el);
    this.el = el;
    
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
    this.cmd = "";
  }

  move_end_of_line(){
    const to = this.getEndOfLine();
    this.goto_char(to);
  }

  backward_delete_char(){
    const text = this.val();
    const pos = this.getPoint();
    const pre = text.substring(0, pos - 1);
    const post = text.substring(pos);
    this.val(pre + post);
    this.goto_char(pos - 1);
  }

  kill_line(){
    const from = this.getPoint();
    const to = this.getEndOfLine();
    const val = this.val();
    // this.killRing.push(val.substring(from, to));
    this.val(
      val.substring(0, from) + val.substring(to)
    );
    this.goto_char(from);
  }

  // ----------------
  // My commands

  kyMoveBeginningOfLine(){
    const point = this.getPoint();
    let to = null;

    if(this.isBeginningOfLine(point)){
      to = this.getBeginningOfLineIgnoreSpace();
    }else{
      to = this.getBeginningOfLine();
    }

    this.goto_char(to);
  }

  selectCurrentToken(){
    const from = this.getBeginningOfToken();
    const to = this.getEndOfToken();
    this.el.setSelectionRange(from, to);
  }

  kyDelete(){
    if(this.region_active_p()){
      this.deleteRegion();
    }else{
      this.delete_char();
    }
  }

  kySpace(){
    if(this.region_active_p()){
      this.modifyRegion((sel)=>{
        return this.indent(sel, " ");
      });
    }else{
      this.insert(" ");
    }
  }

  unindentRegionBySpace(){
    if( ! this.region_active_p()){
      return;
    }
    this.modifyRegion((sel)=>{
      return this.unindentSpace(sel);
    });
  }

  kyShiftSpace(){
    if(this.region_active_p()){
      this.unindentRegionBySpace(this);
    }else{
      this.dabbrev_expand();
    }
  }

  upcaseRegion(){
    if( ! this.region_active_p()){
      return;
    }
    this.modifyRegion((sel)=>{
      return sel.toUpperCase();
    });
  }

  downcaseRegion(){
    if( ! this.region_active_p()){
      return;
    }
    this.modifyRegion((sel)=>{
      return sel.toLowerCase();
    });
  }

  kyRotateCase(){
    const capitalize = (str)=>{
      return str.substring(0, 1).toUpperCase()
          + str.substring(1).toLowerCase()
      ;
    }

    if( ! this.region_active_p()){
      this.selectCurrentToken(this);
    }
    this.modifyRegion((sel)=>{
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
    puts(this, ev, ev.keyCode);

    if (this.cmd.length > 0) { this.cmd += " "; }
    if (ev.ctrlKey ) { this.cmd += "C-"; }
    if (ev.altKey  ) { this.cmd += "M-"; }
    if (ev.shiftKey) { this.cmd += "S-"; }

    if (ev.keyCode in keyCodeMap) {
      this.cmd += keyCodeMap[ev.keyCode];
    } else {
      this.cmd = "";
    }

    this.keyHistory.push(this.cmd === "" ? null : this.cmd);
    if (this.keyHistory.length > 4) {
      this.keyHistory.shift();
    }

    const fn = this.keyBind[this.cmd];
    if (fn) {
      ev.preventDefault();
      fn.apply(this, [this, ev]);
      this.cmd = "";
    }
  }

  // ----------------

  /**
   * 選択範囲を加工する
   */
  modifyRegion(fn){
    if( ! this.region_active_p()){
      return;
    }
    const posStart = this.el.selectionStart;
    const posEnd = this.el.selectionEnd;

    const orig = this.val();
    const pre = orig.substr(0, posStart);
    const sel = orig.substring(posStart, posEnd);
    const post = orig.substr(posEnd);
    const modified = fn(sel);
    this.val(pre + modified + post);
    this.el.setSelectionRange(posStart, posStart + modified.length);
    this.focus();
  }

  delete_char(){
    const text = this.val();
    const pos = this.getPoint();
    const pre = text.substring(0, pos);
    const post = text.substring(pos + 1);
    this.val(pre + post);

    this.goto_char(pos);
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

  setRegion(from, to){
    this.el.setSelectionRange(from, to);
  }

  /**
   * @return 最も近い前方の改行の次、またはテキストの最初
   */
  getBeginningOfLine(){
    const text = this.val();
    const point = this.getPoint();
    let to = null;
    for(let i=point-1; i>=0; i--){
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
    const text = this.val();
    const point = this.getPoint();
    let to = null;
    let ch;
    for(let i=point; i<=text.length; i++){
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
    const point = this.getPoint();
    const text = this.val();
    let to = null;
    for(let i=point; i<=text.length; i++){
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
    const text = this.val();
    const point = this.getPoint();
    let to = null;
    let ch;
    for(let i=point-1; i>=0; i--){
      if( ! this.isTokenElem(text.charAt(i))){
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
    const text = this.val();
    const point = this.getPoint();
    let to = null;
    for(let i=point; i<=text.length; i++){
      if( ! this.isTokenElem(text.charAt(i))){
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
    const beg = this.region_beginning();

    this.modifyRegion((sel)=>{
      return "";
    });

    this.goto_char(beg);
  }

  insert(str){
    const text = this.val();
    const pos = this.getPoint();
    const pre = text.substring(0, pos);
    const post = text.substring(pos);
    this.val(pre + str + post);

    this.goto_char(pos + str.length);
  }

  indent(text, indentStr){
    const lines = text.split("\n");

    return lines.map((line, i)=>{
      if( i === lines.length - 1 && line === "" ){
        return "";
      }else{
        return indentStr + line;
      }
    }).join("\n");
  }

  unindentSpace(text){
    const lines = text.split("\n");

    return lines.map((line)=>{
      if(line.match(/^\t/)){
        line.match(/^(\t+)(.*)$/);
        const tabs = RegExp.$1;
        const rightContext = RegExp.$2;
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
