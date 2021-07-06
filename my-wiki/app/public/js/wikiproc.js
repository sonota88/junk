(()=>{

  const util = {
    HTML_ESCAPE_MAP: {
      "<": "&lt;"
      ,">": "&gt;"
      ,'"': "&quot;"
      // TODO single quote
    }

    ,escapeHTML: (src)=>{
      return src
        .replace(/&/g, "&amp;")
        .replace(/[\&<>]/g, function(m){
          if(m === "&"){
            return "&amp;";
          }else{
            return util.HTML_ESCAPE_MAP[m];
          }
        });
    }
  };

  // --------------------------------

var WikiProc = {
  idTitleMap: null
  ,getTitleByPageId: function(pageId){
    return this.idTitleMap["" + pageId];
  }

  /**
   * absolue: デフォルト
   * relative: 静的HTMLへの変換用
   */
  ,opts: {
    pagePathType: 'absolute' // or 'relative'
  }
};

function makeHeadingId(htitle){
  // MediaWiki style
  return encodeURIComponent(htitle.replace(/ /g, "_"))
    .replace(/%/g, ".");
}


function Elem(type, content){
  this.type = type;
  this.list = [];
  this.content = content; // html
}
Elem.prototype = {
  toHtml: function(){
    if( ! this.type ){
      return this.content ? lineToHtml(this.content) : "";
    }

    var attr = "";
    if(this.attr){
      for(var k in this.attr){
        var v = this.attr[k];
        attr += " " + k + '="' + v + '"';
      }
    }

    var innerHTML;
    if(this.type === "pre"){
      if(this.attr["class"] === "ul"){
        innerHTML = this.content;
      }else{
        // 通常の pre 要素
        innerHTML = util.escapeHTML(this.content);
      }
    }else{
      innerHTML = this.content;
    }

    if(this.type === "hr"){
      return "<" + this.type + " />";
    }else{
      return "<" + this.type + " "
        + attr
        + " >"
        + innerHTML
        + "</" + this.type + ">";
    }
  }
};


function isundef(it){
  return typeof it === "undefined";
}

function _last(xs){
  return xs[xs.length - 1];
}


function makeEMIndex(formatted){
  // Index of emphatic text
  var emReference = "";
  var emphasis = xtag(formatted, "em");
  var emRefElem = null;
  if(emphasis.length > 0){
    for(var a=0,len=emphasis.length; a<len; a++){
      var id = "emphasis_" + a;
      emphasis[a].id = id;
      emReference += '<li><a href="#' + id + '">' + emphasis[a].innerHTML + '</a></li>\n';
    }
    emReference = "<ul>" + emReference + "</ul>";
    emReference = "<h1>Index of emphatic texts</h1>" + emReference;

    emRefElem = createElement(
      null, "div"
      , { id: "emphasis_index" }, {}
      , emReference
    );

    // add to TOC
    var temp = createElement(
      document.getElementById("toc").childNodes[0]
      , "li", {}, {}
      , '<a href="#emphasis_index">Index of emphatic texts</a>'
    );
  }

  return emRefElem;
}


////////////////////////////////
// utils


// function puts(){ console.log(arguments); }


function xtag( elem, tagName ){
  return elem.getElementsByTagName( tagName );
}

function insertAsFirstChild(parent, child){
  parent.insertBefore(child, parent.firstChild);
}


function unshift(first, arr){
  var xs = [ first ];
  for(var a=0,len=arr.length; a<len; a++){
    xs.push(arr[a]);
  }
  return xs;
}


function strip(str){
  if(!str){
    return null;
  }
  return str.replace( /^[\s\t\n\r\n]+/, "" ).replace( /[\s\t\r\n]+$/, "" );
}


function mkStr(str, n){
  return new Array(n+1).join(str);
}


function expandTabs(str){
  return str.replace(
      /\t/g
    , mkStr(" ", 8)
  );
}


////////////////////////////////


var jsonTable = (function(){
  function toHtml(plain){
    return (""+plain).replace(/\n/g, "<br />");
  }

  function jsonToTableForArray(rows){
    // var table = createElement(null, "table", {"class": "from_json"});
    var html = "<table class='from_json'>";
    for(var a=0, lenA=rows.length; a<lenA; a++){
      var row = rows[a];
      // var tr = createElement(table, "tr");
      html += "<tr>";
      for(var b=0, lenB=row.length; b<lenB; b++){
        var col = row[b];
        html += "<td>" + toHtml(col);
        html += "</td>";
      }
      html += "</tr>";
    }
    return html + "</table>";
  }

  function jsonToTableForObject(rows){
    var table = "<table class='from_json'>";
    var colNames = rows[0];

    table += "<tr>";
    for(var a=0, lenA=colNames.length; a<lenA; a++){
      var colName = colNames[a];
      table += "<td>" + toHtml(colName) + "</td>";
    }
    table += "</tr>";

    var row;
    for(var b=1, lenB=rows.length; b<lenB; b++){
      row = rows[b];
      table += "<tr>";
      for(var c=0, lenC=colNames.length; c<lenC; c++){
        var colName = colNames[c];
        var col = row[colName];
        table += "<td>" + toHtml(col) + "</td>";
      }
      table += "</tr>";
    }
    return table + "</table>";
  }


  function jsonToTable(json){
    var rows = JSON.parse(json);
    var isArray = (rows[1] instanceof Array);

    if(isArray){
      return jsonToTableForArray(rows);
    }else{
      return jsonToTableForObject(rows);
    }
  }

  return {
    jsonToTable: jsonToTable
  };
})();


////////////////////////////////


/**
 * String.prototype.indexOf() のように開始位置を指定して検索する。
 */
function searchFrom(str, re, fromIndex){
  var idx = str.substring(fromIndex).search(re);
  if(idx < 0){
    return idx;
  }else{
    return idx + fromIndex;
  }
}

function makePageLink(content){
  var pageId, title;

  if(content.match( /^(\d+):(.+)$/ )){
    pageId = parseInt(RegExp.$1, 10);
    title = RegExp.$2;
  }else{
    pageId = parseInt(content, 10);
    title = WikiProc.getTitleByPageId(pageId);
    if(!title){
      title = "[?]" + content;
    }
  }

  var href;
  switch(WikiProc.opts.pagePathType){
  case "absolute": 
    href = "/page/" + pageId;
    break;
  case "relative": 
    href = "./" + pageId + ".html";
    break;
  default:
    throw new Error("pagePathType not supported (" + WikiProc.opts.pagePathType + ")");
  }
  return '<a href="' + href + '">'
      + util.escapeHTML(title)
      + '</a>';
}

function procInline(line){

  var work = line;
  var els = [];

  // 行頭の処理
  if(      /^\*(.+?)\*( |$)/    .test(work)
        || /^_(.+?)_( |$)/      .test(work)
        || /^`(.+?)`( |$)/      .test(work)
        || /^\[\[(.+?)\]\]( |$)/.test(work)
  ){
    work = " " + work;
  }

  var ws, we; // width start/end

  while(work.length > 0){

    var pos = work.search( /( \*| _| `| \[\[)/ );

    if(pos >= 0){
      els.push( work.substring(0, pos) ); // left context
      work = work.substring(pos);

      if(/^ \*/.test(work)){

        ws = 2; // " *"
        we = 2; // "* "
        var posEnd = searchFrom(work, /\*( |$)/, ws);
        if(posEnd < 0){
          els.push(" *");
          work = work.substring(ws);
        }else{
          els.push('<b>' + work.substring(ws, posEnd) + '</b>');
          work = work.substring(posEnd + we);
        }

      }else if(/^ _/.test(work)){

        ws = 2; // " _"
        we = 2; // "_ "
        var posEnd = searchFrom(work, /_( |$)/, ws);
        if(posEnd < 0){
          els.push(" _");
          work = work.substring(ws);
        }else{
          els.push('<em>' + work.substring(ws, posEnd) + '</em>');
          work = work.substring(posEnd + we);
        }

      }else if(/^ `/.test(work)){

        ws = 2; // " `"
        we = 2; // "` "
        var posEnd = searchFrom(work, /`( |$)/, ws);
        if(posEnd < 0){
          els.push(" `");
          work = work.substring(ws);
        }else{
          els.push('<tt>' + work.substring(ws, posEnd) + '</tt>');
          work = work.substring(posEnd + we);
        }

      }else if(/^ \[\[/.test(work)){

        ws = 3; // " [["
        we = 3; // "]] "
        var posEnd = searchFrom(work, /\]\]( |$)/, ws);
        if(posEnd < 0){
          els.push(" [[");
          work = work.substring(ws);
        }else{
          var content = work.substring(ws, posEnd);
          els.push(makePageLink(content));
          work = work.substring(posEnd + we);
        }

      }
    }else{
      // 行内マークアップがこれ以後存在しない
      els.push(work);
      work = "";
    }
  }

  return els;
}


function lineToHtml(line){

  if( line.match( /^(https?|file):\/\// ) ){
    var content = line;
    try{
      content = decodeURIComponent(line);
    } catch (x) {
      ;
    }
    return '<a href="' + line + '">' + content + '</a>';
  }else if( line.match( /^link: (.+)/ ) ){
    var href = RegExp.$1;
    return '<a href="' + href + '">' + href + '</a>';
  }else if( line.match( /^youtube: ((https?|file):\/\/.+$)/ ) ){
    var url = RegExp.$1;
    url.match(/v=([^&]+)/);
    var ytid = RegExp.$1;
    return ''
        +'<div class="youtube">'
        +'<iframe width="560" height="315" src="//www.youtube.com/embed/' + ytid + '" frameborder="0" allowfullscreen></iframe>'
        + '<br /><a href="' + url + '">' + url + '</a>'
        + '</div>';
  }else if(line.match( /^img: (.+)$/ )){
    return '<img src="' + RegExp.$1 + '" />';
  }else{
    return procInline(line).join("");
  }
};


function Parser(){

  var line2elem = function(line){
    var elem = new Elem();

    if(line.match( /^----/ )){
      elem.type = "hr";
      elem.content = "";
    }else{
      elem.content = line;
    }

    return elem;
  };


  this.getMinHeadSpacesSize = function(lines){
    var firstLine = expandTabs(lines[0]);
    firstLine.match(/^( +)/);
    var minSpaces = RegExp.$1.length;

    for(var a=0,len=lines.length; a<len; a++){
      var line = expandTabs(lines[a]);
      if(line.match(/^(\s*)/)){
        var length = RegExp.$1.length;
        if(length < minSpaces){
          minSpaces = length;
        }
      }
    }
    return minSpaces;
  };


  this.eliminateHeadSpaces = function(lines){
    var minSpacesLength = this.getMinHeadSpacesSize(lines);
    var headSpaces = mkStr(" ", minSpacesLength);
    var headSpacesRE = new RegExp("^" + headSpaces);
    return lines
      .map(function(line){
             return expandTabs(line)
               .replace(headSpacesRE, "");
           });
  };

  function cdr(list){
    return list.slice(1, list.length);
  }

  this.procPRE = function(lines){
    function isIndented(line){
      return /^\s/.test(line);
    }

    var indent = 0; // トップレベルは0
    var temp = [];

    while(lines.length > 0){
      var nextLine = lines[0];

      if( ! isIndented(nextLine) ){
        break;
      }

      var line = lines.shift();
      temp.push(line);
    }

    var _class = "indentBlock";
    if( /^\s+## src ##$/.test(temp[0]) ){
      temp = cdr(temp);
    }else if( temp[0].match(/^\s+## src:(.+) ##$/) ){
      temp = cdr(temp);
      _class += " prettyprint lang-" + RegExp.$1;
    }else{
      ;
    }
    var content;
    if(temp.length > 0){
      content = this.eliminateHeadSpaces(temp).join("\n");
    }else{
      content = "";
    }
    var elem = new Elem(
      "pre"
      ,content
    );
    elem.attr = {
      "class": _class
    };

    return {
      elem: elem
      , lines: lines
    };
  };

  function procTABLE(lines){
    lines.shift();

    var _lines = [];

    var line;
    while(lines.length > 0){
      line = lines.shift();
      if( line.match(/^\}t-*$/) ){
        break;
      }else{
        _lines.push(line);
      }
    }

    var json = _lines.join("\n");
    var table = jsonTable.jsonToTable(json);
    // var elem = new Elem("div", table.outerHTML );
    var elem = new Elem("div", table);

    return {
      elem: elem
      , lines: lines
    };
  }

  function procMermaid(lines){
    lines.shift();

    var _lines = [];

    var line;
    while(lines.length > 0){
      line = lines.shift();
      if( line.match(/^```/) ){
        break;
      }else{
        _lines.push(line);
      }
    }

    var src = _lines.join("\n");
    var elem = new Elem("div", src);
    elem.attr = { "class": "mermaid"};

    return {
      elem: elem
      , lines: lines
    };
  }

  this.parseMain = function(lines){
    var node = { list: [] };
    var result = [];

    var l = null;
    while(lines.length > 0){
      l = lines.shift();

      if( ! l ){
        var elem = new Elem();
        elem.content = null;
        node.list.push(elem);

      }else if( /^\s+## src(:.+)? ##$/.test(l) ){
        var x = this.procPRE(unshift(l, lines));
        var elem = x.elem;
        lines = x.lines;
        node.list.push(elem);
      }else if( /^```mermaid/.test(l) ){
        var x = procMermaid( unshift(l, lines) );
        var elem = x.elem;
        lines = x.lines;
        node.list.push(elem);
      }else if( /^t\{-*$/.test(l) ){
        var x = procTABLE( unshift(l, lines) );
        var elem = x.elem;
        lines = x.lines;
        node.list.push(elem);
      }else if( /^b\{-*$/.test(l) ){
        node.list.push( new Elem(null, '<div class="box">') );
      }else if( /^\}b-*$/.test(l) ){
        node.list.push( new Elem(null, "</div>") );
      }else if( /^q\{-*$/.test(l) ){
        node.list.push( new Elem(null, "<blockquote>") );
      }else if( /^\}q-*$/.test(l) ){
        node.list.push( new Elem(null, "</blockquote>") );
      }else{
        node.list.push( line2elem(l) );
      }
    }

    return { node: node };
  };


  this.parse = function(text){
    var lines = text.split( "\n" ) ;

    var result = this.parseMain( lines );
    return result.node;
  };
}


var Outline = (function(){
  function Outline(parent){
    this.isRoot = (parent == null);
    this.level = this.isRoot ? 0 : parent.level + 1;
    this.kids = [];
    if( ! this.isRoot ){
      parent.kids.push(this);
    }

    this.title = null;
    this.lineno = 1;
  }

  return Outline;
})();


function OutlineParser(){

  this.getBlockTitle = function(content){
    var rawTitle = strip(content);
    var title;
    if(rawTitle.match( /^\[notoc\] (.+)/ )){
      title = strip(RegExp.$1);
    }else{
      title = rawTitle;
    }
    return title;
  };

  this.getBlockShowInToc = function(content){
    return ! /^\[notoc\] (.+)/.test(content);
  };

  /**
   * pre はインデントされているため、
   * 行頭を見ていけば見出しとそれ以外を区別できる。
   */
  this.parse = function(src){
    var lines = src.split("\n");
    var level = 0;
    var index = 1;

    var root = new Outline();
    var current = root;

    var stack = [];
    var buf = [];
    var linenos = [];
    var lineno = 0;
    while(lines.length > 0){
      lineno++;
      var line = lines.shift();

      if(
           line.match(/^(=+)([^=].*?)=*$/)
        || line.match(/^(■+)([^■].*?)$/)
      ){
        if(buf.length > 0){
          current.kids.push(buf.join("\n"));
          buf = [];
        }

        var head = this.procHn(line);
        head.lineno = lineno;
        var delta = head.level - current.level;

        if(0 < delta){
          for(var a=0; a<delta; a++){
            stack.push(current);
            current = new Outline(current);
          }
        }else if(delta < 0){
          for(var a=0; a<-delta; a++){
            stack.pop();
          }
          current = new Outline(_last(stack));
        }else{
          current = new Outline(_last(stack));
        }

        current.index = index; index++;
        current.lineno = head.lineno;
        current.showInToc = this.getBlockShowInToc(head.content);
        current.title = this.getBlockTitle(head.content);

        linenos.push({ lv: head.level, from: lineno, idx: current.index});

      }else{
        buf.push(line);
      }
    }
    var linenoMax = lineno;

    if(buf.length > 0){
      current.kids.push(buf.join("\n"));
    }

    linenos.forEach(function(ln1, i){
      ln1.to = null;

      var nextHead = linenos.filter(function(ln2, i2){
        return i2 > i && ln2.lv <= ln1.lv;
      })[0];

      if(nextHead){
        ln1.to = nextHead.from - 1;
      }else{
        ln1.to = linenoMax;
      }
    });

    function find(idx){
      for(var i=0,len=linenos.length; i<len; i++){
        var ln = linenos[i];
        if(ln.idx === idx){
          return ln;
        }
      }
      return null;
    }

    function addLineRange(sec){
      var ln = find(sec.index);
      if(ln){
        sec.lineFrom = ln.from;
        sec.lineTo = ln.to;
      }
      sec.kids.forEach(function(kid){
        if(typeof kid !== "string"){
          addLineRange(kid);
        }
      });
    }
    addLineRange(root);

    return root;
  };

  this.procHn = function(line){
    if(line.match(/^(=+)([^=].*?)=*$/)){
      return {
        level: RegExp.$1.length
        , content: RegExp.$2
      };
    }else if(line.match(/^(■+)([^■].*?)$/)){
      return {
        level: 4 - RegExp.$1.length
        , content: RegExp.$2
      };
    }else{
      return null;
    }
  };

  this.markup = function(doc){
    var result = "";
    var list = doc.list;

    var elem, nextElem, temp;
    for(var a=0,len=list.length; a<len; a++){
      elem = list[a];
      nextElem = list[a+1];
      temp = elem.toHtml();
      result += temp;

      if( temp === '<div class="box">'
          || temp.match(/^<\/?blockquote/)
          || temp.match(/^<\/?pre/)
          || (elem.type === "hr" || (nextElem && nextElem.type === "hr"))
        ){
          //
        }else{
          //result += "<br />";
          result += "\n";
        }
    }

    return result;
  };

  this.toHTMLElementLeaf = function(block){
    var src = block;
    var parser = new Parser();
    var parsed = parser.parse(src);
    return '<div class="outline">' + this.markup(parsed) + '</div>';
  };

  this.isNode = function(obj){
    return (obj.kids != null);
  };

  this.toHTMLElement = function(block, pageId){
    var html = '<div class="outline">';

    if( block.title ){
      var klass = "";
      if( ! block.showInToc ){
        klass = 'class="notoc"';
      }
      var hid = makeHeadingId(block.title);
      html += '<h' + block.level + ' id="' + hid + '\" ' + klass + ">";

      html += block.title;
      html += ' <a href="#' + hid + '">&para;</a>';

      var lineRange = block.lineFrom + "," + block.lineTo;
      const href = `/page/${pageId}/edit?range=${lineRange}`;
      html += ` <a class="edit_section" href="${href}">edit</a>`;

      html += "</h" + block.level + ">";
    }

    for( var a=0,len=block.kids.length; a<len; a++ ){
      var kid = block.kids[a];
      if(this.isNode(kid)){
        html += this.toHTMLElement(kid, pageId);
      }else{
        html += this.toHTMLElementLeaf(kid, pageId);
      }
    }

    return html + "</div>";
  };
}



function formatDate(date){
  function fmt(n){
    return "" + (n<10 ? "0" + n : n);
  }

  var y = date.getFullYear();
  var m = date.getMonth()+1;
  var d = date.getDate();
  var h = date.getHours();
  var min = date.getMinutes();
  return y
    + "-" + fmt(m)
    + "-" + fmt(d)
    + " " + fmt(h)
    + ":" + fmt(min);
}


function splitPreamble(src){
  var lines = src.split("\n");
  var info = {};
  var preamble_range = 20;
    for(var a=0; a<preamble_range; a++){
      if(!lines[a]){ continue; }
      if(lines[a].match(/^title:(.+)/) ){
        info.title = RegExp.$1;
        delete lines[a];
      }else if(lines[a].match(/^by:(.+)/) ){
        info.by = RegExp.$1;
        delete lines[a];
      }else if(lines[a].match(/^date:(.+)/) ){
        info.date = RegExp.$1;
        delete lines[a];
      }
    }

  var _lines = [];
  for(var a=0,len=lines.length; a<len; a++){
    var line = lines[a];
    typeof line !== "undefined" && _lines.push(line);
  }

  return {
    info: info
    , body: _lines.join("\n")
  };
}


function makePreamble(info){
  var lines = [];
  info.by && lines.push( "by: " + info.by);
  info.date && lines.push( "date: " + info.date );
  lines.push( "last modified: " + formatDate(new Date(document.lastModified)) );

  var preamble = createElement(
    null, "pre"
    , { "class": "preamble" }, {}
    , lines.join("\n")
  );

  return preamble;
}


function getTitle(info){
  var title = "untitled";
  if(info.title){
    title = info.title;
    info.by && (title += " by " + info.by);
    info.date && (title += " (" + info.date + ")");
  }

  return title;
}

////////////////////////////////

function printOutline(ol){
  var ind0 = "";
  var ind1 = "";
  for(var i=0; i<ol.level * 3; i++){
    ind0 += " ";
  }
  for(var i=0; i<(ol.level+1) * 3; i++){
    ind1 += " ";
  }
  function p0(x){
    puts(ind0 + x);
  }
  function p(x){
    puts(ind1 + x);
  }
  p0("{");
  p(ol.level + ":" + ol.title);
  p("index=" + ol.index);
  p("## lineno=" + ol.lineno);
  p("## " + ol.lineFrom + "--" + ol.lineTo);
  // p(ol.children.length);
  ol.children.forEach(function(kid, i){
    if(typeof kid === "string"){
      p("(text)");
    }else{
      printOutline(kid);
    }
  });
  p0("}");
}

function toHTML(pageId, src, idTitleMap, opts){
  WikiProc.idTitleMap = idTitleMap;

  opts = opts || {};
  if(opts.pagePathType) WikiProc.opts.pagePathType = opts.pagePathType;

  var olParser = new OutlineParser();
  var outline = olParser.parse(src);
  // puts(">>================================");
  // printOutline(outline);
  // puts("<<================================");
  var wikiPage = {
    mainContent: olParser.toHTMLElement(outline, pageId)
  };

  return wikiPage;
}

  window.wikiproc = {
    toHTML: toHTML
    ,_OutlineParser: OutlineParser
  };
})();

// exports.wikiproc = {
//   toHTML: toHTML
//   ,_OutlineParser: OutlineParser
// };