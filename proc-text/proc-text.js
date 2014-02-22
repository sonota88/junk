function puts(){ console.log(arguments); } 

function _id(id){
  return document.getElementById(id);
}

function main(){
  var srcElem = _id("src");
  var resultElem = _id("result");
  var procElem = _id("proc");
  var convertButton = _id("convert");

  function getSrcLines(){
    var src = srcElem.value;
    var lines = [];
    var pos, head, tail;
    while(true){
      pos = src.indexOf("\n");
      if(pos === -1){
        break;
      }
      head = src.substr(0, pos+1);
      tail = src.substr(pos+1, src.length);
      lines.push(head);

      src = tail;
    }
    if(src.length > 0){
      lines.push(src);
    }

    return lines;
  }

  function each(list, func){
    for(var a=0,len=list.length; a<len; a++){
      func(list[a], a);
    }
  }

  function convert(){
    try{
      var srcLines = getSrcLines();
      eval(" function proc(src, lines){ "
           + " var result = ''; "
           + procElem.value
           + " return result; "
           + " } "
          );
      resultElem.value = proc(srcElem.value, srcLines);
    }catch(ex){
      // console.dir(ex);
      resultElem.value = ex.toString();
    }
  }

  convertButton.addEventListener("click", convert, false);

  // Ctrl+Enter でも変換実行
  var KEYCODE_ENTER = 13;
  procElem.addEventListener(
    "keydown"
    , function(ev){
      if(ev.ctrlKey
         && ev.keyCode == KEYCODE_ENTER)
      {
        convert();
      }
    }, false);
}

window.onload = main;
