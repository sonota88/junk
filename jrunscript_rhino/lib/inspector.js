var tabSize = 4;

function _any(obj, proc){
  var result = false;
  if(isArray(obj)){
    obj.forEach(
      function(e, i){
        if(proc(e)){
          result = true;
        }
      });
  }else if(isObject(obj)){
    for(var k in obj){
      if(proc(obj[k])){
        result = true;
      }
    }
  }else{
    throw "invalid argument";
  }
  return result;
}

function times(n, proc){
  for(var a=0; a<n; a++){
    proc(a);
  }
}

function repeatSpace(n){
  var str = "";
  times(n, function(a){
          str += " ";
        });
  return str;
}

function escapeStr(s){
  return s
    .replace(/\\/g, "\\\\")
    .replace(/\t/g, "\\t")
    .replace(/\r/g, "\\r")
    .replace(/\n/g, "\\n")
    .replace(/"/g, '\\"')
  ;
}

function isArray(obj){
  return (obj instanceof Array);
}
function isObject(obj){
  return (typeof obj === "object");
}

function isNested(obj){
  return _any(
    obj, function(e){
      return isArray(e) || isObject(e);
    });
}

function inspect(obj, depth, opts){
  if(typeof depth !== "number"){
    throw "invalid depth " + depth;
  }
  if(depth > 8){ return "(too deep)"; }

  if(obj === null){
    return "null";
  }
  
  if(typeof obj === "undefined"){

    return "undefined";

  }else if(typeof obj === "string"){

    var s = "";
    // if(obj.length > 32){
    //   s = obj.substring(0, 32) + "...";
    // }else{
      s = obj;
    // }
    return '"'+ escapeStr(s) + '"';

  }else if(typeof obj === "number"){

    return "" + obj;

  }else if(typeof obj === "function"){

    return "function";

  }else if(isArray(obj)){

    var doPrettify = (opts && opts.pretty) && isNested(obj);
    
    var indentFirst = "";
    var indentRest = "";
    var indentClose = "";
    if(doPrettify){
      indentFirst = repeatSpace((depth+1) * tabSize);
      indentRest = repeatSpace((depth+1) * tabSize - 2);
      indentClose = repeatSpace(depth * tabSize);
    }

    var str = "[";
    for(var a=0,len=obj.length;a<len; a++ ){
      if(a === 0){
        if(doPrettify){ str += "\n" + indentFirst; }
      }else{
        if(doPrettify){ str += "\n" + indentRest; }
        str += ", ";
      }
      str += inspect(obj[a], depth + 1, opts);
    }
    if(doPrettify){ str += "\n" + indentClose; }
    return str + "]";

  }else if(isObject(obj)){

    var doPrettify = (opts && opts.pretty) && isNested(obj);
    
    var indentFirst = "";
    var indentRest = "";
    var indentClose = "";
    if(doPrettify){
      indentFirst = repeatSpace((depth+1) * tabSize);
      indentRest = repeatSpace((depth+1) * tabSize - 2);
      indentClose = repeatSpace(depth * tabSize);
    }

    var str = "{";
    var a = 0;
    for(var k in obj){
      if(a === 0){
        if(doPrettify){ str += "\n" + indentFirst; }
      }else{
        if(doPrettify){ str += "\n" + indentRest; }
        str += ", ";
      }
      str += "" + k + ": ";
      str += inspect(obj[k], depth + 1, opts);
      a++;
    }
    if(doPrettify){ str += "\n" + indentClose; }
    str += "}";
    return str;
  }

  throw "inspect failed (" + obj + ":" + (typeof obj) + ")";
}

//----------------------

function _inspect(obj){
  return inspect(obj, 0, {pretty: false});
}

function _prettyInspect(obj){
  return inspect(obj, 0, {pretty: true});
}

// ----------------

function _print(obj){
  println(_inspect(obj));
}
function _prettyPrint(obj){
  println(_prettyInspect(obj));
}

// ----------------

var inspector = {};
inspector.p = _print;
inspector.pp = _prettyPrint;
inspector.prettyInspect = _prettyInspect;
inspector.inspect = _inspect;

exports.inspector = inspector;
