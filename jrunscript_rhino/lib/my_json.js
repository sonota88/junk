var MyJSON = {};

function isArray(arg){
  return Object.prototype.toString.call(arg) === '[object Array]';
}

function isJavaObject(arg){
  return typeof arg === "object" && arg.getClass;
}

function escapeStr(s){
  return '"' + s
      .replace(/\\/g, "\\\\")
      .replace(/\t/g, "\\t")
      .replace(/\r/g, "\\r")
      .replace(/\n/g, "\\n")
      + '"'
  ;
}

function stringify(arg){
  if(arg === null
     || arg !== arg // NaN
     || typeof arg === 'undefined'
    ){
    return 'null';
  }else if(typeof arg === 'number'){
    return '' + arg;
  }else if(typeof arg === 'string'){
    return escapeStr(arg);
  }else if(typeof arg === 'boolean'){
    return arg ? 'true' : 'false';
  }else if(typeof arg === 'function'){
    return "<#fn>";
  }else if( isJavaObject(arg) ){
    return "<#" + arg.getClass().getName() + " " + arg.toString() + ">";
  }else if(arg instanceof RegExp){
    return "{}";
  }else if(isArray(arg)){
    var s = '[';
    for(var i=0,len=arg.length; i<len; i++){
      if(i>0){
        s += ", ";
      }
      s += stringify(arg[i]);
    }
    return s + ']';
  }else if(typeof arg === 'object'){
    var s = '{';
    var i=0;
    for(var k in arg){
      if(i>0){
        s += ", ";
      }
      s += escapeStr(k) + ': ' + stringify(arg[k]);
      i++;
    }
    return s + '}';
  }else{
    throw new Error('unknown type');
  }
}

MyJSON.parse = function(json){
  return eval('(' + json + ')');
};

MyJSON.stringify = function(arg){
  return stringify(arg);
};

exports.MyJSON = MyJSON;
