var MyJSON = {};

function isArray(arg){
  return Object.prototype.toString.call(arg) === '[object Array]';
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
  }else if(isArray(arg)){
    return '[' + arg.map(stringify).join(', ') + ']';
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
