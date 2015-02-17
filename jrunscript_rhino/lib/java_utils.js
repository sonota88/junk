var JavaUtils = (function(){

  "use strict";

  var __ = {};

  __.toJavaString = function(javaScriptString){
    return new java.lang.String(javaScriptString);
  };

  __.createArray = function(type, size){
    var _type;
    if(type === "Byte"){
      _type = java.lang.Byte.TYPE;
    }else{
      _type = type;
    }
    return java.lang.reflect.Array.newInstance(_type, size);
  };

  return __;
})();

exports.JavaUtils = JavaUtils;
