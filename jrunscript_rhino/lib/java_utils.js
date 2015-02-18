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

  /**
   * @param javaList {java.util.List}
   */
  __.list2ary = function(javaList){
    var xs = [];
    for(var i=0; i<javaList.size(); i++){
      xs.push(javaList.get(i));
    }
    return xs;
  };

  __.map2obj = function(javaMap){
    var obj = {};
    var k;
    for(var it = javaMap.keySet().iterator(); it.hasNext();){
      k = it.next();
      obj[k] = javaMap.get(k);
    }
    return obj;
  };

  return __;
})();

exports.JavaUtils = JavaUtils;
