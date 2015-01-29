var ByteArrayUtils = (function(){
  "use strict";

  var __ = {};

  __.create = function(size){
    return java.lang.reflect.Array.newInstance(
      java.lang.Byte.TYPE, size);
  };

  __.substr = function(array, from, to){
     if( ! to ){
       to = array.length;
     }
     return java.util.Arrays.copyOfRange(array, from, to);
  };

  return __;
})();

exports.ByteArrayUtils = ByteArrayUtils;
