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

  __.toStr = function(array, encoding){
    return new java.lang.String(array, encoding || "UTF-8");
  };

  __.indexOf = function(bytes, subset){
     for(var a=0,len1=bytes.length; a<len1; a++){
       for(var b=0,len2=subset.length; b<len2; b++){
         if(bytes[a+b] == subset[b]){
           if(b == subset.length - 1){
             return a;
           }
         }else{
           break;
         }
       }
     }
     return -1;
  };

  return __;
})();

exports.ByteArrayUtils = ByteArrayUtils;
