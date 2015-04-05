var Base64 = (function(){

  "use strict";

  var Base64 = {};

  var CHAR_SEQ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
          + "abcdefghijklmnopqrstuvwxyz"
          + "0123456789+/";

  function n2b(n, size){
    var s = "";
    var pow, div;
    for(var d=size - 1; d>=0; d--){
      pow = Math.pow(2, d);
      div = Math.floor(n / pow);
      s += div;
      if(pow <= n) n -= pow;
    }
    return s;
  }

  function b2n(str){
    var n = 0;
    var max = str.length - 1;
    for(var d=0; d<=max; d++){
      if(str.charAt(max - d) === "1"){
        n += Math.pow(2, d);
      }
    }
    return n;
  }

  function enc2bin(s){
    var n = CHAR_SEQ.indexOf(s);
    if(n >= 0){
      return n2b(n, 6);
    }else{
      return "";
    }
  }
    
  Base64.decode = function(str){
    var binstr = "";
    var i, len;
    for(i=0, len=str.length; i<len; i++){
      binstr += enc2bin(str[i]);
    }

    var ascii = "";
    var bin8;
    for(i=0, len=binstr.length; i<len; i += 8){
      if(i + 8 > len) break;
      bin8 = binstr.substring(i, i + 8);
      ascii += String.fromCharCode(b2n(bin8));
    }

    return ascii;
  };

  return Base64;
})();

exports.Base64 = Base64;
