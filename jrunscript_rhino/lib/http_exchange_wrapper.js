var HttpExchangeWrapper = (function(){
  
  "use strict";

  function HttpExchangeWrapper(he){
    this._he = he;
  }
  var __ = HttpExchangeWrapper.prototype;

  __.getPath = function(){
    return "" + this._he.getRequestURI().getPath();
  };

  __.addResponseHeaders = function(obj){
    var headers = this._he.getResponseHeaders();
    for(var key in obj){
      headers.add(key, obj[key]);
    }
  };

  return HttpExchangeWrapper;
})();

////////////////////////////////

exports.HttpExchangeWrapper = HttpExchangeWrapper;
