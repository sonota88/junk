var HttpExchangeWrapper = (function(){
  
  "use strict";

  /**
   * @param {string} jsString
   * @return {java.lang.String}
   */
  function toJavaString(jsString){
    return new java.lang.String(jsString);
  };

  function withOutputStreamWriter(fos, encoding, fn){
    var osw;
    try {
      osw = new OutputStreamWriter(fos, encoding);
      fn(osw);
    } finally {
      if(osw){ osw.close(); }
    }
  }

  function withBufferedWriter(writer, fn){
    var bw;
    try {
      bw = new BufferedWriter(writer);
      fn(writer);
    } finally {
      if(bw){ bw.close(); }
    }
  }


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

  __.writeStringResponse = function(status, body){
    var javaStr = new toJavaString(body);

    this._he.sendResponseHeaders(
      status, javaStr.getBytes("UTF-8").length);

    var os = this._he.getResponseBody();

    withOutputStreamWriter(os, "UTF-8", function(w){
      withBufferedWriter(w, function(bw){
        bw.write(javaStr);
      });
    });

    os.close();
  };

  return HttpExchangeWrapper;
})();

////////////////////////////////

exports.HttpExchangeWrapper = HttpExchangeWrapper;
