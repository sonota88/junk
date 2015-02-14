var HttpExchangeWrapper = (function(){
  
  "use strict";

  /**
   * @param {string} jsString
   * @return {java.lang.String}
   */
  function toJavaString(jsString){
    return new java.lang.String(jsString);
  };

  function withFileInputStream(file, proc){
    var fis = new FileInputStream(file);
    try{
      proc(fis);
    } finally {
      if(fis){ fis.close(); }
    }
  }

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

  var contentTypes = {
    "jpg": "image/jpeg"
    , "jpeg": "image/jpeg"
    , "png": "image/png"
    , "svg": "image/svg+xml"
    , "gif": "image/gif"
    , "css": "text/css"
    , "js": "text/javascript"
    , "json": "application/json"
  };

  function getContentType(path){
    if(path.match(/(.+)\.(.+)$/)){
      var ext = RegExp.$2.toLowerCase();
      return contentTypes[ext];
    }else{
      return null;
    }
  }

  function createByteArray(size){
    return java.lang.reflect.Array.newInstance(
      java.lang.Byte.TYPE, size);
  }


  function HttpExchangeWrapper(he){
    this._he = he;
  }
  var __ = HttpExchangeWrapper.prototype;

  __.getPath = function(){
    return "" + this._he.getRequestURI().getPath();
  };

  __.getMethod = function(){
    return "" + this._he.getRequestMethod();
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

  __.writeFileResponse = function(path){
    var resOS = this._he.getResponseBody();

    var file = new File(path);

    var contentType = getContentType(path);
    // putskv("contentType", contentType);

    if(contentType){
      this._he.getResponseHeaders().add(
        "Content-Type", contentType);
    }

    this._he.sendResponseHeaders(
      200, file.length());

    withFileInputStream(file, function(fis){
      var buf = createByteArray(1024);
      var len;
      while((len = fis.read(buf)) >= 0){
        resOS.write(buf, 0, len);
      }
    });

    resOS.close();
  };

  return HttpExchangeWrapper;
})();

////////////////////////////////

exports.HttpExchangeWrapper = HttpExchangeWrapper;
