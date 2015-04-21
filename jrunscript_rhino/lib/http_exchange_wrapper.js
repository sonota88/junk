var JavaUtils = require("java_utils");
var ByteArrayUtils = require("byte_array_utils");

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

  function splitBytes(reqBody, boundary){
    var tail = reqBody;
    var result = [];
    while(true){
      var pos = ByteArrayUtils.indexOf(tail, boundary);
      if(pos < 0) break;
      var pre = ByteArrayUtils.substr(tail, 0, pos);
      result.push(pre);
      tail = ByteArrayUtils.substr(tail, pos + boundary.length);
    }

    return result;
  }

  function javaString(str){
    return new java.lang.String(str);
  }

  function parsePart(part){
    var str = "" + ByteArrayUtils.toStr(part, "UTF-8");

    if( ! str.match(/Content-Disposition:/) ){
      return null;
    }

    var sep = javaString("\r\n\r\n").getBytes();
    var pos = ByteArrayUtils.indexOf(part, sep);
    var header = ByteArrayUtils.toStr(ByteArrayUtils.substr(part, 0, pos));
    var terminater = javaString("\r\n--");
    var body = ByteArrayUtils.substr(
      part
      , pos + sep.length
      , part.length - terminater.length()
    );
    
    var jsHeader = "" + header;
    var name = null;
    if(jsHeader.match(/; name="(.+?)"/)){
      name = RegExp.$1;
    }
    var filename = null;
    if(jsHeader.match(/; filename="(.+?)"/)){
      filename = RegExp.$1;
    }
    var _body;
    if(jsHeader.match(/Content-Type: (.+)/)){
      var contentType = RegExp.$1;
      _body = {
        contentType: contentType
        , type: "binary"
        , value: body
        , filename: filename
      };
    }else{
      _body = {
        type: "string"
        , value: "" + ByteArrayUtils.toStr(body)
      };
    }

    return {
      header: "" + header
      , name: name
      , body: _body
    };
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

  __.writeString = function(status, body){
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

  __.writeFile = function(path){
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


  /**
   * @return Java byte[]
   */
  __.getRequestBody = function(){

    // InputStream
    var is = this._he.getRequestBody();
    try{
      var buf = createByteArray(1024);
      var n;
      var baos = new ByteArrayOutputStream();

      while(true){
        n = is.read(buf);
        if(n === -1){
          break;
        }
        baos.write(buf, 0, n);
      }
    } finally {
      if(is){ is.close(); }
    }

    return baos.toByteArray();
  };

  __._isMultipartFormData = function(){
    // TODO refactor
    var headers = JavaUtils.map2obj(this._he.getRequestHeaders());

    if( ! headers["Content-type"]){
      return false;
    }

    var contentTypes = JavaUtils.list2ary(headers["Content-type"]);
    var contentType = contentTypes[0];

    return contentType.match(/^multipart\/form-data/) != null;
  };


  /**
   * @param queryStr java.lang.String
   */
  __._parseQueryString = function(str){
    var params = {};

    if( str == null ){
      return params;
    }

    var pairs = str.split("&");
    _.each(pairs, function(pair){
      pair.match(/^(.+?)=(.*)/);
      var k = RegExp.$1;
      var v = decodeURIComponent(("" + RegExp.$2).replace(/\+/g, " "));
      params[k] = v;
    });

    return params;
  };

  __._getBoundary = function(){
    // TODO refactor
    var headers = JavaUtils.map2obj(this._he.getRequestHeaders());
    if( ! headers["Content-type"] ){
      return false;
    }

    var contentTypes = JavaUtils.list2ary(headers["Content-type"]);
    var contentType = contentTypes[0];

    contentType.match(/boundary=(\-+\d+)$/);
    return RegExp.$1;
  };

  __._getMultipartParams = function(reqBody){
    var boundary = this._getBoundary();
    var parts = splitBytes(reqBody, javaString(boundary).getBytes());
    var partDataList = [];
    _(parts).each(function(part, i){
      var partData = parsePart(part);
      if(partData){ partDataList.push(partData); }
    });

    var params = {};
    _(partDataList).each(function(partData, i){
      if(partData.body.type === "string"){
        params[partData.name] = partData.body.value;
      }else if(partData.body.type === "binary"){
        params[partData.name] = partData;
      }else{
        _dump(partData);
        throw "must not happen";
      }
    });
    return params;
  };


  /**
   * @param reqBody Java byte[]
   */
  __.getParams = function(){
    var reqBody = this.getRequestBody();

    // java.lang.String
    var queryStr;

    if( this._isMultipartFormData() ){
      return this._getMultipartParams(reqBody);

    }else{
      // puts("is not multipart");
      var method = this.getMethod().toUpperCase();

      if( method === "GET" ){
        queryStr = this._he.getRequestURI().getQuery();
      }else if( method === "POST" ){
        queryStr = ByteArrayUtils.toStr(reqBody);
      }else{
        throw "method not supported: " + method;
      }

      return this._parseQueryString(queryStr);
    }
  };

  __.close = function(){
    this._he.close();
  };

  return HttpExchangeWrapper;
})();

////////////////////////////////

exports.HttpExchangeWrapper = HttpExchangeWrapper;
