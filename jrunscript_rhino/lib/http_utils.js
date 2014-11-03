var HttpUtils = (function(){

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

  function escapeHTML(src){
    return src
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    ;
  }

  ////////////////////////////////

  function HttpUtils(){}

  HttpUtils.writeStringResponse = function(he, status, str){

    var javaStr = new toJavaString(str);

    he.sendResponseHeaders(
      status, javaStr.getBytes("UTF-8").length);

    var os = he.getResponseBody();

    withOutputStreamWriter(os, "UTF-8", function(w){
      withBufferedWriter(w, function(bw){
        bw.write(javaStr);
      });
    });

    os.close();
  };

  HttpUtils.makeErrorPageHtmlForDebug = function(ex){
    var html = '<html><body>';
    html += ex;
    html += '<hr />';
    html += '<pre style="font-family: monospace;">';
    for(var k in ex){
      html += "" + k + " (" + escapeHTML("" + ex[k]) + ")\n";
    }
    html += '</pre>';
    html += '</body></html>';
    return html;
  };

  return HttpUtils;
})();

exports.HttpUtils = HttpUtils;
