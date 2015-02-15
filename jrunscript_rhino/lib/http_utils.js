var HttpUtils = (function(){

  "use strict";

  function escapeHTML(src){
    return src
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#39;')
    ;
  }

  ////////////////////////////////

  function HttpUtils(){}

  HttpUtils.makeErrorPageHtml = function(ex, isDebug){
    if(isDebug){
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
    }else{
      var html = '<html><body>';
      html += '<p>Error</p>';
      html += '</body></html>';
      return html;
    }
  };

  return HttpUtils;
})();


HttpUtils.Request = (function(){

  function Request(){
    this.params = {};
  }

  return Request;
})();


HttpUtils.Response = (function(){

  function Response(){
    this.body = "{body}";
    this.status = 500;
    this.contentType = null;
  }
  var __ = Response.prototype;

  __.send = function(body){
    this.status = 200;
    this.body = body;
  };

  __.setContentType = function(contentType){
    this.contentType = contentType;
  };

  return Response;
})();


exports.HttpUtils = HttpUtils;
