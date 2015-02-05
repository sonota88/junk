var HttpUtils = (function(){

  function escapeHTML(src){
    return src
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    ;
  }

  ////////////////////////////////

  function HttpUtils(){}

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
