(function(){
  var global = this;
  var stdLibDir = "" + java.lang.System.getenv("JJS_STDLIB_DIR");
  global.__FILE__ = (new File("./").getCanonicalPath()
      + "/" + engine.get(engine.FILENAME))
      .replace( /\\/g, "/" );
  global.LOAD_PATH = [
    __FILE__.replace( /^(.*)\/.+?$/, '$1' )
    , stdLibDir
  ];
  load(stdLibDir + "/init_jrunscript.js");
  load(stdLibDir + "/my_init.js");
})();

var _ = require("lib/underscore/underscore");

////////////////////////////////

var Base64 = require("lib/base64");

function assert(act, exp){
  if(act !== exp){
    puts("expected (" + exp + ")");
    puts("but      (" + act + ")");
  }
}

var exs = [];
function fail(ex){
  exs.push(ex);
}

(function(){
  
  (function(){
    puts("basic case");
    var act = Base64.decode("YWJj");
    assert(act, "abc");
  })();

  (function(){
    puts("文字数が3の倍数でない場合");
    var act = Base64.decode("YWI=");
    assert(act, "ab");
  })();

})();

if(exs.length > 0){
  _.each(exs, function(ex){
    puts(ex);
  });
  throw "fail";
}
