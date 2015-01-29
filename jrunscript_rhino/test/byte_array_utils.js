(function(){
  var global = this;
  var stdLibDir = "/foo/bar/stdlib";
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

////////////////////////////////

var ByteArrayUtils = require("lib/byte_array_utils");

function assert(act, exp){
  if(act !== exp){
    puts("expected (" + exp + ")");
    puts("but      (" + act + ")");
  }
}

// byte array to JavaScript string
function b2s(bytes){
  return "" + new java.lang.String(bytes);
}

(function(){
  
  var orig = new java.lang.String("abcde").getBytes();
  assert(b2s(orig), "abcde");

  (function(){
    var act = ByteArrayUtils.substr(orig, 0, 2);
    assert(b2s(act), "ab");
  })();

  (function(){
    var act = ByteArrayUtils.substr(orig, 3, 5);
    assert(b2s(act), "de");
  })();

  (function(){
    var act = ByteArrayUtils.substr(orig, 2);
    assert(b2s(act), "cde");
  })();

})();
