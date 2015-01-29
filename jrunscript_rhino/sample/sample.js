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

////////////////////////////////

// var lib1 = require("baz/lib1");

// var lib2 = require(
//   __init_jjs__.File.join(
//     __init_jjs__.File.dirname(__FILE__)
//     ,"baz/lib2"
//   )
// );

// LOAD_PATH.push("/path/to/other/lib/dir");
// var lib3 = require("subdir/lib3");
