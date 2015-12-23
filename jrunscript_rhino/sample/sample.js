(function(global){
  function basename(path){
    var parts = ("" + path).replace(/\\/g, "/").split("/");
    return parts[parts.length-1];
  }

  var stdLibDir = "" + java.lang.System.getenv("JJS_STDLIB_DIR");
  global.__FILE__ = (new File("./").getCanonicalPath()
      + "/" + basename(engine.get(engine.FILENAME)))
      .replace( /\\/g, "/" );
  global.LOAD_PATH = [
    __FILE__.replace( /^(.*)\/.+?$/, '$1' )
    , stdLibDir + "/lib"
  ];
  load(stdLibDir + "/init_jrunscript.js");
  load(stdLibDir + "/my_init.js");
})(this);

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
