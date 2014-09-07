(function(){
  var global = this;
  var stdLibDir = "/foo/bar/stdlib";
  global.__FILE__ = new File("./").getCanonicalPath()
      + "/" + engine.get(engine.FILENAME);
  global.LOAD_PATH = [
    __FILE__.replace( /^(.*)\/.+?$/, '$1' )
    , stdLibDir
  ];
  load(stdLibDir + "/init_jrunscript.js");
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
