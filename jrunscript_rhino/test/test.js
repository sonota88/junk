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

(function(args){

  var testFile = "" + args[0];
  if( ! testFile.match(/\.js$/) ){
    testFile += ".js";
  }
  
  eval( _File.read(testFile) );

})(arguments);

if(exs.length > 0){
  _.each(exs, function(ex){
    puts(ex);
  });
  throw "fail";
}
