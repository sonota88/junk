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

var Optparse = require("lib/optparse");

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
    var args = ["a", "b"];
    var opts = Optparse.parse(args);
    assert(opts.shift(), "a");
    assert(opts.shift(), "b");
  })();

  (function(){
    var args = ["a", "-b"];
    var opts = Optparse.parse(args);
    assert(opts.shift(), "a");
    assert(opts.has("-b"), true);
    assert(opts.has("-a"), false);
  })();

  (function(){
    var args = ["-a", "123"];
    var opts = Optparse.parse(args, {
      requireValue: ["-a"]
    });
    assert(opts.rest.length, 0);
    assert(opts.has("-a"), true);
    assert(opts.valueOf("-a"), "123");
  })();

  (function(){
    var args = ["-a"];
    try{
      var opts = Optparse.parse(args, {
        requireValue: ["-a"]
      });
      fail("must throw 1");
    } catch (x) {
      // OK
    }
  })();

  (function(){
    var args = ["-a"];
    var opts = Optparse.parse(args);
    assert(opts.rest.length, 0);
    assert(opts.has("-a"), true);
  })();

  (function(){
    var args = ["-a", "123"];
    var opts = Optparse.parse(args);
    assert(opts.has("-a"), true);
    assert(opts.valueOf("-a"), null);
    assert(opts.rest.length, 1);
  })();

})();

if(exs.length > 0){
  _.each(exs, function(ex){
    puts(ex);
  });
  throw "fail";
}
