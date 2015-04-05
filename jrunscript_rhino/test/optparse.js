var Optparse = require("lib/optparse");

(function(){
  puts("no options");
  var args = ["a", "b"];
  var opts = Optparse.parse(args);
  assert(opts.shift(), "a");
  assert(opts.shift(), "b");
})();

(function(){
  puts("not required");
  var args = ["a", "-b"];
  var opts = Optparse.parse(args, [
    ["-b"]
  ]);
  assert(opts.shift(), "a");
  assert(opts.has("-b"), true);
  assert(opts.has("-a"), false);
})();

(function(){
  puts("required");
  var args = ["-a", "123"];
  var opts = Optparse.parse(args, [
    ["-a", true]
  ]);
  assert(opts.rest.length, 0);
  assert(opts.has("-a"), true);
  assert(opts.valueOf("-a"), "123");
})();

(function(){
  puts("required but no value");
  var args = ["-a"];
  try{
    var opts = Optparse.parse(args, [
      ["-a", true]
    ]);
    fail("must throw 1");
  } catch (x) {
    // OK
  }
})();
