var ByteArrayUtils = require("byte_array_utils");

// byte array to JavaScript string
function b2s(bytes){
  return "" + new java.lang.String(bytes);
}

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
