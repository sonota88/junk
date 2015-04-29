var Base64 = require("base64");

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
