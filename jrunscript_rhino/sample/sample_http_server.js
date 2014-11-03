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

importClass(com.sun.net.httpserver.HttpHandler);
importClass(com.sun.net.httpserver.HttpServer);

var HttpUtils = require("lib/http_utils");

function handleFunc(he){
  try {
    HttpUtils.writeStringResponse(he, 200, "Hello!");
  } catch (ex) {
    println(ex);
    println(ex.stack);
    
    HttpUtils.writeStringResponse(
      he, 500, HttpUtils.makeErrorPageHtmlForDebug(ex));
  }finally{
    he.close();
  }
}

function main(){
  var PORT = 8100;

  // 自ホストからのみアクセス可
  var server = HttpServer.create(new InetSocketAddress("localhost", PORT), 0);
  // 他ホストからもアクセス可
  // var server = HttpServer.create(new InetSocketAddress(PORT), 0);

  server.createContext("/", new HttpHandler({handle: handleFunc}));

  server.start();
  println("server start at port " + PORT);

  java.lang.Thread.currentThread().suspend();
}

try{
  main();
} catch (ex) {
  println(ex);
  println(ex.stack);
}
