(function(){

   function _readFile(path, opts){
     var fis = new FileInputStream(new File(path));
     var reader = new java.io.InputStreamReader(fis, "UTF-8");
     var br = new BufferedReader(reader);
     var sb = new java.lang.StringBuilder();

     var line;
     while(true){
       line = br.readLine();
       if(line === null){
         break;
       }
       sb.append(line + "\n");
     }

     br.close();
     reader.close();
     fis.close();

     return sb.toString();
   }

   function _load(str) {
     var src = _readFile(str);

     var oldFilename = engine.get(engine.FILENAME);
     engine.put(engine.FILENAME, str);
     try {
       engine.eval(
         "(function(){ "
         + src
         + " })();"
       );
     } catch(e) {
       println("----");
       for(var k in e){
         println("" + k + "(" + e[k] + ")");
       }
       println("----");
       throw e;
     } finally {
       engine.put(engine.FILENAME, oldFilename);
     }
   }

  function fileExists(path){
    var file = new File(path);
    return file.exists();
  }

  var requirePaths = ["."];

   function require(path){
     exports = {};
     var _path;
     if(path.match(/\.js$/)){
       _path = path;
     }else{
       _path = path + ".js";
     }

     var foundPath;
     for(var a=0,len=requirePaths.length; a<len; a++){
       var modulePath = requirePaths[a] + "/" + _path;
       if(fileExists(modulePath)){
         foundPath = modulePath;
         break;
       }
     }
     
     if(foundPath){
       _load(foundPath);
     }else{
       throw "module not found: " + path;
     }

     var obj;
     for(var k in exports){
       obj = exports[k];
     }
     exports = {};
     return obj;
   }
  require.paths = requirePaths;

   // ----------------

   var toplevel = this;
   toplevel.exports = {};
   toplevel.require = require;
 })();
