(function(){
  var global = this;

  global.LOAD_PATH = global.LOAD_PATH || [];

  function each(list, fn){
    var ret = null;
    for(var i=0,len=list.length; i<len; i++){
      ret = fn(list[i], i);
      if(ret === false){
        break;
      }
    }
  }

  function getCallStack(){
    var stack = [];
    try{
      eval(".");
    }catch(e){
      var lines = e.stack.split("\n");
      for(var i=lines.length; i>=1; i--){
        var line = lines[i];
        if(typeof line === "undefined"
           || line.match(/^\s*$/))
        {
          // skip
        }else{
          stack.push(line.replace(/^\s*/, ""));
        }
      }
    }
    return stack;
  }

  function dumpError(ex){
    var s = "";
    s += ex.name + ": ";
    s += ex.message + "\n";
    if(typeof ex.stack === "undefined"){
      s += "(no stack)";
    }else if(typeof ex.stack === "object" && ex.stack.length){
      for(var i=ex.stack.length-1; i>=0; i--){
        s += ex.stack[i] + "\n";
      }
    }else{
      s += ex.stack;
    }

    println(s);
  }

  function MyError(msg){
    var self = this;
    this.name = "MyError";
    this.message = msg;

    var stack = getCallStack();
    this.stack = [];
    each(stack, function(stackLine){
      self.stack.push(stackLine);
    });
  }

  // ----------------

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

     return "" + sb.toString();
   }

   function _load(str) {
     var src = _readFile(str);
     var scriptFullPath = "" + new File(str).getCanonicalPath();

     var oldFilename = engine.get(engine.FILENAME);
     engine.put(engine.FILENAME, str);
     try {
       engine.eval(
         '(function(){ var __FILE__ = "' + scriptFullPath + '"; '
         + src
         + " })();"
       );
     } catch(e) {
       dumpError(e);
       throw e;
     } finally {
       engine.put(engine.FILENAME, oldFilename);
     }
   }

  function fileExists(path){
    var file = new File(path);
    return file.exists();
  }

   function require(path){
     exports = {};
     var _path;
     if(path.match(/\.js$/)){
       _path = path;
     }else{
       _path = path + ".js";
     }

     var foundPath;
     each(global.LOAD_PATH, function(path2){
       var fullPath;
       if(_path.match( /^\// )){
         fullPath = _path;
       }else{
         fullPath = path2 + "/" + _path;
       }
       if(fileExists(fullPath)){
         foundPath = fullPath;
         return false; // break
       }
     });
     if(foundPath){
       _load(foundPath);
     }else{
       throw new MyError("module not found: " + path);
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

   global.exports = {};
   global.require = require;
 })();
