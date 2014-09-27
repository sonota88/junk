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
      if( ! e.stack){
        return ["stack not available"];
      }

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

  var _File = {
    join: function(){
      var path = "";
      each(arguments, function(arg){
        if( path === "" || path.match( /\/$/ )){
          ;
        }else{
          path += "/";
        }
        path += arg;
      });
      return path;
    }
    ,dirname: function(path){
      return path.replace( /^(.+)\/.+?$/, '$1' );
    }
    ,exists: function(path){
      return new File(path).exists();
    }
    ,read: function(path){
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
  };

  // ----------------

  function _load(str) {
    var src = _File.read(str);
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

  function _require(path){
    exports = {};
    if( ! path.match(/\.js$/) ){
      path = path + ".js";
    }

    var foundPath;
    each(global.LOAD_PATH, function(loadPath){
      var fullPath;
      if(path.match( /^\// )
         || path.match( /^[A-Z]:\//i ) // Windows
        ){
        fullPath = path;
      }else{
        fullPath = _File.join(loadPath, path);
      }
      if(_File.exists(fullPath)){
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

  function require(path){
    var module;
    try{
      module = require(path);
    } catch (ex) {
      dumpError(ex);
      throw ex;
    }
    return module;
  }

  // ----------------

  global.exports = {};
  global.require = require;
  global.__init_jjs__ = {
    File: _File
  };
})();
