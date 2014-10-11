(function(){
  var global = this;

  global.JSON = JSON || require("my_json");

  function _puts(val){
    if(
      val === null
      || typeof val === "undefined"
      || typeof val === "string"
      || typeof val === "number"
      || typeof val === "boolean"
      || val instanceof RegExp
    ){
      println(val);
    }else{
      println(JSON.stringify(val));
    }
  }

  global.puts = function(){
    for(var i=0,len=arguments.length; i<len; i++){
      _puts(arguments[i]);
    }
  };

  global.putskv = function(k, v){
    puts("" + k + " (" + v + ")");
  };

  global.dump = function(obj){
    for(var k in obj){
      putskv(k, obj[k]);
    }
  };

  global.range = function(from, to){
    var xs = [];
    for(var x=from; x<=to; x++){
      xs.push(x);
    }
    return xs;
  };

  global.poorfmt = function(){
    if(arguments.length === 0){
      throw new Error("arguments must not be empty");
    }

    var template = arguments[0];
    var vals = Array.prototype.slice.call(arguments, 1);
    
    var ret = "";
    var i = 0;
    while(template.length > 0){
      var c2 = template.substring(0, 2);
      if(c2 === "%%"){
        ret += "%";
        template = template.substring(2);
      }else if(c2 === "%s"){
        ret += vals[i]; i++;
        template = template.substring(2);
      }else{
        ret += template.substring(0, 1);
        template = template.substring(1);
      }
    }

    var numPlaceholders = i;
    var numVariables = arguments.length-1;
    if(numPlaceholders !== numVariables){
      throw new Error("invalid status");
    }

    return ret;
  };


  ////////////////////////////////


  (function(){

    function MyArray(arg){
      if(typeof arg === "undefined"){
        throw new Error("illegal argument");
      }
      this._list = arg;
    }

    function _ma(list){
      return new MyArray(list);
    }

    MyArray.prototype.size = function(){
      return this._list.length;
    };

    MyArray.prototype.get = function(i){
      if(arguments.length === 0){
        return this._list;
      }
      return this._list[i];
    };

    MyArray.prototype.push = function(el){
      return this._list.push(el);
    };

    MyArray.prototype.each = function(fn){
      var ret;
      for(var i=0,len=this._list.length; i<len; i++){
        ret = fn(this._list[i], i);
        if(ret === false){ break; }
      }
    };

    MyArray.prototype.map = function(fn){
      var list = _ma([]);
      for(var i=0,len=this._list.length; i<len; i++){
        list.push( fn(this._list[i], i) );
      }
      return list;
    };

    MyArray.prototype.filter = function(fn){
      var list = _ma([]);
      for(var i=0,len=this._list.length; i<len; i++){
        if( fn(this._list[i], i) ){
          list.push( this._list[i] );
        }
      }
      return list;
    };

    global._ma = _ma;
  })();


  ////////////////////////////////


  global._File = (function(){

    function _File(){}

    function createArray(type, size){
      var javaType = type;
      if(type === "Byte"){
        javaType = java.lang.Byte.TYPE;
      }else{
        throw new Error("not yet implemented");
      }
      return java.lang.reflect.Array.newInstance(
        javaType, size);
    };

    function withFileInputStream(file, fn){
      var fis;
      try{
        fis = new FileInputStream(file);
        fn(fis);
      }finally{
        if(fis){
          fis.close();
        }
      }
    }

    function withBufferedInputStream(is, fn){
      var bis;
      try{
        bis = new BufferedInputStream(is);
        fn(bis);
      } finally {
        if(bis){
          bis.close();
        }
      }
    }

    function withFileWriter(file, fn){
      var fw = new FileWriter(file);
      try{
        fn(fw);
      } finally {
        if(fw){ fw.close(); }
      }
    }

    function withBufferedWriter(writer, fn){
      var bw = new BufferedWriter(writer);
      try {
        fn(writer);
      } finally {
        if(bw){ bw.close(); }
      }
    }

    _File.read = function(path, opts){
      if(opts && opts.binary){

        var file = new File(path);
        var result = createArray("Byte", file.length());

        withFileInputStream(file, function(is){
          withBufferedInputStream(is, function(bis){
            bis.read(result);
          });
        });

        return result;
      }else{
        var byteArray = _File.read(path, { binary: true });
        var enc = (opts && opts.encoding) || "UTF-8";
        return "" + (new java.lang.String(byteArray, enc));
      }
    };

    _File.write = function(path, text){
      withFileWriter(new File(path), function(fw){
        withBufferedWriter(fw, function(bw){
          bw.write(text);
        });
      });
    };

    _File.exists = function(path){
      return new File(path).exists();
    };

    _File.dirname = function(path){
      return path.replace( /^(.+)\/.+?$/, '$1');
    };

    return _File;
  })();


  ////////////////////////////////


  global.Dir = (function(){
    
    var Dir = {};

    function _traverse(rootDir, fn){
      var _rootDir = new File(rootDir);
      var paths = _rootDir.listFiles();
      var path;
      for(var i=0,len=paths.length; i<len; i++){
        path = paths[i];
        if(path.isDirectory()){
          fn("" + path.getPath() + "/");
          _traverse(""+path, fn);
        }else{
          fn("" + path.getPath());
        }
      }
    }

    Dir.allPaths = function(rootDir){
      var paths = [];
      _traverse(rootDir, function(path){
        paths.push(path);
      });
      return paths;
    };
    
    Dir.traverse = function(rootDir, fn){
      _traverse(rootDir, fn);
    };
    
    return Dir;
  })();


  ////////////////////////////////


  (function(){
    function createArray(type, size){
      var _type = type;
      if(type === "Byte"){
        _type = java.lang.Byte.TYPE;
      }
      return java.lang.reflect.Array.newInstance(
        _type, size);
    };

    // ----------------

    var ByteArrayUtil = (function(){

      var ByteArrayUtil = {};

      ByteArrayUtil.substr = function(array, from, to){
        var size;
        if(to){
          size = to - from;
        }else{
          size = array.length - from;
        }
        var result = createArray("Byte", size);
          for(var i=0; i<size; i++){
            result[i] = array[from + i];
          }
        return result;
      };

      ByteArrayUtil.toStr = function(bary, encoding){
        if(!encoding){
          encoding = "UTF-8";
        }
        return new java.lang.String(bary, encoding);
      };

      return ByteArrayUtil;
    })();

    // ----------------

    function makeReaderThread(is, out, enc){
      var _enc = enc || "UTF-8";
      var isr = new InputStreamReader(is, _enc);
      return new java.lang.Thread(function(){
        var n, sw;
        while(true){
          n = isr.read();
          if(n < 0){
            break;
          }
          sw = new StringWriter(1);
          sw.write(n);
          out.print(sw);
        }
      });
    }

    /**
     * @param opts
     *   outPrinter: default: System.out
     *   errPrinter: default: System.err
     */
    var _execSync = function(args, workDir, opts){
      if(typeof args === "string"){
        args = args.split( /\s+/ );
      }
      workDir = workDir || ".";
      opts = opts || {};

      // println("exec command: " + args.join(" "));
      var javaArgs = createArray(
        java.lang.String, args.length);
      args.forEach(function(e, i){
        javaArgs[i] = e;
      });

      var pb = new java.lang.ProcessBuilder(javaArgs);
      pb.directory(new File(workDir));

      var proc = pb.start();
      var is = proc.getInputStream();
      var es = proc.getErrorStream();

      var outPrinter = opts.outPrinter || java.lang.System.out;
      var errPrinter = opts.errPrinter || java.lang.System.err;

      var isTh = makeReaderThread(is, outPrinter, opts.encoding);
      var esTh = makeReaderThread(es, errPrinter, opts.encoding);

      isTh.start();
      esTh.start();

      while(isTh.isAlive() || isTh.isAlive()){
        java.lang.Thread.sleep(100);
      }

      var status = proc.waitFor();

      if(status !== 0){
        throw new Error("exec failed " + status);
      }

      return status;
    };

    /**
     * out, err はstdout/stderrに出すだけ。
     */
    global.execSync = function(args, workDir){
      var status = _execSync(args, workDir);

      return {
        status: status
      };
    };
    global.system = global.execSync;

    /**
     * out, err を文字列として取得する。
     */
    global.execSyncStr = function(args, workDir, enc){
      var swout = new StringWriter();
      var swerr = new StringWriter();
      var _enc = enc || "UTF-8";
      var status;

      try{
        status = _execSync(
          args, workDir
         ,{ outPrinter: new PrintWriter(swout)
           ,errPrinter: new PrintWriter(swerr)
           ,encoding: _enc
          }
        );
      }catch(e){
        println(e);
        println("[OUT]\n" + swout.toString());
        println("[ERR]\n" + swerr.toString());
      }

      return {
        status: status
       ,out: "" + swout.toString()
       ,err: "" + swerr.toString()
      };
    };

  })();
})();
