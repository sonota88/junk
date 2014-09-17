(function(){
  var global = this;

  global.puts = function(){
    for(var i=0,len=arguments.length; i<len; i++){
      println(arguments[i]);
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


  ////////////////////////////////


  (function(){

    function MyArray(arg){
      if(typeof arg === "undefined"){
        throw new Error("illegal argument");
      }
      this.list = arg;
    }

    function _ma(list){
      return new MyArray(list);
    }

    MyArray.prototype.each = function(fn){
      var ret;
      for(var i=0,len=this.list.length; i<len; i++){
        ret = fn(this.list[i], i);
        if(ret === false){ break; }
      }
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

    return _File;
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
