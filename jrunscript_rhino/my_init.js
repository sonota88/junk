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
      return "" + (new java.lang.String(byteArray, "UTF-8"));
    }
  };
  
  return _File;
})();
