"use strict";

var FileUtils = (function(){

  var FileUtils = {};

  function createByteArray(size){
    return java.lang.reflect.Array.newInstance(
      java.lang.Byte.TYPE, size);
  }

  FileUtils.cp = function(src, dest){

    if(new File(dest).exists()){
      throw new Error("dest file exists");
    }

    var fis, fos;
    try{
      fis = new FileInputStream(src);
      fos = new FileOutputStream(dest);
      
      var buf = createByteArray(1024);
      var len;

      while(true){
        len = fis.read(buf);
        if(len === -1){
          break;
        }
        fos.write(buf, 0, len);
      }

    }finally{
      if(fis){ fis.close(); }
      if(fos){ fos.close(); }
    }
  };

  FileUtils.rm = function(path){
    new File(path)["delete"]();
  };

  FileUtils.mv = function(src, dest){
    new File(src).renameTo(new File(dest));
  };

  return FileUtils;
})();


exports.FileUtils = FileUtils;
