"use strict";

var FileUtils = (function(){

  var _ = require("lib/underscore/underscore");

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
    var ret = new File(path)["delete"]();
    if(ret === false){
      throw new Error("failed to delete (" + path + ")");
    }
  };

  FileUtils.rmRecursive = function(dir){
    var paths;

    paths = Dir.allPaths(dir);
    _(paths).each(function(path){
      if( ! Dir.isDir(path) ){
        FileUtils.rm(path);
      }
    });

    paths = Dir.allPaths(dir).sort(function(a, b){
      // descending order
      return b.length - a.length;
    });
    _(paths).each(function(path){
      if( Dir.isDir(path) ){
        Dir.rmdir(path);
      }
    });

    Dir.rmdir(dir);
  };

  FileUtils.mv = function(src, dest){
    new File(src).renameTo(new File(dest));
  };

  return FileUtils;
})();


exports.FileUtils = FileUtils;
