// sjis/crlf

var global = this;

function each(list, fn){
    var ret = null;
    for(var i=0,len=list.length; i<len; i++){
        ret = fn(list[i], i);
        if( ret === false ){
            break;
        }
    }
}

function print(){
    for(var i=0,len=arguments.length; i<len; i++){
        WScript.StdOut.Write(arguments[i]); 
    }
}

function puts(){
    for(var i=0,len=arguments.length; i<len; i++){
        WScript.StdOut.Write(arguments[i] + "\n"); 
    }
}

// --------------------------------

var _File = (
    function(){

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

          ,exists: function(path){
            var fso = new ActiveXObject("Scripting.FileSystemObject");
            return fso.FileExists(path);
          }
        };

        _File.read = function(path, charset){
            var sin = new ActiveXObject("ADODB.Stream");
            sin.type = 2; // text
            sin.charset = charset || "Shift_JIS";
            sin.open();
            sin.loadFromFile(path);
            var text = sin.readText(-1); // read all
            sin.close();
            return text;
        };

        _File.write = function(path, text, charset){
            var sout = new ActiveXObject("ADODB.Stream");
            sout.mode = 3; // read/write
            sout.type = 2; // text
            sout.charset = charset || "Shift_JIS";
            sout.open();
            sout.writeText(text, 0);
            sout.saveToFile(path, 2);
            sout.close();
        };

        return _File;
    })();

function _load(str){
  var src = _File.read(str);
  var scriptFullPath = str;
  try {
    eval(
      '(function(){ var __FILE__ = "' + scriptFullPath + '"; '
        + src
        + " })();"
    );
  } catch(e) {
    // dumpError(e);
    throw e;
  } finally {
    ;
  }
}

function require(path){
  global.exports = global.exports || {};
  var exports = global.exports;

  if( ! path.match( /\.js$/) ){
    path = path + ".js";
  }

  var foundPath;
  each(global.LOAD_PATH, function(loadPath){
    var fullPath;
    if(path.match( /^\// )){
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
    throw new Error("module not found: " + path);
  }
  var obj;
  for(var k in exports){
    obj = exports[k];
  }
  exports = {};
  return obj;
}

// --------------------------------

global.each = each;
global.print = print;
global.puts = puts;
global._File = _File;
global.require = require;
