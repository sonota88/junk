// sjis/crlf

(function(){
  var global = this;
  var stdLibDir = "C:/path/to/wsh_jscript";
  global.__FILE__ = WScript.ScriptFullName.replace( /\\/g, "/");
  global.LOAD_PATH = [
    global.__FILE__.replace( /^(.*)\/.+?$/, '$1')
    , stdLibDir
  ];

  function _fileExists(path){
    var fso = new ActiveXObject("Scripting.FileSystemObject");
    return fso.FileExists(path);
  }

  function _readFileSjis(path){
    if( ! _fileExists(path)){
      throw new Error("file not found: " + path);
    }

    var sin = new ActiveXObject("ADODB.Stream");
    sin.type = 2; // text
    sin.charset = "Shift_JIS";
    sin.open();
    sin.loadFromFile(path);
    var text = sin.readText(-1); // read all
    sin.close();
    return text;
  }

  function load(path){
    var src = _readFileSjis(path);
    eval("(function(){ " + src + " })();");
  }

  load(stdLibDir + "/init_wsh.js");
  load(stdLibDir + "/my_init.js");
})();

// --------------------------------

print(1);
puts(2);

each([11, 12, 13], function(it){
  puts(it);
});

_File.write("temp.txt", "‚  FDSA " + new Date());
puts(_File.read("temp.txt", "Shift_JIS"));

// var calc = require("calc");
// puts(calc.plus(1, 2));
