"use strict";

require("underscore/underscore");

var Db2Utils = (function(){

  function Db2Utils(){}

  Db2Utils.exec = function(cmds){
    var src = "";
    var tempFile = "__temp.txt";
    src += "connect to MYDB user USER using PASS" + ";\n";
    src += _(cmds).each(function(cmd){
      src += cmd + ";\n";
    });
    src += "connect reset" + ";\n";

    _File.write(tempFile, src);

    // Windows
    system("db2cmd /C /W /I db2 -tvf " + tempFile
           , null
           , { encoding: "Shift_JIS" }
          );
  };

  Db2Utils.importCsv_replace = function(path, tableName){
    Db2Utils.exec([
      "import from " + path + " of DEL replace into " + tableName
    ]);
  };

  Db2Utils.exportCsv = function(path, tableName){
    Db2Utils.exec([
      "export to " + path + " of DEL select * " + tableName
    ]);
  };

  return Db2Utils;
})();
