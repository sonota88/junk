"use strict";

load("my_init.js");
loadUtf8("libo_calc.js");

////////////////////////////////

function isBlank(val){
  return ("" + val).length === 0;
}

////////////////////////////////

var TableSheet = (function(){

  function TableSheet(sheet){
    this.sh = sheet;
    this.offset = this._getOffset(sheet);
  }

  TableSheet.prototype.get = function(ci, ri){
    return this.sh.get(ci, ri);
  };

  TableSheet.prototype.getTablePName = function(){
    return this.sh.get(0, 0);
  };
  TableSheet.prototype.getTableLName = function(){
    return this.sh.get(0, 1);
  };

  TableSheet.prototype.getOffset = function(){
    return this.offset;
  };

  TableSheet.prototype._getOffset = function(){
    var offset = {
      row: null
      ,col: null
    };
    var val;

    var limit = 100;

    // row
    for(var ri=0; ri<limit; ri++){
      val = this.get(0, ri);
      if(val === '>>begin'){
        offset.row = ri;
      }
    }

    // column
    for(var ci=0; ci<limit; ci++){
      val = this.get(ci, 0);
      if(val === '>>begin'){
        offset.col = ci;
      }
    }

    if(offset.row === null){
      throw new Error("row offset not found");
    }
    if(offset.col === null){
      throw new Error("col offset not found");
    }

    return offset;
  };

  TableSheet.prototype.getDataRowRange = function(){
    var ri = this.offset.row + 2;
    var ris = [];
    while(true){
      // 4行空白が続いたらデータ部終了
      var cell1 = this.get(1, ri    );
      var cell2 = this.get(1, ri + 1);
      var cell3 = this.get(1, ri + 2);
      var cell4 = this.get(1, ri + 3);

      if(
        isBlank(cell1)
          && isBlank(cell2)
          && isBlank(cell3)
          && isBlank(cell4)
      ){
        break;
      }
      ris.push(ri);
      ri++;
    }
    return ris;
  };

  /**
   * @return 対象テーブルのカラム数
   */
  TableSheet.prototype.getNumCols = function(){
    var riFrom = this.offset.row;

    var ci = this.offset.col;
    while(true){
      var pname = this.get(ci, riFrom + 1);

      if(isBlank(pname)){
        break;
      }
      if(ci >= 100){ // for safety
        throw new Error("number of columns is too many");
      }
      ci++;
    }
    return ci - this.offset.col;
  };

  TableSheet.prototype.getMetaData = function(){
    var me = this;
    var numCols = this.getNumCols();

    var md = [];

    range(this.offset.col, this.offset.col + numCols - 1).each(function(ci){
      var lname = me.get(ci, me.offset.row);
      var pname = me.get(ci, me.offset.row + 1);
      md.push({
        lname: lname, pname: pname
      });
    });

    return md;
  };

  TableSheet.prototype.toTsv = function(){
    var me = this;

    var md = this.getMetaData();

    var numCols = md.length;
    print(" / numCols=" + numCols);
    var rows = [];

    var dataRowRange = this.getDataRowRange();
    print(" / dataRowRange=" + dataRowRange[0]
         + "-" + dataRowRange[dataRowRange.length - 1]);
    _ma(dataRowRange).each(function(ri){
      // 1列目 ＝ 制御列
      var ctrlStr = "" + me.get(0, ri);
      var cols = [];
      if(ctrlStr.match(/^#/)){
        // "#" で始まっている行は無視
        return; // continue
      }
      for(var ci=me.offset.col; ci <= me.offset.col + numCols - 1; ci++){
        var val = "" + me.get(ci, ri);
        val = val.replace(/^'/, "");
        cols.push(val);
      }
      rows.push(cols);
    });

    var tsvs = "";

    // header
    tsvs += "\n" + "<__TABLE__>"
      + "pname:" + this.getTablePName()
      + "\t" + "lname:" + this.getTableLName() + "\n";

    // logical name
    tsvs += Tsv.toTsv(_ma(md).map(function(it){
      return it.lname;
    }));
    tsvs += "\n";

    // physical name
    tsvs += Tsv.toTsv(_ma(md).map(function(it){
      return it.pname;
    }));
    tsvs += "\n";

    // data
    _ma(rows).each(function(row){
      tsvs += Tsv.toTsv(row);
      tsvs += "\n";
    });

    return tsvs;
  };

  return TableSheet;
})();

function withTempFile(originalFilePath, tempFilePath, fn){
  try{
    if(_File.exists(tempFilePath)){
      FileUtils.rm(tempFilePath);
    }
    FileUtils.cp(originalFilePath, tempFilePath);

    fn(tempFilePath);

  }finally{
    if(_File.exists(tempFilePath)){
      FileUtils.rm(tempFilePath);
    }
  }
}

/////////////////////////////

function main(path){
  var odsPath = _File.fixPath(path);
  if( ! _File.isAbsolutePath(odsPath) ){
    odsPath = Dir.pwd() + "/" + odsPath;
  }

  withTempFile(odsPath, odsPath + ".TEMP.ods", function(tempFilePath){
    var out = "";
    
    Calc.open(tempFilePath, function(doc){

      var sheets = doc.getSheets();

      sheets.forEach(function(sh, i){
        var tableSheet = new TableSheet(sh);
        print("sheet=" + sh.name);
        print(" / table=" + tableSheet.getTablePName()
             + " (" + tableSheet.getTableLName() + ")");
        var offset = tableSheet.getOffset();
        print(" / offset=row:" + offset.row
             + ", col:" + offset.col);

        out += tableSheet.toTsv();
        print("\n");
      });
    });

    _File.write(odsPath + ".tsv", out);
  });
}

main("" + arguments[0]);