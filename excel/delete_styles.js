// WSH用
// Shift JIS + CRLF

////////////////////////////////

function print(arg){
  WScript.StdOut.write("" + arg);
}

function puts(arg){
  print("" + arg + "\n");
}

function putsp(k, v){
  puts("" + k + " ("+ v +")");
}

function dump(obj){
  for(var k in obj){
    putsp(k, obj[k]);
  }
}

////////////////////////////////

function withBook(app, path, fn){
  var book;
  try{
    // 既存のブックを開く
    putsp("path", path);
    book = app.Workbooks.Open(path);
    fn(book);
  } catch (x) {
    book.close();
  }
}

var defaultStyles = [
  "良い"
  // ...
];

var excludeList = defaultStyles.concat([
  // ...
]);

var _excludeList = _ary(excludeList);
function exclude_p(name){
  return _excludeList.contains(name);
}

function deleteStyle(book, i, name){
  try{
    book.Styles(i).Delete();
  } catch (x) {
    puts("");
    puts("削除失敗 (" + e.name + ") i(" + i + ") name(" + name + ")");
  }
}

function main(file){
  // Excelオブジェクトを取得（Excelの起動）
  var excelApp = new ActiveXObject("Excel.Application");
  
  // Excelアプリケーションの表示
  excelApp.Visible = true;

  withBook(excelApp, file, function(book){
    
    // スタイル数
    var numStyles = book.Styles.count;
    putsp("numStyles", numStyles);
    puts("----");
    
    var i, name, done = 0;

    // 破壊的操作であるため逆順に処理
    // n個の場合 1 <= x <= n
    for(i=numStyles; i>=1; i--){
      name = book.Styles(i).Name;
      if(exclude_p(name)){
        puts("\n" + "skip: (" + name + ")");
      }else{
        deleteStyle(book, i, name);
      }
      done += 1;
      if(done % 100 === 0){ print("."); }
      if(done % 1000 === 0){ print(" " + done + " "); }
    }

    putsp("numStyles", book.Styles.count);
    
    excelApp.DisplayAlerts = false; // 既存ファイルがあっても上書きする
    book.Save();
  });

  // Excelを終了
  excelApp.Quit();

  // オブジェクトを開放
  excelApp = null;
}

try{
  var xlsPath = WScript.Arguments(0);
  putsp("xlsPath", xlsPath);
  main(xlsPath);
} catch (e) {
  puts(e);
  dump(e);
  throw e;
}
