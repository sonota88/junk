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

function print(arg){
    WScript.StdOut.Write(arg);
}

function puts(){
    each(arguments, function(it){
        print(it + "\n"); 
    });
}

// --------------------------------

var _File = (
    function(){

        var _File = {};

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

// --------------------------------

global.each = each;
global.print = print;
global.puts = puts;
global._File = _File;
