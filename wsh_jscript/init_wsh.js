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

global.each = each;
global.print = print;
global.puts = puts;
