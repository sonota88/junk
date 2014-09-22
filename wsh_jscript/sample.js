// sjis/crlf

function _readFileSjis(path){
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

// --------------------------------

load("init_wsh.js");
print(1);
puts(2);

each([11, 12, 13], function(it){
    puts(it);
});
