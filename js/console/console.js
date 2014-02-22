var _console = (function(){
  
  var $log, $input;
  var maxLines = 100;
  var keyCodes = {
    ENTER: 13
  };
  var maxDepath = 4;

  function each(array, proc){
    for(var a=0,len=array.length; a<len; a++){
      proc(array[a]);
    }
  }

  function join(array, sep){
    var s = "";
    if(array.length === 0){
      return s;
    }

    s = array[0];
    for(var a=1,len=array.length; a<len; a++){
      s += sep + array[a];
    }
    return s;
  }

  function _log(msg){
    var lines = $log.val().split("\n");
    lines.push(msg);

    if(lines[0].length === 0){ lines.shift(); }

    var numOverFlowed = lines.length - maxLines;
    for(var a=0; a<numOverFlowed; a++){
      lines.shift();
    }

    var text = join(lines, "\n");
    $log.val(text);

    // scroll to bottom
    var pos = text.length;
    if($log.get(0).setSelectionRange){
      $log.get(0).setSelectionRange(pos, pos);
    }

    $input.focus();
  }

    function _inspect(arg, depth){
      var sep = ", ";
      depth = depth || 0;
      
      if(depth >= maxDepath){
        return "(too deep)";
      }

      if(arg === null){
        return "null";
      }

      var type = typeof arg;
      
      switch(type){
      case "undefined":
        return "undefined";
      case "boolean":
        return "" + arg;
      case "number":
        return "" + arg;
      case "string":
        return '"' + arg + '"';
      case "function":
        return "" + arg;
      default:
        ;
      }

      if(arg instanceof Array){
        var list = [];
        for(var a=0,len=arg.length; a<len; a++){
          list.push(_inspect(arg[a], depth + 1));
        }

        var result = "[";
        result += join(list, sep);
        result += "]";
        return result;
      }

      if(arg instanceof Object){
        var list = [];
        for(var k in arg){
          var v = arg[k];
          list.push("" + k + ": " + _inspect(v, depth + 1));
        }

        var result = "{";
        result += join(list, sep);
        result += "}";
        return result;
      }

      if(type === "object"){
        return "" + arg;
      }
     
      throw new Error("unknown type (" + arg + ")");
    }

  ////////////////////////////////

  function init(){

    var css = {
      log: {
        "margin": "0"
        ,"padding": "0"
        ,"border": "solid 1px #444"
        ,"font-size": "11px"
        ,"font-family": "monospace"
      }
      ,input: {
        "margin": "0"
        ,"padding": "0"
        ,"border": "solid 1px #444"
        ,"font-size": "11px"
        ,"font-family": "monospace"
      }
      ,outer: {
        "position": "fixed"
        ,"top": "10px"
        ,"right": "10px"
        ,"padding": "2px"
        ,"border": "solid 1px #444"
        ,"box-shadow": "0 0 4px rgba(0,0,0, 0.3)"
      }
    };

    ////////////////////////////////

    $log = $("<textarea id='_log'></textarea>")
        .css(css.log)
    ;
    $input = $("<textarea></textarea>")
        .css(css.input)
        .on("keydown", function(ev){
          if(ev.keyCode === keyCodes.ENTER){
            var code = $input.val();
            try{
              var result = eval(code);
            }catch(e){
              _log(e.message);
            }
            _log(_inspect(result));
            return false;
          }
          return null;
        })
    ;
    var $br = $("<br />");

    var $outer = $("<div></div>")
        .css(css.outer)
        .append($log)
        .append($br)
        .append($input)
    ;

    $("body").append($outer);
  }

  ////////////////////////////////

  function log(){
    each(arguments, function(it){
      _log("" + it);
    });
  }
  
  function inspect(){
    each(arguments, function(it){
      _log(_inspect(it));
    });
  }
  
  return {
    init: init
    , log: log
    , inspect: inspect
    , p: inspect
  };
})();
