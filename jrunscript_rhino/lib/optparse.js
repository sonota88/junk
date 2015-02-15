var Options = (function(){

  "use strict";

  function Options(){
    this.flags = [];
    this.rest = [];
  }
  var __ = Options.prototype;

  __.shift = function(){
    return this.rest.shift();
  };

  __.has = function(name){
    var flags = _.pluck(this.flags, "name");
    return _.contains(flags, name);
  };

  __.valueOf = function(name){
    var flag = _.find(this.flags, function(_flag){
      return _flag.name === name;
    });
    return flag.value;
  };

  return Options;
})();

var Optparse = (function(){

  "use strict";

  function valueRequired(requireValueList, name){
    return _.contains(requireValueList, name);
  }

  var Optparse = {};
  
  Optparse.parse = function(args, opts){

    opts = opts || {};
    opts.requireValue = opts.requireValue || [];

    var _args = _.map(args, function(arg){
      return "" + arg;
    });
    
    var ret = new Options();

    var i=0;
    while(i < _args.length){
      var arg = _args[i];
      if(arg.match(/^-/)){
        var name = arg;
        var value = null;
        if(valueRequired(opts.requireValue, name)){
          if(i + 1 >= args.length){
            throw "value does not exist";
          }
          value = _args[i + 1];
          i++;
        }
        ret.flags.push({
          name: name, value: value
        });
      }else{
        ret.rest.push(arg);
      }
      i++;
    }

    return ret;
  };

  return Optparse;
})();

exports.Optparse = Optparse;
