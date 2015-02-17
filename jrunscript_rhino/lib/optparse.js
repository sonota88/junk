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
    return _.any(this.flags, function(flag){
      return flag.name === name;
    });
  };

  __.valueOf = function(name){
    var flag = _.find(this.flags, function(_flag){
      return _flag.name === name;
    });
    return flag.value;
  };

  return Options;
})();


var Cfg = (function(){

  "use strict";

  function Cfg(name, required){
    this.name = name;
    this.required = required;
  }
  var __ = Cfg.prototype;

  Cfg.fromArray = function(xs){
    return new Cfg(xs[0], !! xs[1]);
  };

  return Cfg;
})();


var Optparse = (function(){

  "use strict";

  function findCfg(cfgs, name){
    return _.find(cfgs, function(cfg){
      return cfg.name === name;
    });
  }

  var Optparse = {};
  
  Optparse.parse = function(args, argCfgs){

    var _args = _.map(args, function(arg){
      return "" + arg;
    });
    
    var cfgs = _.map(argCfgs || [], Cfg.fromArray);

    var names = _.map(cfgs, function(cfg){
      return cfg.name;
    });

    var opts = new Options();

    var i=0;
    while(i < _args.length){
      var arg = _args[i];

      if(_.contains(names, arg)){
        var name = arg;
        var cfg = findCfg(cfgs, name);

        var value = null;
        if(cfg.required){
          if(i + 1 >= _args.length){
            throw "value does not exist";
          }
          value = _args[i + 1];
          i++;
        }

        opts.flags.push({
          name: name, value: value
        });
      }else{
        opts.rest.push(arg);
      }
      i++;
    }

    return opts;
  };

  return Optparse;
})();

exports.Optparse = Optparse;
