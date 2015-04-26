var Kijitora = {};

Kijitora.App = (function(){

  "use strict";

  function App(appRoot){
    this._routes = {
      GET: {}
      , POST: {}
      , PUT: {}
      , DELETE: {}
      , PATCH: {}
    };
    this._appRoot = appRoot;
  };
  var __ = App.prototype;

  App.create = function(appRoot){
    return new App(appRoot);
  };

  __.addAction = function(method, path, func){
    this._routes[method][path] = func;
  };
  __.get = function(path, func){
    this.addAction("GET", path, func);
  };
  __.post = function(path, func){
    this.addAction("POST", path, func);
  };
  __.put = function(path, func){
    this.addAction("PUT", path, func);
  };
  __["delete"] = function(path, func){
    this.addAction("DELETE", path, func);
  };
  __.delete_ = __["delete"]; // alias
  __.patch = function(path, func){
    this.addAction("PATCH", path, func);
  };

  /**
   * @return params. マッチしない場合は null.
   */
  function path_match(path, pattern){

    var parts        = path.split("/");
    var partsPattern = pattern.split("/");
    var params = {};

    var len = parts.length;
    if(len < partsPattern.length){
      len = partsPattern.length;
    }

    for(var i=0; i<len; i++){
      var a = parts[i];
      var b = "" + partsPattern[i];
      if(b.match(/^:/)){
        var _key = b.substr(1);
        var key;
        if( _key.match(/^(.+)\((.+)\)$/) ){
          key = RegExp.$1;
          var re = new RegExp(RegExp.$2);
          if( ! a.match(re) ){
            return null;
          }
        }else{
          key = _key;
        }
        params[key] = a;
      }else{
        if(a != b){
          return null;
        }
      }
    }

    return params;
  }

  __.dispatch = function(method, path, req, res){
    // putskv("method", method);
    // putskv("path", path);
    var funcs = this._routes[method];

    var actionFound = false;

    for(var _path in funcs){
      var params = path_match(path, _path);
      if(params != null){
        for(var k in params){
          req.params[k] = params[k];
        }
        funcs[_path](req, res);
        actionFound = true;
        break;
      }
    }

    if( ! actionFound ){
      var filePath = this._appRoot + "/public" + path;
      if(_File.exists(filePath)){
        res.filePath = filePath;
        actionFound = true;
      }else{
        throw new Error("file not found: (" + path + ")");
      }
    }

    if( ! actionFound ){
      throw new Error("action not found");
    }
  };

  return App;
})();

exports.Kijitora = Kijitora;
